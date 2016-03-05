{-|
Module      : Reactive.DOM.Internal.Node
Description : Definition of reactive DOM elements/nodes.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Reactive.DOM.Internal.Node where

import Prelude hiding ((.), id, span)
import GHC.TypeLits (Symbol)
import Control.Category
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Fix
import Data.Proxy
import Data.Void
import Data.Functor.Compose
import Data.Profunctor
import Data.Bifunctor (bimap)
import Data.List (delete)
import Data.Semigroup (Semigroup(..), Endo(..), Dual(..))
import Data.Unique
import qualified Data.Map as M
import Data.IORef
import qualified Data.Text as T
import Data.JSString.Text
import GHCJS.Types
import GHCJS.Marshal.Pure (PToJSVal, pToJSVal)
import GHCJS.DOM.Types hiding (Event, Element, Document)
import qualified GHCJS.DOM.Types as DOM.Types
import GHCJS.DOM.Element hiding (Element)
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.Node as Node
import GHCJS.DOM.Document hiding (Document)
import GHCJS.DOM.EventM hiding (event)
import qualified GHCJS.DOM.EventM as EventM
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.HTMLInputElement (getValue)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Internal.Tag
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
import System.IO.Unsafe
import Unsafe.Coerce

-- | A monad in which DOM elements are described. It's parameterized by a
--   Symbol which presumably is one of the W3C standard tag names.
newtype ElementBuilder (tag :: Symbol) t = ElementBuilder {
      runElementBuilder
          :: StateT (ReadOnlyElement tag)
             (WriterT (Dual (Endo ElementSchema))
             MomentIO)
             t
    }

deriving instance Functor (ElementBuilder tag)
deriving instance Applicative (ElementBuilder tag)
deriving instance Monad (ElementBuilder tag)
deriving instance MonadFix (ElementBuilder tag)
instance MonadMoment (ElementBuilder tag) where
    liftMoment = ElementBuilder . lift . lift . liftMoment

-- | Run an ElementBuilder.
buildElement
    :: forall tag t .
       ( )
    => ElementBuilder tag t
    -> ReadOnlyElement tag
    -> MomentIO (t, ReadOnlyElement tag, Dual (Endo ElementSchema))
buildElement extrinsic el = do
    ((t, el'), eschemaDualEndo)
        <- runWriterT (runStateT (runElementBuilder extrinsic) el)
    pure (t, el', eschemaDualEndo)

liftMomentIO :: MomentIO t -> ElementBuilder tag t
liftMomentIO = ElementBuilder . lift . lift

newtype Child t = Child {
      runChild :: (t, SomeNode)
    }

childData :: Child t -> t
childData = fst . runChild

childNode :: forall t . Child t -> SomeNode
childNode = snd . runChild

data SetChild t where
    SetWidgetChild :: Either (Child t) (UI t) -> SetChild t
    SetTextChild :: T.Text -> SetChild ()

newChild :: UI t -> SetChild t
newChild = SetWidgetChild . Right

existingChild :: Child t -> SetChild t
existingChild = SetWidgetChild . Left

textChild :: T.Text -> SetChild ()
textChild = SetTextChild

runSetChild :: SetChild r -> Document -> Compose MomentIO Child r
runSetChild child document = Compose $ case child of
    SetTextChild txt -> do
        textNode <- makeText document txt
        node <- liftIO $ someText textNode
        pure (Child ((), node))
    SetWidgetChild (Left existing) ->
        pure (Child (runChild existing))
    SetWidgetChild (Right ui) -> do
        (t, el) <- buildUI ui document
        node <- liftIO $ someElement el
        pure (Child (t, node))

data Children f = Children {
      childrenInitial :: f SetChild
    , childrenChanges :: Event [Change f SetChild]
    }

constantChildren :: f SetChild -> Children f
constantChildren = flip Children never

children :: f SetChild -> Event [Change f SetChild] -> Children f
children = Children

data ViewChildren f = ViewChildren {
      viewChildrenInitial :: f Child
    , viewChildrenChanges :: Event [Change f Child]
    , viewChildrenEvent :: Event (f Child)
    }

viewChildrenBehavior :: ViewChildren f -> ElementBuilder tag (Behavior (f Child))
viewChildrenBehavior viewChildren = ElementBuilder $
    (lift . lift) (stepper (viewChildrenInitial viewChildren) (viewChildrenEvent viewChildren))

viewChildrenTrans
    :: (forall f . g f -> h f)
    -> (forall f . [Change g f] -> [Change h f])
    -> ViewChildren g
    -> ViewChildren h
viewChildrenTrans trans transc (ViewChildren a b c) = ViewChildren a' b' c'
  where
    a' = trans a
    b' = transc <$> b
    c' = trans <$> c

data Widget (tag :: Symbol) s t where
    Widget
        :: ( ChildrenContainer f )
        => (  (s, ViewChildren f)
           -> ElementBuilder tag (t, Children f)
           )
        -> Widget tag s t

instance Functor (Widget tag s) where
    fmap f (Widget mk) = Widget $ \(s, c) -> do
        (t, c') <- mk (s, c)
        pure (f t, c')

instance Profunctor (Widget tag) where
    dimap l r (Widget mk) = Widget $ \(s, c) -> do
        (t, c') <- mk (l s, c)
        pure (r t, c')

-- | Like first for arrows, but since Widget is not an arrow (can't give arr)
--   we make our own.
passthrough :: Widget tag s t -> Widget tag (s, c) (t, c)
passthrough (Widget mk) = Widget $ \(~((s, c), viewChildren)) -> do
    (t, setChildren) <- mk (s, viewChildren)
    pure ((t, c), setChildren)

-- | Like dimap, but we allow you to MomentIO.
dimap'
    :: (q -> ElementBuilder tag s)
    -> (t -> ElementBuilder tag u)
    -> Widget tag s t
    -> Widget tag q u
dimap' l r (Widget mk) = Widget $ \(q, c) -> do
    s <- l q
    (t, c') <- mk (s, c)
    u <- r t
    pure (u, c')

lmap' :: (q -> ElementBuilder tag s) -> Widget tag s t -> Widget tag q t
lmap' l = dimap' l pure

rmap' :: (t -> ElementBuilder tag u) -> Widget tag s t -> Widget tag s u
rmap' r = dimap' pure r

widget
    :: ( ChildrenContainer f )
    => (  (s, ViewChildren f)
       -> ElementBuilder tag (t, Children f)
       )
    -> Widget tag s t
widget = Widget

type OpenWidget s t = forall tag . Widget tag s t

closeWidget :: Tag tag -> OpenWidget s t -> Widget tag s t
closeWidget _ w = w

tag :: Tag tag -> OpenWidget s t -> Widget tag s t
tag = closeWidget

data UI t where
    UI :: W3CTag tag => Tag tag -> Widget tag () t -> UI t

instance Functor UI where
    fmap f (UI proxy w) = UI proxy (fmap f w)

ui :: forall tag t . W3CTag tag => Widget tag () t -> UI t
ui = UI (Tag :: Tag tag)

knot
    :: Widget tag t t
    -> Widget tag () t
knot (Widget mk) = Widget $ \(_, viewChildren) -> mdo
    (t, setChildren) <- mk (t, viewChildren)
    pure (t, setChildren)

tie
    :: Widget tag s t
    -> (t -> ElementBuilder tag s)
    -> Widget tag () t
tie (Widget mk) f = Widget $ \(_, viewChildren) -> mdo
    (t, setChildren) <- mk (s, viewChildren)
    s <- f t
    pure (t, setChildren)

-- | A modifier uses the output, model, and children of some Widget to
--   alter the output. It cannot change the model nor the children, just the
--   output value and type.
newtype Modifier tag s t = Modifier {
      runModifier :: s -> ElementBuilder tag t
    }

instance Functor (Modifier tag s) where
    fmap f = Modifier . (fmap . fmap) f . runModifier

instance Applicative (Modifier tag s) where
    pure = Modifier . pure . pure
    (Modifier mf) <*> (Modifier mx) = Modifier $ \s ->
        ($) <$> mf s <*> mx s

instance Monad (Modifier tag s) where
    return = pure
    (Modifier mx) >>= k = Modifier $ \s -> do
        y <- mx s
        runModifier (k y) s

modifier :: (s -> ElementBuilder tag t) -> Modifier tag s t
modifier = Modifier

modify
    :: forall tag s t u .
       Widget tag s t
    -> Modifier tag t u
    -> Widget tag s u
modify (Widget mk) modifier = Widget mk'
  where
    mk' (s, viewChildren) = do
        (t, setChildren) <- mk (s, viewChildren)
        u <- runModifier modifier t
        pure (u, setChildren)

makeChildrenInput
    :: forall f r .
       ( ChildrenContainer f
       )
    => Document
    -> Children f
    -> MomentIO ( ViewChildren f
                , Sequence MomentIO [ChildrenMutation SomeNode SomeNode]
                )
makeChildrenInput document childrenOutput = mdo

    firstChildren :: f Child
        <- functorCommute . functorTrans (flip runSetChild document) $ childrenInitial childrenOutput


    -- childrenChanges childrenOutput :: Event [Change f SetChild]
    -- flip runSetChild document :: SetChild t -> Compose MomentIO SetChild t
    -- functorTrans (flip runSetChild document)
    --     :: Change f SetChild -> Change f (Compose MomentIO SetChild)
    -- functorCommute . (functorTrans (flip runSetChild document))
    --     :: Change f SetChild -> MomentIO (Change f Child)
    -- traverse (functorCommute . (functorTrans (flip runSetChild document)))
    --     :: [Change f SetChild -> MomentIO [Change f Child]
    sequencedChanges :: Event [Change f Child]
        <- execute (traverse (functorCommute . (functorTrans (flip runSetChild document))) <$> childrenChanges childrenOutput)

    let restChildren :: Event (f Child, [ChildrenMutation SomeNode SomeNode])
        restChildren =
                runChanges (getChange childNode)
            <$> beChildren
            <@> sequencedChanges

    beChildren :: Behavior (f Child)
        <- stepper firstChildren (fst <$> restChildren)

    let firstMutation :: [ChildrenMutation SomeNode SomeNode]
        firstMutation = AppendChild <$> childrenContainerList childNode firstChildren
    let restMutations :: Event [ChildrenMutation SomeNode SomeNode]
        restMutations = snd <$> restChildren
    let mutationSequence :: Sequence MomentIO [ChildrenMutation SomeNode SomeNode]
        mutationSequence = firstMutation |> restMutations

    -- This give some indication of which DOM mutations are happening.
    -- Comment it out.
    reactimate (Prelude.print <$> restMutations)

    pure ( ViewChildren firstChildren sequencedChanges (fst <$> restChildren)
         , mutationSequence
         )

buildWidget
    :: forall tag s t .
       ( W3CTag tag )
    => Widget tag s t
    -> s
    -> Document
    -> MomentIO (t, Element)
buildWidget (Widget mk) s document = mdo
    Just el <- document `createElement` Just (w3cTagName (Tag :: Tag tag))
    let ebuilder = mk (s, childrenInput)
    let roelem = ReadOnlyElement el M.empty
    ((t, childrenOutput), roelem', eschemaDualEndo)
        <- buildElement ebuilder roelem
    (childrenInput, mutationSequence) <- makeChildrenInput document childrenOutput
    let eschema = appEndo (getDual eschemaDualEndo) $ emptySchema
    _ <- reactimateChildren el mutationSequence
    _ <- runElementSchema eschema document el
    liftIO (wireEvents roelem')
    pure (t, el)

buildUI :: UI t -> Document -> MomentIO (t, Element)
buildUI (UI _ widget) = buildWidget widget ()

reactimateChildren
    :: Element
    -> Sequence MomentIO [ChildrenMutation SomeNode SomeNode]
    -> MomentIO (Sequence MomentIO ())
reactimateChildren parent seqnc = do
    sequenceCommute (liftIO . flip runChildrenMutationsIO parent <$> seqnc)

render
    :: ( IsNode parent
       )
    => Document
    -> parent
    -> UI t
    -> MomentIO (RenderedNode, t)
render document parent ui = do
    (t, el) <- buildUI ui document
    rendered <- renderElement parent el
    pure (rendered, t)

-- | For use in ElementBuilder state. Gives access to certain features of
--   a DOM element and holds deferred event bindings.
data ReadOnlyElement (tag :: Symbol) = ReadOnlyElement {
      getReadOnlyElement :: Element
    , getEvents :: M.Map (DOMString, Bool) (EventBinding tag)
    }

data EventBinding (tag :: Symbol) where
    EventBinding
        :: forall e tag .
           ( ElementEvent e tag )
        => Proxy e
        -> Event (EventData e)
        -> EventM Element (DOMEvent e) ()
        -> (EventData e -> IO ())
        -> EventBinding tag

runEventBinding
    :: forall tag .
       Element
    -> (DOMString, Bool)
    -> EventBinding tag
    -> IO ()
runEventBinding el (eventName, fireWhenBubbled) (EventBinding (Proxy :: Proxy e) _ eventM fire) = do
    let action :: EventM Element (DOMEvent e) ()
        action = do eventM
                    domEvent :: DOMEvent e <- EventM.event
                    mbubbled <- liftIO $ getJSProperty domEvent "bubbled"
                    let bubbled = case mbubbled of
                            Just "true" -> True
                            _ -> False
                    if (not bubbled) || (bubbled && fireWhenBubbled)
                    then do d <- liftIO (eventData (Proxy :: Proxy e) (Proxy :: Proxy tag) el domEvent)
                            liftIO $ setJSProperty domEvent "bubbled" (Just "true")
                            liftIO $ fire d
                            pure ()
                    else pure ()
    on el (EventName eventName) action
    pure ()

wireEvents :: ReadOnlyElement tag -> IO ()
wireEvents (ReadOnlyElement el eventMap) =
    M.foldWithKey (\k v rest -> runEventBinding el k v >> rest) (pure ()) eventMap

elementEvent
    :: forall event tag .
       ElementEvent event tag
    => event
    -> ReadOnlyElement tag
    -> Bool -- True if it should fire even when bubbled.
    -> MomentIO (Event (EventData event), ReadOnlyElement tag)
elementEvent event roelement fireWhenBubbled = case existingBinding of
    -- unsafeCoerce is OK. We know ev must have the right type, because the
    -- only way it could have come to be here is if it was inserted for
    -- the same key, and the key is determined by the type @event@.
    Just (EventBinding _ ev _ _) -> pure (unsafeCoerce ev, roelement)
    Nothing -> do
        (ev, fire) <- newEvent
        let binding = EventBinding (Proxy :: Proxy event) ev eventM fire
        let newEvents = M.alter (const (Just binding)) (key, fireWhenBubbled) (getEvents roelement)
        pure (ev, roelement { getEvents = newEvents })
  where
    existingBinding = M.lookup (key, fireWhenBubbled) (getEvents roelement)
    EventName key = eventName (Proxy :: Proxy event) (Proxy :: Proxy tag)
    eventM :: EventM Element (DOMEvent event) ()
    eventM = specialHandler (Proxy :: Proxy event) (Proxy :: Proxy tag)

-- | Since we don't want to allow `execute` in ElementBuilder, we offer a
--   specialized way of using effectful events. Only IOEvents with benign
--   effects can be obtained.
newtype IOEvent t = IOEvent {
      runIOEvent :: IO t
    }

deriving instance Functor IOEvent
deriving instance Applicative IOEvent
deriving instance Monad IOEvent

ioEvent :: IOEvent (s -> t) -> Event s -> ElementBuilder tag (Event t)
ioEvent ioevent ev = ElementBuilder $ do
    let io = runIOEvent ioevent
    lift . lift $ (execute (liftIO . (<*>) io . pure <$> ev))

clientRect :: ElementBuilder tag (IOEvent ClientRect)
clientRect = ElementBuilder $ do
    roelem <- get
    let el = getReadOnlyElement roelem
    let getIt = do Just rect <- getBoundingClientRect el
                   pure rect
    pure (IOEvent getIt)

clientHeight :: ElementBuilder tag (IOEvent Double)
clientHeight = ElementBuilder $ do
    el <- get
    pure (IOEvent (getClientHeight (getReadOnlyElement el)))

clientWidth :: ElementBuilder tag (IOEvent Double)
clientWidth = ElementBuilder $ do
    el <- get
    pure (IOEvent (getClientWidth (getReadOnlyElement el)))

offsetHeight :: ElementBuilder tag (IOEvent Double)
offsetHeight = ElementBuilder $ do
    el <- get
    pure (IOEvent (getOffsetHeight (getReadOnlyElement el)))

offsetWidth :: ElementBuilder tag (IOEvent Double)
offsetWidth = ElementBuilder $ do
    el <- get
    pure (IOEvent (getOffsetWidth (getReadOnlyElement el)))

scrollHeight :: ElementBuilder tag (IOEvent Int)
scrollHeight = ElementBuilder $ do
    el <- get
    pure (IOEvent (getScrollHeight (getReadOnlyElement el)))

scrollWidth :: ElementBuilder tag (IOEvent Int)
scrollWidth = ElementBuilder $ do
    el <- get
    pure (IOEvent (getScrollWidth (getReadOnlyElement el)))

-- TODO HasEvent tag event
event
    :: forall event tag . 
       ElementEvent event tag
    => event
    -> ElementBuilder tag (Event (EventData event))
event ev = ElementBuilder $ do
    roelem <- get
    (ev, roelem') <- (lift . lift) (elementEvent ev roelem False)
    put roelem'
    pure ev

class
    ( PToJSVal (DOMEvent event)
    , IsEvent (DOMEvent event)
    , W3CTag tag
    ) => ElementEvent event (tag :: Symbol)
  where
    type EventData event :: *
    type DOMEvent event :: *
    eventName :: Proxy event -> Proxy tag -> EventName Element (DOMEvent event)
    eventData :: Proxy event -> Proxy tag -> Element -> DOMEvent event -> IO (EventData event)
    -- In case you need to do special effects in the event handler.
    -- Motivating case: the Submit event *always* prevents the default action.
    -- Without this, the page will reload.
    specialHandler :: Proxy event -> Proxy tag -> EventM Element (DOMEvent event) ()
    specialHandler _ _ = pure ()

data Click = Click
instance W3CTag tag => ElementEvent Click tag where
    type EventData Click = ()
    type DOMEvent Click = MouseEvent
    eventName _ _ = Element.click
    eventData _ _ _ _ = pure ()

data Mouseenter = Mouseenter
instance W3CTag tag => ElementEvent Mouseenter tag where
    type EventData Mouseenter = ()
    type DOMEvent Mouseenter = MouseEvent
    eventName _ _ = Element.mouseEnter
    eventData _ _ _ _ = pure ()

data Mouseleave = Mouseleave
instance W3CTag tag => ElementEvent Mouseleave tag where
    type EventData Mouseleave = ()
    type DOMEvent Mouseleave = MouseEvent
    eventName _ _ = Element.mouseLeave
    eventData _ _ _ _ = pure ()

data Submit = Submit
instance ElementEvent Submit "form" where
    type EventData Submit = ()
    type DOMEvent Submit = DOM.Types.Event
    eventName _ _ = Element.submit
    eventData _ _ _ _ = pure ()
    specialHandler _ _ = preventDefault

data Input = Input
instance ElementEvent Input "input" where
    type EventData Input = T.Text
    type DOMEvent Input = DOM.Types.Event
    eventName _ _ = Element.input
    eventData _ _ el _ = maybe "" id <$> getValue (castToHTMLInputElement el)

data Scroll = Scroll
data ScrollData = ScrollData {
      scrollDataTop :: Int
    , scrollDataLeft :: Int
    }
instance W3CTag tag => ElementEvent Scroll tag where
    type EventData Scroll = ScrollData
    type DOMEvent Scroll = DOM.Types.UIEvent
    eventName _ _ = Element.scroll
    eventData _ _ el _ = ScrollData <$> getScrollTop el <*> getScrollLeft el

type Document = DOM.Types.Document
type Element = DOM.Types.Element

newtype IdentifiedMap k v = IdentifiedMap {
      runIdentifiedMap :: (M.Map k v, Unique)
    }

instance (Show k, Show v) => Show (IdentifiedMap k v) where
    show im = let (m, u) = runIdentifiedMap im
              in  "IdentifiedMap " ++ show (hashUnique u) ++ " : " ++ show m

{-# NOINLINE makeIdentifiedMap #-}
makeIdentifiedMap :: M.Map k v -> IdentifiedMap k v
makeIdentifiedMap m = unsafePerformIO $ do
    u <- newUnique
    pure $ IdentifiedMap (m, u)

instance Eq (IdentifiedMap k v) where
    l == r = snd (runIdentifiedMap l) == snd (runIdentifiedMap r)

instance Ord (IdentifiedMap k v) where
    l `compare` r = snd (runIdentifiedMap l) `compare` snd (runIdentifiedMap r)

data Action t = Set t | Unset t | NoOp

runAction :: Eq t => Action t -> [t] -> [t]
runAction action = case action of
    Set t -> (:) t
    Unset t -> delete t
    NoOp -> id

type Properties = IdentifiedMap T.Text T.Text
type Style = IdentifiedMap T.Text T.Text
type Attributes = IdentifiedMap T.Text T.Text

{-# NOINLINE makeStyle #-}
makeStyle :: [(T.Text, T.Text)] -> Style
makeStyle = makeIdentifiedMap . M.fromList

makeProperties :: [(T.Text, T.Text)] -> Properties
makeProperties = makeIdentifiedMap . M.fromList

makeAttributes :: [(T.Text, T.Text)] -> Attributes
makeAttributes = makeIdentifiedMap . M.fromList

computeStyle :: [Style] -> M.Map T.Text T.Text
computeStyle = foldl (<>) M.empty . fmap (fst . runIdentifiedMap)

computeProperties :: [Properties] -> M.Map T.Text T.Text
computeProperties = computeStyle

computeAttributes :: [Attributes] -> M.Map T.Text T.Text
computeAttributes = computeStyle

type ElementSchemaChild = Either Element Text

-- | Description of a DOM element.
data ElementSchema = ElementSchema {
      elementSchemaProperties :: Sequence MomentIO [Properties]
    , elementSchemaAttributes :: Sequence MomentIO [Attributes]
    , elementSchemaStyle :: Sequence MomentIO [Style]
    -- Sometimes we need to do some effectful work on a proper DOM element in
    -- order to get what we want. For instance, using external libraries like
    -- Leaflet (map visualizations). We throw in the document for good
    -- measure.
    , elementSchemaPostprocess :: Sequence MomentIO Postprocess
    }

newtype Postprocess = Postprocess {
      runPostprocess :: Document -> Element -> IO ()
    }

emptyPostprocess :: Postprocess
emptyPostprocess = Postprocess (const (const (pure ())))

sequencePostprocess
    :: Postprocess
    -> Postprocess
    -> Postprocess
sequencePostprocess (Postprocess f) (Postprocess g) = Postprocess $ \doc el ->
    f doc el >> g doc el

-- | An empty ElementSchema: no attributes, properties, style, children, etc.
--   "div" is the chosen tag.
--   The choice of NoChildren guarantees that there are no children.
emptySchema :: ElementSchema
emptySchema =
    let props = always []
        attrs = always []
        style = always []
        postProcess = always emptyPostprocess
    in  ElementSchema props
                      attrs
                      style
                      postProcess

schemaStyle
    :: Sequence MomentIO (Action Style)
    -> ElementSchema
    -> ElementSchema
schemaStyle actions schema = schema {
      elementSchemaStyle = mergeStyle actions (elementSchemaStyle schema)
    }
  where
    mergeStyle
        :: Sequence MomentIO (Action Style)
        -> Sequence MomentIO [Style]
        -> Sequence MomentIO [Style]
    mergeStyle actions seqnc = runAction <$> actions <*> seqnc

schemaAttributes
    :: Sequence MomentIO (Action Attributes)
    -> ElementSchema
    -> ElementSchema
schemaAttributes actions schema = schema {
      elementSchemaAttributes = mergeAttributes actions (elementSchemaAttributes schema)
    }
  where
    mergeAttributes
        :: Sequence MomentIO (Action Attributes)
        -> Sequence MomentIO [Attributes]
        -> Sequence MomentIO [Attributes]
    mergeAttributes actions seqnc = runAction <$> actions <*> seqnc

schemaProperties
    :: Sequence MomentIO (Action Properties)
    -> ElementSchema
    -> ElementSchema
schemaProperties actions schema = schema {
      elementSchemaProperties = mergeProperties actions (elementSchemaProperties schema)
    }
  where
    mergeProperties
        :: Sequence MomentIO (Action Properties)
        -> Sequence MomentIO [Properties]
        -> Sequence MomentIO [Properties]
    mergeProperties actions seqnc = runAction <$> actions <*> seqnc

schemaPostprocess
    :: Sequence MomentIO Postprocess
    -> ElementSchema
    -> ElementSchema
schemaPostprocess post schema = schema {
      elementSchemaPostprocess = mergePostprocess post (elementSchemaPostprocess schema)
    }
  where
    mergePostprocess
        :: Sequence MomentIO Postprocess
        -> Sequence MomentIO Postprocess
        -> Sequence MomentIO Postprocess
    mergePostprocess new existing = (flip sequencePostprocess) <$> new <*> existing

runElementSchema :: ElementSchema -> Document -> Element -> MomentIO ()
runElementSchema eschema document el = do
    -- Reactimating these sequences probably causes a space leak. As far as I
    -- know, reactimating an event keeps that event alive... seems that's how
    -- it has to be.
    -- In typical use, these events will often be never (in case of constant
    -- style, attributes, properties). But every non-never event which is
    -- used to define these sequences will never be collected! We need some way
    -- to "unreactimate" when the element is collected.
    reactimateProperties el (elementSchemaProperties eschema)
    reactimateAttributes el (elementSchemaAttributes eschema)
    reactimateStyle el (elementSchemaStyle eschema)
    reactimatePostprocess document el (elementSchemaPostprocess eschema)
    pure ()

  where

    reactimatePostprocess :: Document -> Element -> Sequence MomentIO Postprocess -> MomentIO ()
    reactimatePostprocess document element sequence = do
        sequenceReactimate ((\x -> runPostprocess x document element) <$> sequence)

    reactimateProperties :: Element -> Sequence MomentIO [Properties] -> MomentIO ()
    reactimateProperties element sequence = do
        currentProperties <- liftIO $ newIORef mempty
        let changeProperties :: [Properties] -> IO ()
            changeProperties new = do
                let props :: M.Map T.Text T.Text
                    props = computeProperties new
                current <- readIORef currentProperties
                let (add, remove) = diffProperties current props
                removeProperties element remove
                addProperties element add
                writeIORef currentProperties props
        sequenceReactimate (changeProperties <$> sequence)
        return ()

    addProperties :: Element -> M.Map T.Text T.Text -> IO ()
    addProperties element properties = do
        let propertiesList :: [(T.Text , Maybe T.Text)]
            propertiesList = M.foldWithKey (\x y -> (:) (x, Just y)) [] properties
        forM_ propertiesList (\(x, y) -> setJSProperty element (textToJSString x) (textToJSString <$> y))

    removeProperties :: Element -> M.Map T.Text T.Text -> IO ()
    removeProperties element properties = do
        let propertyNames :: [T.Text]
            propertyNames = M.keys properties
        _ :: [Maybe JSString] <- forM propertyNames (removeJSProperty element . textToJSString)
        return ()

    diffProperties old new = (toAdd, toRemove)
      where
        toAdd = M.differenceWith justWhenDifferent new old
        toRemove = M.differenceWith justWhenDifferent old new
        justWhenDifferent x y = if x /= y then Just x else Nothing


    reactimateAttributes :: Element -> Sequence MomentIO [Attributes] -> MomentIO ()
    reactimateAttributes element sequence = do
        currentAttributes <- liftIO $ newIORef mempty
        let changeAttributes new = do
                let attrs = computeAttributes new
                current <- readIORef currentAttributes
                let (add, remove) = diffAttributes current attrs
                removeAttributes element remove
                addAttributes element add
                writeIORef currentAttributes attrs
        sequenceReactimate (changeAttributes <$> sequence)
        return ()

    addAttributes :: Element -> M.Map T.Text T.Text -> IO ()
    addAttributes el attrs = do
        let attrList :: [(T.Text, T.Text)]
            attrList = M.foldWithKey (\x y -> (:) (x, y)) [] attrs
        forM_ attrList (\(x, y) -> setAttribute el (textToJSString x) (textToJSString y))

    removeAttributes :: Element -> M.Map T.Text T.Text -> IO ()
    removeAttributes el attrs = do
        let attrNames :: [T.Text]
            attrNames = M.keys attrs
        forM_ attrNames (removeAttribute el . textToJSString)

    diffAttributes old new = (toAdd, toRemove)
      where
        toAdd = M.differenceWith justWhenDifferent new old
        toRemove = M.differenceWith justWhenDifferent old new
        justWhenDifferent x y = if x /= y then Just x else Nothing


    reactimateStyle :: Element -> Sequence MomentIO [Style] -> MomentIO ()
    reactimateStyle element sequence = do
        currentStyle <- liftIO $ newIORef mempty
        let changeStyle new = do
                let styl = computeStyle new
                current <- readIORef currentStyle
                let (add, remove) = diffStyle current styl
                removeStyle element remove
                addStyle element add
                writeIORef currentStyle styl
        sequenceReactimate (changeStyle <$> sequence)
        return ()

    addStyle :: Element -> M.Map T.Text T.Text -> IO ()
    addStyle element style = do
        let styleList :: [(T.Text, Maybe T.Text, T.Text)]
            styleList = M.foldWithKey (\x y -> (:) (x, Just y, "")) [] style
        Just css <- getStyle element
        forM_ styleList (\(x, y, z) -> setProperty css (textToJSString x) (textToJSString <$> y) (textToJSString z))

    removeStyle :: Element -> M.Map T.Text T.Text -> IO ()
    removeStyle element style = do
        let styleNames :: [T.Text]
            styleNames = M.keys style
        Just css <- getStyle element
        -- We bind here because we have to give a type signature in order to
        -- disambiguate.
        _ :: [Maybe JSString] <- forM styleNames (removeProperty css . textToJSString)
        return ()

    -- | First component is the new style not present in old.
    --   Second is the rules in old not present in new (to be removed).
    diffStyle oldStyle newStyle = (toAdd, toRemove)
      where
        toAdd = M.differenceWith justWhenDifferent newStyle oldStyle
        toRemove = M.differenceWith justWhenDifferent oldStyle newStyle
        justWhenDifferent x y = if x /= y then Just x else Nothing

foreign import javascript unsafe "$1[$2]=$3;" js_setJSProperty
    :: JSVal -> JSString -> Nullable JSString -> IO ()

foreign import javascript unsafe "$1[$2]" js_getJSProperty
    :: JSVal -> JSString -> IO (Nullable JSString)

foreign import javascript unsafe "delete $1[$2];" js_removeJSProperty
    :: JSVal -> JSString -> IO (Nullable JSString)

setJSProperty :: PToJSVal thing => thing -> JSString -> Maybe JSString -> IO ()
setJSProperty thing x y = js_setJSProperty (pToJSVal thing) x (maybeToNullable y)

getJSProperty :: PToJSVal thing => thing -> JSString -> IO (Maybe JSString)
getJSProperty thing key = nullableToMaybe <$> js_getJSProperty (pToJSVal thing) key

removeJSProperty :: PToJSVal thing => thing -> JSString -> IO (Maybe JSString)
removeJSProperty thing x = nullableToMaybe <$> js_removeJSProperty (pToJSVal thing) x

style
    :: W3CTag tag
    => Sequence MomentIO (Action Style)
    -> ElementBuilder tag ()
style s = ElementBuilder $ lift (tell (Dual (Endo (schemaStyle s))))

styleHover
    :: ( ElementEvent Mouseenter tag
       , ElementEvent Mouseleave tag
       )
    => Style
    -> ElementBuilder tag ()
styleHover s = do
    enterev <- event Mouseenter
    leaveev <- event Mouseleave
    let set = const (Set s) <$> enterev
    let unset = const (Unset s) <$> leaveev
    style (NoOp |> unionWith const set unset)
    pure ()

attributes
    :: W3CTag tag
    => Sequence MomentIO (Action Attributes)
    -> ElementBuilder tag ()
attributes a = ElementBuilder $ lift (tell (Dual (Endo (schemaAttributes a))))

properties
    :: W3CTag tag
    => Sequence MomentIO (Action Properties)
    -> ElementBuilder tag ()
properties p = ElementBuilder $ lift (tell (Dual (Endo (schemaProperties p))))

postprocess
    :: W3CTag tag
    => Sequence MomentIO Postprocess
    -> ElementBuilder tag ()
postprocess p = ElementBuilder $ lift (tell (Dual (Endo (schemaPostprocess p))))

makeText :: Document -> T.Text -> MomentIO Text
makeText document txt = do
    let txtJSString :: JSString = textToJSString txt
    Just txt <- document `createTextNode` txtJSString
    pure txt

data RenderedNode = RenderedNode {
      renderedElementSchemaChild :: Either Element Text
    , unrenderNode :: IO ()
    }

renderElement
    :: ( IsNode parent
       , MonadIO m
       )
    => parent
    -> Element
    -> m RenderedNode
renderElement parent el = do
    parent `appendChild` (Just el)
    let unrender = parent `removeChild` (Just el) >> pure ()
    pure $ RenderedNode (Left el) unrender

-- | Append some Text to a given node.
renderText
    :: ( IsNode parent
       , MonadIO m
       )
    => parent
    -> Text
    -> m RenderedNode
renderText parent txt = do
    parent `appendChild` (Just txt)
    let unrender = parent `removeChild` (Just txt) >> pure ()
    pure $ RenderedNode (Right txt) unrender

-- | Render a text node or element.
renderChild
    :: ( IsNode parent
       , MonadIO m
       )
    => parent
    -> Either Element Text
    -> m RenderedNode
renderChild parent =
    either (renderElement parent)
           (renderText parent)

unrender :: RenderedNode -> MomentIO ()
unrender = liftIO . unrenderNode
