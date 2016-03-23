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
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State hiding (modify)
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
import GHCJS.DOM.MouseEvent (getDataTransfer, getClientX, getClientY)
import GHCJS.DOM.DataTransfer
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

childrenTrans
    :: (forall f . g f -> h f)
    -> (forall f . [Change g f] -> [Change h f])
    -> Children g
    -> Children h
childrenTrans trans transc ~(Children a b) = Children a' b'
  where
    a' = trans a
    b' = transc <$> b

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
viewChildrenTrans trans transc ~(ViewChildren a b c) = ViewChildren a' b' c'
  where
    a' = trans a
    b' = transc <$> b
    c' = trans <$> c

-- | Description of a user interface piece.
--
--   Inside is a kernel, in which some output type q and children must be
--   derived from some input type r and the data from those children.
--   The form of the children may also depend upon the types q and r.
--
--   Once this kernel is given, a Widget remains somewhat flexible due to the
--   other two pieces: monadic input and output arrows formed in such a way
--   that the input may depend upon the output and vice-versa.
--
--   However, sometimes we want the output to force the data available to it.
--   To make this possible, output is segmented: the passback data, always
--   made available lazily to the input, is produced alongside another
--   computation to bring about the final output. When altering the output
--   via the Profunctor interface or through effectful Modifiers, the output
--   will go outside of the recursive knot. But when the knot is tied via
--   tieKnot, it will enter the knot. It's therefore important to be clear about
--   the strictness of a Widget. If its output function is not lazy enough, then
--   the Widget must not be used in tieKnot.
--
data Widget (tag :: Symbol) s t where
    Widget
        :: ( ChildrenContainer (f r q) )
        => (passback -> s -> ElementBuilder tag (passforward, r))
        -- input may depend upon output
        -> (  (r, ViewChildren (f r q))
           -> ElementBuilder tag (q, Children (f r q))
           )
        -> (passforward -> q -> (ElementBuilder tag (passback, ElementBuilder tag t)))
        -- output may depend upon input
        -> Widget tag s t

type IntrinsicPart tag f r q =
       (r, ViewChildren (f r q))
    -> ElementBuilder tag (q, Children (f r q))

instance Functor (Widget tag s) where
    fmap = rmap

instance Profunctor (Widget tag) where
    dimap l r (Widget l' mk r') = Widget l'' mk r''
      where
        l'' passback s = l' passback (l s)
        r'' passforward q = (fmap . fmap . fmap) r (r' passforward q)

{-
-- | Like first for arrows, but since Widget is not an arrow (can't give arr,
--   and it's also not a category) we make our own.
passthrough :: Widget tag s t -> Widget tag (s, c) (t, c)
passthrough (Widget l mk r) = Widget l' mk r'
  where
    l' passback (~(s, c)) = fmap (\(~(pf, r)) -> ((pf, c), r)) (l passback s)
    r' (~(passforward, c)) q = fmap (\(~(pb, t)) -> (pb, (t, c))) (r passforward q)
-}

pullthrough :: Widget tag s t -> Widget tag s (s, t)
pullthrough (Widget l mk r) = Widget l' mk r'
  where
    l' pb s = do
        ~(pf, r) <- l pb s
        pure ((pf, s), r)
    r' (pf, s) q = do
        (pb, mkt) <- r pf q
        pure (pb, (,) s <$> mkt)

-- | Tie a recursive knot: use the output and some new input type to come up
--   with the original input type. Be sure that the function uses its first
--   argument lazily.
tieKnot
    :: forall tag s t r .
       Widget tag s t
    -> (t -> r -> ElementBuilder tag s)
    -> Widget tag r t
tieKnot (Widget l mk r) f = Widget l' mk r'
  where
    l' ~(pb, t) r = do
        s <- f t r
        l pb s
    r' pf q = do
        (pb, mkt) <- r pf q
        -- Look! We force the computation made by r. Better make sure it's
        -- lazy in all the right places.
        t <- mkt
        pure ((pb, t), pure t)

-- | A more general knot tyer: use the output and some new input type to define
--   the old input type and some new output type.
tieKnot'
    :: forall tag s t newS newT .
       Widget tag s t
    -> (t -> newS -> ElementBuilder tag (s, newT))
    -> Widget tag newS newT
tieKnot' (Widget l mk r) f = Widget l' mk r'
  where
    -- To make the input we require access to the output t, so we find it in
    -- the passback. The new output is produced, so we must pass that forward.
    --
    -- Another idea: maybe f should run in r', and pass the input back to
    -- l. With this scheme, the original l must use the input lazily.
    -- With current scheme, f must use the input lazily. Hm, yeah it's
    -- probably better as it is now. It's just a little annoying: even
    -- getSequence will cause divergence.
    l' ~(pb, t) newS = do
        ~(s, newT) <- f t newS
        ~(pf, r) <- l pb s
        pure ((pf, newT), r)
    r' (pf, newT) q = do
        (pb, mkt) <- r pf q
        -- Look again! We force the computation made by r. Better make sure it's
        -- lazy in all the right places.
        t <- mkt
        pure ((pb, t), pure newT)

widget
    :: ( ChildrenContainer (f s t) )
    => (  (s, ViewChildren (f s t))
       -> ElementBuilder tag (t, Children (f s t))
       )
    -> Widget tag s t
widget mk = Widget l mk r
  where
    l pb s = pure ((), s)
    r pf t = pure ((), pure t)

type OpenWidget s t = forall tag . Widget tag s t

-- | Some Widget whose tag is known to be a valid W3CTag, and can therefore be
--   rendered to the DOM.
--   Once closed, only its input and ouput can be altered via its profunctor
--   instance. Events, style, attributes etc. are all untouchable.
data ClosedWidget s t where
    ClosedWidget :: W3CTag tag => Tag tag -> Widget tag s t -> ClosedWidget s t

instance Functor (ClosedWidget s) where
    fmap = rmap

instance Profunctor ClosedWidget where
    dimap l r (ClosedWidget tag w) = ClosedWidget tag (dimap l r w)

closeWidget :: W3CTag tag => Tag tag -> Widget tag s t -> ClosedWidget s t
closeWidget = ClosedWidget

type UI t = ClosedWidget () t

ui :: forall tag t . W3CTag tag => Widget tag () t -> UI t
ui = closeWidget Tag

-- | Whereas the Profunctor interface allows us to use pure functiosn to juggle
--   the input and output of a Widget, a Modifier does the same but with
--   ElementBuilder effects.
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

instance Profunctor (Modifier tag) where
    dimap l r (Modifier f) = Modifier (fmap r . f . l)

instance Category (Modifier tag) where
    id = Modifier pure
    Modifier left . Modifier right = Modifier $ \s -> right s >>= left

instance Arrow (Modifier tag) where
    arr f = Modifier (pure . f)
    first (Modifier f) = Modifier $ \(s, c) -> do
        t <- f s
        pure (t, c)

instance ArrowChoice (Modifier tag) where
    left (Modifier f) = Modifier $ \choice -> case choice of
        Right c -> pure (Right c)
        Left s -> Left <$> f s

instance ArrowApply (Modifier tag) where
    app = Modifier $ \(Modifier f, s) -> f s

idModifier :: Modifier tag s s
idModifier = modifier pure

modifier :: (s -> ElementBuilder tag t) -> Modifier tag s t
modifier = Modifier

modify
    :: forall tag s t s' t' .
       Modifier tag s' s
    -> Modifier tag t t'
    -> Widget tag s t
    -> Widget tag s' t'
modify ml mr (Widget l mk r) = Widget l' mk r'
  where
    -- Modify l so that it passes its unmodified input forwards, for use by r.
    l' pb s' = do
        s <- runModifier ml s'
        ~(pf, r) <- l pb s
        pure (pf, r)
    -- Modify r so that it passes its unmodified output backwards, for use by l.
    r' pf q = do
        (pb, mkt) <- r pf q
        let t' = mkt >>= runModifier mr
        pure (pb, t')

-- | Like modifyr, but only for an effect.
--   Since it's just for the effect, we choose a right-modify (modifyr) because
--   it's less likely that the programmer will make a mistake by forcing a lazy
--   parameter (modifyl's modifier must be lazy in its first parameter).
modify_
    :: forall tag s t .
       Widget tag s t
    -> Modifier tag t ()
    -> Widget tag s t
modify_ w m = modifyr w (m *> modifier pure)

modifyl
    :: forall tag s t u .
       Widget tag s t
    -> Modifier tag u s
    -> Widget tag u t
modifyl w ml = modify ml idModifier w

modifyr
    :: forall tag s t u .
       Widget tag s t
    -> Modifier tag t u
    -> Widget tag s u
modifyr w mr = modify idModifier mr w

-- | Use the children output from a widget's intrinsic part to come up with
--   the input to that same part (a ViewChildren) along with the sequencing
--   giving DOM mutations required to fulfill that widget's specification.
--
--   Strict in both arguments.
makeChildrenInput
    :: forall f .
       ( ChildrenContainer f
       )
    => Document
    -> Children f
    -> MomentIO ( ViewChildren f
                , Sequence [ChildrenMutation SomeNode SomeNode]
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
    let mutationSequence :: Sequence [ChildrenMutation SomeNode SomeNode]
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
buildWidget (Widget l mk r) s document = mdo
    Just el <- document `createElement` Just (w3cTagName (Tag :: Tag tag))
    let roelem = ReadOnlyElement el M.empty
    -- Make a composite ElementBuilder from 3 principal parts of the widget:
    --   the input l
    --   the intrinsic part mk
    --   the output r
    --
    -- Notice that the inputter l must be lazy in passback, the intrinsic part
    -- must be lazy in childrenInput, but the outputter need not be lazy in
    -- either argument.
    let ebuilder = do rec { (passforward, s') <- l passback s
                          ; rec { (t', childrenOutput) <- mk (s', childrenInput)
                                ; (childrenInput, mutationSequence)
                                      <- liftMomentIO $ makeChildrenInput document childrenOutput
                                }
                          ;  (passback, mt) <- r passforward t'
                          }
                      -- This is crucial: the output computation runs outside
                      -- of the recursive knot above. Without this, it would
                      -- be impossible *ever* to be non-lazy in the output
                      -- function with respect to the output of the intrinsic
                      -- part.
                      t <- mt
                      pure (t, mutationSequence)
    ((t, mutationSequence), roelem', eschemaDualEndo)
        <- buildElement ebuilder roelem
    let eschema = appEndo (getDual eschemaDualEndo)
                $ emptySchema
    _ <- reactimateChildren el mutationSequence
    _ <- runElementSchema eschema document el
    liftIO (wireEvents roelem')
    pure (t, el)

buildUI :: UI t -> Document -> MomentIO (t, Element)
buildUI (ClosedWidget _ widget) = buildWidget widget ()

reactimateChildren
    :: Element
    -> Sequence [ChildrenMutation SomeNode SomeNode]
    -> MomentIO (Sequence ())
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
        => e
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
runEventBinding el (eventName, fireWhenBubbled) (EventBinding e _ eventM fire) = do
    let action = do eventM
                    domEvent <- EventM.event
                    mbubbled <- liftIO $ getJSProperty domEvent "bubbled"
                    let bubbled = case mbubbled of
                            Just "true" -> True
                            _ -> False
                    if (not bubbled) || (bubbled && fireWhenBubbled)
                    then do d <- liftIO (eventData e (Proxy :: Proxy tag) el domEvent)
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
        let binding = EventBinding event ev eventM fire
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
    eventData :: event -> Proxy tag -> Element -> DOMEvent event -> IO (EventData event)
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

data Drag = Drag
data DragData = DragData {
      dragDataClientX :: Int
    , dragDataClientY :: Int
    }
instance W3CTag tag => ElementEvent Drag tag where
    type EventData Drag = DragData
    type DOMEvent Drag = DOM.Types.MouseEvent
    eventName _ _ = Element.drag
    eventData _ _ el ev = DragData <$> getClientX ev <*> getClientY ev

data Dragstart = Dragstart Bool
data DragstartData = DragstartData {
      dragstartDataClientX :: Int
    , dragstartDataClientY :: Int
    }
instance W3CTag tag => ElementEvent Dragstart tag where
    type EventData Dragstart = DragstartData
    type DOMEvent Dragstart = DOM.Types.MouseEvent
    eventName _ _ = Element.dragStart
    eventData (Dragstart showDragGhost) _ el ev = do
        when (not showDragGhost) (disableDragGhost el ev)
        DragstartData <$> getClientX ev <*> getClientY ev
      where
        disableDragGhost el ev = do
            Just dt <- getDataTransfer ev
            Just doc <- getOwnerDocument el
            Just emptyThing <- doc `createElement` (Just ("div" :: T.Text))
            setDragImage dt (Just emptyThing) 0 0

-- | Make something draggable. The input determines whether to show a drag
--   ghost (True for yes, False for no).
makeDraggable :: W3CTag tag => Modifier tag Bool (Event (Int, Int))
makeDraggable = modifier $ \showDragGhost -> do
    attributes' (always [Set (makeAttributes [("draggable", "true")])])
    evDragstart <- event (Dragstart showDragGhost)
    evDrag <- event Drag
    let evDragstartCoords = (\d -> (dragstartDataClientX d, dragstartDataClientY d)) <$> evDragstart
    -- The coordinates of the mouse at the last dragstart.
    beDragstartCoords :: Behavior (Int, Int)
        <- stepper (0, 0) evDragstartCoords
    let evDragCoords = (\d -> (dragDataClientX d, dragDataClientY d)) <$> evDrag
    let evDelta = filterJust (makeDelta <$> beDragstartCoords <@> evDragCoords)
    pure evDelta
  where
    -- When the user releases the mouse, drag fires and gives 0,0 always.
    -- Maybe that's a chrome bug?
    -- This is our perhaps irresponsible way of eliminating that behaviour.
    makeDelta (x1, y1) (x2, y2) =
        if x2 == 0 && y2 == 0
        then Nothing
        else Just (x2 - x1, y2 - y1)

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

runAction :: Eq t => Action t -> Endo [t]
runAction action = Endo $ case action of
    Set t -> \ts -> if elem t ts then ts else t : ts
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
      elementSchemaProperties :: SequenceBuilder [Properties]
    , elementSchemaAttributes :: SequenceBuilder [Attributes]
    , elementSchemaStyle :: SequenceBuilder [Style]
    -- Sometimes we need to do some effectful work on a proper DOM element in
    -- order to get what we want. For instance, using external libraries like
    -- Leaflet (map visualizations). We throw in the document for good
    -- measure.
    , elementSchemaPostprocess :: SequenceBuilder Postprocess
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
    let props = sequenceBuilder (always [])
        attrs = sequenceBuilder (always [])
        style = sequenceBuilder (always [])
        postProcess = sequenceBuilder (always emptyPostprocess)
    in  ElementSchema props
                      attrs
                      style
                      postProcess

merge
    :: Eq t
    => Sequence [Action t]
    -> SequenceBuilder [t]
    -> SequenceBuilder [t]
merge actions seqnc =
        appEndo . getDual . mconcat . fmap (Dual . runAction)
    <$> sequenceBuilder actions
    <*> seqnc

schemaStyle
    :: Sequence [Action Style]
    -> ElementSchema
    -> ElementSchema
schemaStyle actions schema = schema {
      elementSchemaStyle = merge actions (elementSchemaStyle schema)
    }

schemaAttributes
    :: Sequence [Action Attributes]
    -> ElementSchema
    -> ElementSchema
schemaAttributes actions schema = schema {
      elementSchemaAttributes = merge actions (elementSchemaAttributes schema)
    }

schemaProperties
    :: Sequence [Action Properties]
    -> ElementSchema
    -> ElementSchema
schemaProperties actions schema = schema {
      elementSchemaProperties = merge actions (elementSchemaProperties schema)
    }

schemaPostprocess
    :: Sequence Postprocess
    -> ElementSchema
    -> ElementSchema
schemaPostprocess post schema = schema {
      elementSchemaPostprocess = mergePostprocess post (elementSchemaPostprocess schema)
    }
  where
    mergePostprocess
        :: Sequence Postprocess
        -> SequenceBuilder Postprocess
        -> SequenceBuilder Postprocess
    mergePostprocess new existing =
        (flip sequencePostprocess) <$> sequenceBuilder new
                                   <*> existing

runElementSchema :: ElementSchema -> Document -> Element -> MomentIO ()
runElementSchema eschema document el = do
    -- Reactimating these sequences probably causes a space leak. As far as I
    -- know, reactimating an event keeps that event alive... seems that's how
    -- it has to be.
    -- In typical use, these events will often be never (in case of constant
    -- style, attributes, properties). But every non-never event which is
    -- used to define these sequences will never be collected! We need some way
    -- to "unreactimate" when the element is collected.
    propsSequence <- buildSequence (elementSchemaProperties eschema)
    attrsSequence <- buildSequence (elementSchemaAttributes eschema)
    styleSequence <- buildSequence (elementSchemaStyle eschema)
    postsSequence <- buildSequence (elementSchemaPostprocess eschema)
    reactimateProperties el propsSequence
    reactimateAttributes el attrsSequence
    reactimateStyle el styleSequence
    reactimatePostprocess document el postsSequence
    pure ()

  where

    reactimatePostprocess :: Document -> Element -> Sequence Postprocess -> MomentIO ()
    reactimatePostprocess document element sequence = do
        sequenceReactimate ((\x -> runPostprocess x document element) <$> sequence)

    reactimateProperties :: Element -> Sequence [Properties] -> MomentIO ()
    reactimateProperties element sequence = do
        currentProperties <- liftIO $ newIORef mempty
        -- For properties we don't diff, as these are sometimes changed by
        -- user input.
        let changeProperties :: [Properties] -> IO ()
            changeProperties new = do
                let props :: M.Map T.Text T.Text
                    props = computeProperties new
                current <- readIORef currentProperties
                --let (add, remove) = diffProperties current props
                removeProperties element current
                addProperties element props
                --putStrLn $ mconcat ["Properties input ", show new]
                --putStrLn $ mconcat ["Removing properties ", show remove]
                --putStrLn $ mconcat ["Adding properties ", show add]
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


    reactimateAttributes :: Element -> Sequence [Attributes] -> MomentIO ()
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


    reactimateStyle :: Element -> Sequence [Style] -> MomentIO ()
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
    => Sequence (Action Style)
    -> ElementBuilder tag ()
style s = ElementBuilder $ lift (tell (Dual (Endo (schemaStyle (fmap pure s)))))

style'
    :: W3CTag tag
    => Sequence [Action Style]
    -> ElementBuilder tag ()
style' s = ElementBuilder $ lift (tell (Dual (Endo (schemaStyle s))))

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
    => Sequence (Action Attributes)
    -> ElementBuilder tag ()
attributes a = ElementBuilder $ lift (tell (Dual (Endo (schemaAttributes (fmap pure a)))))

attributes'
    :: W3CTag tag
    => Sequence [Action Attributes]
    -> ElementBuilder tag ()
attributes' a = ElementBuilder $ lift (tell (Dual (Endo (schemaAttributes a))))

properties
    :: W3CTag tag
    => Sequence (Action Properties)
    -> ElementBuilder tag ()
properties p = ElementBuilder $ lift (tell (Dual (Endo (schemaProperties (fmap pure p)))))

properties'
    :: W3CTag tag
    => Sequence [Action Properties]
    -> ElementBuilder tag ()
properties' p = ElementBuilder $ lift (tell (Dual (Endo (schemaProperties p))))

postprocess
    :: W3CTag tag
    => Sequence Postprocess
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
