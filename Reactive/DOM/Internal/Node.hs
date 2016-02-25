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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

module Reactive.DOM.Internal.Node where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.List (delete)
import Data.Semigroup (Semigroup(..))
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Profunctor
import Data.Foldable
import Data.Unique
import qualified Data.Map as M
import Data.IORef
import Data.Monoid (All(..), Monoid, mempty, mappend, Endo(..), Dual(..))
import qualified Data.Text as T
import Data.JSString.Text
import Text.HTML.SanitizeXSS (sanitizeXSS)
import GHCJS.Types
import GHCJS.DOM.Types hiding (Event, Element, Document)
import qualified GHCJS.DOM.Types as DOM.Types
import GHCJS.DOM.Element hiding (Element(..))
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.Node as Node
import GHCJS.DOM.Document hiding (Document)
import qualified GHCJS.DOM.Document as Document
import GHCJS.DOM.EventM
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.HTMLInputElement (castToHTMLInputElement, getValue)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Edit
import Unsafe.Coerce
import System.IO.Unsafe

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

-- | TBD make an ADT for this with standard HTML5 tags?
type Tag = T.Text

type ElementSchemaChild = Either Element Text

newtype ChildrenContainer f = ChildrenContainer {
      runChildrenContainer :: f ElementSchemaChild
    }

transChildrenContainer
    :: (forall t . f t -> g t)
    -> ChildrenContainer f
    -> ChildrenContainer g
transChildrenContainer trans = ChildrenContainer . trans . runChildrenContainer

-- | Use an accessor to get a particular child.
getChild
    :: (forall t . f t -> t)
    -> ChildrenContainer f
    -> ElementSchemaChild
getChild accessor = accessor . runChildrenContainer

-- | Use a mutator to set a particular child.
setChild
    :: (forall t . t -> f t -> f t)
    -> ElementSchemaChild
    -> ChildrenContainer f
    -> ChildrenContainer f
setChild mutator x = ChildrenContainer . mutator x . runChildrenContainer

dumpChildren
    :: ( Foldable f )
    => ChildrenContainer f
    -> [ElementSchemaChild]
dumpChildren = toList . runChildrenContainer

{-
-- An example of an f for ChildrenContainer. Delete later.
-- An ElementSchema Two t is guaranteed to carry precisely 2 children, each
-- with a value of type t.
newtype Two t = Two { runTwo :: (t, t) }
instance Functor Two where
    fmap f (Two (x, y)) = Two (f x, f y)
instance Foldable Two where
    foldr f b (Two (x, y)) = f x (f y b)
get1 :: Two t -> t
get1 = fst . runTwo
get2 :: Two t -> t
get2 = snd . runTwo
set1 :: t -> Two t -> Two t
set1 x (Two (_, y)) = Two (x, y)
set2 :: t -> Two t -> Two t
set2 y (Two (x, _)) = Two (x, y)
-}

-- | Description of a DOM element.
data ElementSchema f = ElementSchema {
      elementSchemaTag :: Tag
    , elementSchemaProperties :: Sequence [Properties]
    , elementSchemaAttributes :: Sequence [Attributes]
    , elementSchemaStyle :: Sequence [Style]
    , elementSchemaChildren :: Sequence (ChildrenContainer f)
    -- Sometimes we need to do some effectful work on a proper DOM element in
    -- order to get what we want. For instance, using external libraries like
    -- Leaflet (map visualizations). We throw in the document for good
    -- measure.
    , elementSchemaPostprocess :: ElementSchemaPostprocess
    }

-- | Uniformly alter the form of the children containers by way of a natural
--   transformation.
schemaContainerTrans
    :: forall f g .
       (forall t . f t -> g t)
    -> ElementSchema f
    -> ElementSchema g
schemaContainerTrans f eschema = eschema {
      elementSchemaChildren = alteration (elementSchemaChildren eschema)
    }
  where
    alteration :: Sequence (ChildrenContainer f) -> Sequence (ChildrenContainer g)
    alteration = fmap (transChildrenContainer f)

newtype ElementSchemaPostprocess = ElementSchemaPostprocess {
      runElementSchemaPostprocess :: forall document . IsDocument document => document -> Element -> MomentIO ()
    }

-- | An empty ElementSchema: no attributes, properties, style, children, etc.
--   "div" is the chosen tag.
--   The choice of Const () guarantees that there are no children.
elementSchema :: ElementSchema (Const ())
elementSchema =
    let props = always []
        attrs = always []
        style = always []
        children = always (ChildrenContainer (Const ()))
        postProcess = ElementSchemaPostprocess (\_ _ -> pure ())
    in  ElementSchema "div"
                      props
                      attrs
                      style
                      children
                      postProcess

schemaTag :: Tag -> ElementSchema f -> ElementSchema f
schemaTag tag eschema = eschema {
      elementSchemaTag = tag
    }

schemaStyle
    :: Sequence (Action Style)
    -> ElementSchema f
    -> ElementSchema f
schemaStyle actions schema = schema {
      elementSchemaStyle = mergeStyle actions (elementSchemaStyle schema)
    }
  where
    mergeStyle :: Sequence (Action Style) -> Sequence [Style] -> Sequence [Style]
    mergeStyle actions seqnc = runAction <$> actions <*> seqnc

schemaAttributes
    :: Sequence (Action Attributes)
    -> ElementSchema f
    -> ElementSchema f
schemaAttributes actions schema = schema {
      elementSchemaAttributes = mergeAttributes actions (elementSchemaAttributes schema)
    }
  where
    mergeAttributes
        :: Sequence (Action Attributes)
        -> Sequence [Attributes]
        -> Sequence [Attributes]
    mergeAttributes actions seqnc = runAction <$> actions <*> seqnc

schemaProperties
    :: Sequence (Action Properties)
    -> ElementSchema f
    -> ElementSchema f
schemaProperties actions schema = schema {
      elementSchemaProperties = mergeProperties actions (elementSchemaProperties schema)
    }
  where
    mergeProperties
        :: Sequence (Action Properties)
        -> Sequence [Properties]
        -> Sequence [Properties]
    mergeProperties actions seqnc = runAction <$> actions <*> seqnc

schemaChildren
    :: (Sequence (ChildrenContainer f) -> Sequence (ChildrenContainer g))
    -> ElementSchema f
    -> ElementSchema g
schemaChildren f schema = schema {
      elementSchemaChildren = f (elementSchemaChildren schema)
    }

schemaPostprocess
    :: ElementSchemaPostprocess
    -> ElementSchema f
    -> ElementSchema f
schemaPostprocess post schema = schema {
      elementSchemaPostprocess = post
    }

-- | Make a DOM element using a tag name. A Unique is generated and its
--   hash is set as the "virtual_id" attribute so that you can easily
--   select these elements in browser debuggers.
makeElement :: IsDocument document => document -> Tag -> MomentIO Element
makeElement document tag = do
    Just el <- document `createElement` Just tag
    uniq <- liftIO newUnique
    let hash = hashUnique uniq
    setAttribute el (textToJSString "virtual_id") (textToJSString (T.pack (show hash)))
    pure el

makeText :: Document -> T.Text -> MomentIO Text
makeText document txt = do
    let sanitized = sanitizeXSS txt
    let txtJSString :: JSString = textToJSString sanitized
    Just txt <- document `createTextNode` txtJSString
    pure txt

data RenderedNode = RenderedNode {
      renderedElementSchemaChild :: ElementSchemaChild
    , unrenderNode :: IO ()
    }

renderedFrom :: RenderedNode -> ElementSchemaChild -> Bool
renderedFrom rendered child = renderedElementSchemaChild rendered == child

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
renderElementSchemaChild
    :: ( IsNode parent
       , MonadIO m
       )
    => parent
    -> ElementSchemaChild
    -> m RenderedNode
renderElementSchemaChild parent =
    either (renderElement parent)
           (renderText parent)

render
    :: ( IsNode parent
       , Foldable f
       )
    => Document
    -> parent
    -> Widget f t
    -> MomentIO (RenderedNode, t)
render document parent ebuilder = do
    (el, t) <- buildElement ebuilder document
    rendered <- renderElement parent el
    pure (rendered, t)

unrender :: RenderedNode -> MomentIO ()
unrender = liftIO . unrenderNode

-- | Wire up an ElementSchema: create an element for it, and reactimate the
--   sequences of style, children, etc.
--
--   TBD will the reactimates that we do here ever stop firing? It'd be nice
--   if they were dropped whenever the browser collects the element, but I
--   suspect the reactimates themselves will prevent the elements from ever
--   being collected!
--   Perhaps it's better to use execute, never reactimate, because it seems
--   that the execute will stop once its event is collected. So we could
--   just tack those events onto the element and then when the element is
--   unreachable, so too are the events.
runElementSchema
    :: ( Foldable f )
    => Document
    -> ElementSchema f
    -> MomentIO Element
runElementSchema document eschema = mdo
    el <- makeElement document (elementSchemaTag eschema)

    -- * Children *
    -- This sequenceCommute renders the new list of children whenever it
    -- changes.
    -- Fires with the old nodes whenever the children event fires.
    -- Clears all currently rendered nodes and throws in the new ones. So
    -- if it fires and the list hasne't even changed, we still do DOM
    -- mutation.
 
    {-
    let clearAndRender :: ([RenderedNode], [ElementSchemaChild]) -> MomentIO [RenderedNode]
        clearAndRender (old, new) = liftIO $ do
            mapM_ unrenderNode old
            mapM (renderElementSchemaChild el) new

    (initialChildren, evChildren) <- runSequence childrenSequence
    initialRendered <- mapM (renderElementSchemaChild el) initialChildren
    oldRendered <- stepper initialRendered evRendered
    evRendered <- execute (curry clearAndRender <$> oldRendered <@> evChildren)
    -}

    let childrenSequence = dumpChildren <$> elementSchemaChildren eschema
    (initialChildren, evChildren) <- runSequence childrenSequence
    initialRendered <- mapM (renderElementSchemaChild el) initialChildren
    oldRendered <- stepper initialRendered evRendered
    evRendered <- execute (updateChildren el . reverse . editList <$> childrenEdits)
    let childrenEdits :: Event (EditList RenderedNode ElementSchemaChild)
        childrenEdits = edits renderedFrom <$> oldRendered <@> evChildren

    reactimate (Prelude.print . fmap (mapEdit (const ()) (const ())) . editList  <$> childrenEdits)
    --reactimate ((\x -> Prelude.print (concat ["Update cost: ", show (length (totalCost x))])) <$> evRendered)

    reactimateProperties el (elementSchemaProperties eschema)
    reactimateAttributes el (elementSchemaAttributes eschema)
    reactimateStyle el (elementSchemaStyle eschema)

    _ <- runElementSchemaPostprocess (elementSchemaPostprocess eschema) document el

    pure el 

  where

    childrenEdit :: Element -> RunEdit MomentIO RenderedNode ElementSchemaChild RenderedNode
    childrenEdit el = RunEdit dropChild (consChild el) keepChild

    dropChild :: RenderedNode -> MomentIO ()
    dropChild rendered = do
       {-
        liftIO (putStrLn "dropChild")
        let el = renderedElementSchemaChild rendered
        case el of
            Left el -> do
                Just (vid :: String) <- getAttribute el ("virtual_id" :: String)
                liftIO (putStrLn vid)
            _ -> pure ()
        -}
        liftIO (unrenderNode rendered)

    consChild :: Element -> ElementSchemaChild -> MomentIO RenderedNode
    consChild el child = do
        {-
        liftIO (putStrLn "consChild")
        Just (vid :: String) <- getAttribute el ("virtual_id" :: String)
        liftIO (putStrLn "Parent id:")
        liftIO (putStrLn vid)
        case child of
            Left elem -> do Just (vid' :: String) <- getAttribute elem ("virtual_id" :: String)
                            liftIO (putStrLn "Child id:")
                            liftIO (putStrLn vid')
                            liftIO (Prelude.print (el == elem))
            Right _ -> liftIO (putStrLn " Got text ")
        -}
        renderElementSchemaChild el child

    keepChild :: RenderedNode -> ElementSchemaChild -> MomentIO RenderedNode
    keepChild rendered _ = do
        {-
        liftIO (putStrLn "keepChild")
        let el = renderedElementSchemaChild rendered
        case el of
            Left el -> do
                Just (vid :: String) <- getAttribute el ("virtual_id" :: String)
                liftIO (putStrLn vid)
            _ -> pure ()
        -}
        pure rendered

    updateChildren
        :: Element
        -> [Edit RenderedNode ElementSchemaChild]
        -> MomentIO [RenderedNode]
    updateChildren = runEdits . childrenEdit

    reactimateProperties :: Element -> Sequence [Properties] -> MomentIO ()
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
    :: Element -> JSString -> Nullable JSString -> IO ()

foreign import javascript unsafe "delete $1[$2];" js_removeJSProperty
    :: Element -> JSString -> IO (Nullable JSString)

setJSProperty :: Element -> JSString -> Maybe JSString -> IO ()
setJSProperty el x y = js_setJSProperty el x (maybeToNullable y)

removeJSProperty :: Element -> JSString -> IO (Maybe JSString)
removeJSProperty el x = nullableToMaybe <$> js_removeJSProperty el x

data ReadOnlyElement = ReadOnlyElement {
      getReadOnlyElement :: Element
    , wireEvents :: IORef [Element -> MomentIO ()]
    }

-- | An indexed monad to build elements. We can't come up with a simple monad
--   because pure/return makes no sense; there's not enough information there
--   to come up with a function from g to f.
newtype ElementBuilder g f t = ElementBuilder {
      runElementBuilder
          :: (Document, ReadOnlyElement)
          -> MomentIO (t, ElementSchema g -> ElementSchema f)
    }

instance Functor (ElementBuilder g f) where
    fmap f builder = ElementBuilder $ \rd -> do
        (t, mk) <- runElementBuilder builder rd
        pure (f t, mk)

getDocument :: ElementBuilder f f Document
getDocument = ElementBuilder $ \rd -> pure (fst rd, id)

getElement :: ElementBuilder f f ReadOnlyElement
getElement = ElementBuilder $ \rd -> pure (snd rd, id)

-- TBD can we drop the t parameter? From ElementSchema too? If the children
-- need to carry data, that can go into the functor, no?
type Widget f t = ElementBuilder (Const ()) f t

buildElement
    :: forall f t .
       ( Foldable f )
    => Widget f t
    -> Document
    -> MomentIO (Element, t)
buildElement builder document = mdo
    ioref <- liftIO (newIORef [])
    (t, mkelem) <- runElementBuilder builder (document, ReadOnlyElement element ioref)
    let eschema :: ElementSchema f
        eschema = mkelem elementSchema
    element <- runElementSchema document eschema
    actions <- liftIO (readIORef ioref)
    sequence (fmap (flip ($) element) actions)
    pure (element, t)

class IndexedFunctor f where
    ixmap :: (s -> t) -> f a b s -> f a b t

class IndexedApplicative f where
    ixpure :: t -> f a a t
    ixap :: f a b (s -> t) -> f b c s -> f a c t

class IndexedMonad f where
    ixbind :: f a b s -> (s -> f b c t) -> f a c t

class IndexedMonadFix f where
    ixmfix :: (t -> f a b t) -> f a b t

instance IndexedFunctor ElementBuilder where
    ixmap f builder = ElementBuilder $ \rd -> do
        (t, mk) <- runElementBuilder builder rd
        pure (f t, mk)

instance IndexedApplicative ElementBuilder where
    ixpure x = ElementBuilder $ \_ -> pure (x, id)
    ixap mf mx = ElementBuilder $ \rd -> do
        (f, mkf) <- runElementBuilder mf rd
        (x, mkx) <- runElementBuilder mx rd
        pure (f x, mkx . mkf)

instance IndexedMonad ElementBuilder where
    ixbind mx k = ElementBuilder $ \rd -> do
        (x, mkx) <- runElementBuilder mx rd
        (y, mky) <- runElementBuilder (k x) rd
        pure (y, mky . mkx)

instance IndexedMonadFix ElementBuilder where
    ixmfix mk = ElementBuilder $ \rd -> mdo
        (t, mkf) <- runElementBuilder (mk t) rd
        pure (t, mkf)

infixl 1 >>>=
(>>>=) :: IndexedMonad f => f a b s -> (s -> f b c t) -> f a c t
(>>>=) = ixbind

infixl 1 >>>>
(>>>>) :: IndexedMonad f => f a b s -> f b c t -> f a c t
l >>>> r = l >>>= \_ -> r

infixl 4 <*>>
(<*>>) :: IndexedApplicative f => f a b (s -> t) -> f b c s -> f a c t
(<*>>) = ixap

infixl 4 <$>>
(<$>>) :: IndexedFunctor f => (s -> t) -> f a b s -> f a b t
(<$>>) = ixmap

-- I suspect we'll have issues with recursive do. And that's a big problem;
-- we really need it... Hm, unless we can support it by manually fixing and
-- then using knot...
knot :: (r -> ElementBuilder a b (t, r)) -> ElementBuilder a b t
knot mk = ElementBuilder $ \rd -> mdo
    ((t, r), mkf) <- runElementBuilder (mk r) rd
    pure (t, mkf)

{-
elementBuilderLmap
    :: (h -> g)
    -> ElementBuilder g f t
    -> ElementBuilder h f t
elementBuilderLmap l builder = ElementBuilder $ \rd -> do
    (t, mk) <- runElementBuilder builder rd
    pure (t, lmap l mk)

elementBuilderRmap
    :: (f -> h)
    -> ElementBuilder g f t
    -> ElementBuilder g h t
elementBuilderRmap r builder = ElementBuilder $ \rd -> do
    (t, mk) <- runElementBuilder builder rd
    pure (t, rmap r mk)
-}

momentIO :: MomentIO t -> ElementBuilder a a t
momentIO mio = ElementBuilder $ \_ -> do
    t <- mio
    pure (t, id)

tag :: Tag -> ElementBuilder f f ()
tag tag = ElementBuilder $ \_ -> pure ((), schemaTag tag)

style
    :: Sequence (Action Style)
    -> ElementBuilder f f ()
style s = ElementBuilder $ \_ -> pure ((), schemaStyle s)

attributes
    :: Sequence (Action Attributes)
    -> ElementBuilder f f ()
attributes a = ElementBuilder $ \_ -> pure ((), schemaAttributes a)

properties
    :: Sequence (Action Properties)
    -> ElementBuilder f f ()
properties p = ElementBuilder $ \_ -> pure ((), schemaProperties p)

-- Idea for these: since they must be used lazyily, why not give something like
-- this:
--   clientHeight :: (s -> Double -> t) -> Event s -> Event t
clientHeight :: ElementBuilder a a (IO Double)
clientHeight = ElementBuilder $ \(_, roelement) -> do
    pure (getClientHeight (getReadOnlyElement roelement), id)

clientWidth :: ElementBuilder a a (IO Double) 
clientWidth = ElementBuilder $ \(_, roelement) -> do
    pure (getClientWidth (getReadOnlyElement roelement), id)

scrollHeight :: ElementBuilder a a (IO Int)
scrollHeight = ElementBuilder $ \(_, roelement) -> do
    pure (getScrollHeight (getReadOnlyElement roelement), id)

scrollWidth :: ElementBuilder a a (IO Int)
scrollWidth = ElementBuilder $ \(_, roelement) -> do
    pure (getScrollWidth (getReadOnlyElement roelement), id)

event
    :: ElementEvent event
    => event
    -> ElementBuilder a a (Event (EventData event))
event event = ElementBuilder $ \(_, roelement) -> do
    ev <- elementEvent event roelement
    pure (ev, id)

elementEvent
    :: ElementEvent event
    => event
    -> ReadOnlyElement
    -> MomentIO (Event (EventData event))
elementEvent event roelement = do
    (ev, fire) <- newEvent
    let action = wireElementEvent event fire
    liftIO (modifyIORef (wireEvents roelement) ((:) action))
    pure ev

class ElementEvent event where
    type EventData event :: *
    wireElementEvent :: event -> (EventData event -> IO ()) -> Element -> MomentIO ()

data Click = Click
instance ElementEvent Click where
    type EventData Click = ()
    wireElementEvent Click fire el = liftIO $ do
        _ <- on el Element.click (liftIO (fire ()))
        pure ()

data Submit = Submit
instance ElementEvent Submit where
    type EventData Submit = ()
    wireElementEvent Submit fire el = liftIO $ do
        _ <- on el Element.submit (liftIO (fire ()))
        pure ()

data Input = Input
instance ElementEvent Input where
    type EventData Input = T.Text
    wireElementEvent Input fire el = liftIO $ do
        _ <- on el Element.input (fireEvent el)
        pure ()
      where
        fireEvent el = do
            t <- maybe "" id <$> getValue (castToHTMLInputElement el)
            liftIO (fire t)

data Scroll = Scroll
data ScrollData = ScrollData {
      scrollDataTop :: Int
    , scrollDataLeft :: Int
    }
instance ElementEvent Scroll where
    type EventData Scroll = ScrollData
    wireElementEvent Scroll fire el = liftIO $ do
        _ <- on el Element.scroll (fireEvent el)
        pure ()
      where
        fireEvent el = do
            d <- ScrollData <$> getScrollTop el <*> getScrollLeft el
            liftIO (fire d)

widget
    :: ( Foldable g )
    => Widget g t
    -> ElementBuilder f f (ElementSchemaChild, t)
widget wid = ElementBuilder $ \(doc, _) -> do
    (wid', t) <- buildElement wid doc
    pure ((Left wid', t), id)

text
    :: T.Text
    -> ElementBuilder f f ElementSchemaChild
text txt = ElementBuilder $ \(doc, _) -> do
    txt' <- makeText doc txt
    pure (Right txt', id)

-- | Alter the children of the ElementSchema.
children
    :: forall g f .
       (Sequence (g ElementSchemaChild) -> Sequence (f ElementSchemaChild))
    -> ElementBuilder g f ()
children f = ElementBuilder $ \rd -> do
    pure ((), schemaChildren f')
  where
    f' :: Sequence (ChildrenContainer g) -> Sequence (ChildrenContainer f)
    f' = fmap ChildrenContainer . f . fmap runChildrenContainer
