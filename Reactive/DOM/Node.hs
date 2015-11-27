{-|
Module      : Reactive.DOM.Node
Description : Definition of reactive DOM elements/nodes.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Reactive.DOM.Node where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Unique
import qualified Data.Map as M
import Data.IORef
import GHCJS.Types
import GHCJS.DOM.Types hiding (Event)
import qualified GHCJS.DOM.Types as DOM.Types
import GHCJS.DOM.Element as Element
import GHCJS.DOM.Node as Node
import GHCJS.DOM.Document as Document
import GHCJS.DOM.EventM
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.EventTargetClosures
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Unsafe.Coerce

type Properties = M.Map JSString JSString
type Style = M.Map JSString JSString
type Attributes = M.Map JSString JSString

type VirtualElementEvents = M.Map DOMString VirtualEvent

-- | Contains a reactive-banana Event and a way to fire it.
data VirtualEvent where
    VirtualEvent
        :: forall event t .
           ( IsEvent event )
        => Event t
        -> (Element -> event -> IO t)
        -> Handler t
        -> VirtualEvent

type VirtualElementReactimates = [VirtualReactimate]

data VirtualReactimate where
    VirtualReactimate
        :: forall event .
           ( )
        => Event event
        -> (Element -> event -> IO ())
        -> VirtualReactimate

type NaturalTransformation m n = forall t . m t -> n t

{-
data Eval m t where
    Eval :: m t -> (forall s . m s -> s) -> Eval m t

instance Functor m => Functor (Eval m) where
    fmap f (Eval x g) = Eval (f <$> x) g

instance Applicative (Eval Identity) where
    pure x = Eval (pure x) runIdentity
    (Eval f _) <*> (Eval x _) = Eval (f <*> x) runIdentity

makeEval :: (forall s . m s -> s) -> m t -> Eval m t
makeEval f x = Eval x f

eval :: Eval m t -> t
eval (Eval x f) = f x
-}

-- | 
data VirtualElement (m :: * -> *) = VirtualElement {
      virtualElementUnique :: Unique
    , virtualElementTag :: m JSString
    , virtualElementProperties :: m (SBehavior Properties)
    , virtualElementAttributes :: m (SBehavior  Attributes)
    , virtualElementStyle :: m (SBehavior Style)
    , virtualElementChildren :: m (SBehavior [VirtualNode m])
    , virtualElementEvents :: IORef VirtualElementEvents
    , virtualElementReactimates :: IORef VirtualElementReactimates
    }

instance Eq (VirtualElement m) where
    x == y = virtualElementUnique x == virtualElementUnique y

instance Ord (VirtualElement m) where
    x `compare` y = virtualElementUnique x `compare` virtualElementUnique y

velemMergeStyle
    :: forall f g m .
       ( Functor m
       , UnionsTo (Sequence f g) (SBehavior) ~ SBehavior
       , Unionable f Identity g Identity
       )
    => Sequence f g Style
    -> VirtualElement m
    -> VirtualElement m
velemMergeStyle seq velem = velem {
      virtualElementStyle = mergeStyle seq <$> virtualElementStyle velem
    }
  where
    mergeStyle :: Sequence f g Style -> SBehavior Style -> SBehavior Style
    mergeStyle left right = left <||> right

type VirtualText m = m JSString

velemTrans
    :: ( Functor m )
    => NaturalTransformation m n
    -> VirtualElement m
    -> VirtualElement n
velemTrans trans velem = velem {
      virtualElementTag = trans (virtualElementTag velem)
    , virtualElementProperties = trans (virtualElementProperties velem)
    , virtualElementAttributes = trans (virtualElementAttributes velem)
    , virtualElementStyle = trans (virtualElementStyle velem)
    , virtualElementChildren = trans ((fmap . fmap . fmap) (vnodeTrans trans) (virtualElementChildren velem))
    }

vtextTrans
    :: ( )
    => NaturalTransformation m n
    -> VirtualText m
    -> VirtualText n
vtextTrans = ($)

vnodeTrans
    :: ( Functor m )
    => (forall t . m t -> n t)
    -> VirtualNode m
    -> VirtualNode n
vnodeTrans trans = either (Left . velemTrans trans) (Right . vtextTrans trans)

mapStyle
     :: Functor m
     => (Style -> Style)
     -> VirtualElement m
     -> VirtualElement m
mapStyle f velem = velem { virtualElementStyle = (fmap . fmap) f (virtualElementStyle velem) }

data RenderedVirtualElement = RenderedVirtualElement {
      renderedVirtualElementUnique :: Unique
    , renderedVirtualElement :: Element
    }

renderedFrom :: RenderedVirtualElement -> VirtualElement m -> Bool
renderedFrom x y = renderedVirtualElementUnique x == virtualElementUnique y

data RenderedText = RenderedText {
      renderedTextString :: JSString
    , renderedTextNode :: Text
    }

renderText
    :: ( IsDocument document
       , MonadIO n
       )
    => document
    -> VirtualText Identity
    -> n RenderedText
renderText document str = do
    Just text <- document `createTextNode` (runIdentity str)
    return (RenderedText (runIdentity str) text)

type VirtualNode m = Either (VirtualElement m) (VirtualText m)
type RenderedNode = Either RenderedVirtualElement RenderedText

node :: VirtualElement m -> VirtualNode m
node = Left

text :: m JSString -> VirtualNode m
text = Right

renderVirtualNode
    :: ( IsDocument document
       )
    => document
    -> VirtualNode Identity
    -> MomentIO RenderedNode
renderVirtualNode document = either (fmap Left . renderVirtualElement document)
                                    (fmap Right . liftIO . renderText document)

renderedNode :: RenderedNode -> Node
renderedNode = either (toNode . renderedVirtualElement) (toNode . renderedTextNode)

virtualElement
    :: ( Applicative m )
    => m JSString
    -> m (SBehavior Properties)
    -> m (SBehavior Attributes)
    -> m (SBehavior Style)
    -> m (SBehavior [VirtualNode m])
    -> MomentIO (VirtualElement m)
virtualElement tagName props attrs style kids = do
    unique <- liftIO $ newUnique
    refEvent <- liftIO $ newIORef M.empty
    refReactimate <- liftIO $ newIORef []
    return (VirtualElement unique tagName props attrs style kids refEvent refReactimate)

renderVirtualElement
    :: ( IsDocument document
       )
    => document
    -> VirtualElement Identity
    -> MomentIO RenderedVirtualElement
renderVirtualElement document velem = do
    Just el <- document `createElement` (Just (runIdentity (virtualElementTag velem)))
    reactimateProperties el (runIdentity (virtualElementProperties velem))
    reactimateAttributes el (runIdentity (virtualElementAttributes velem))
    reactimateStyle el (runIdentity (virtualElementStyle velem))
    reactimateChildren document el (runIdentity (virtualElementChildren velem))
    events <- liftIO $ readIORef (virtualElementEvents velem)
    wireVirtualEvents el events
    reactimates <- liftIO $ readIORef (virtualElementReactimates velem)
    wireVirtualReactimates el reactimates
    return (RenderedVirtualElement (virtualElementUnique velem) el)

wireVirtualEvents
    :: ( )
    => Element
    -> VirtualElementEvents
    -> MomentIO ()
wireVirtualEvents el vevents = M.foldWithKey (wireVirtualEvent el) (return ()) vevents
  where
    wireVirtualEvent :: Element -> DOMString -> VirtualEvent -> MomentIO () -> MomentIO ()
    wireVirtualEvent el eventName (VirtualEvent ev io fire) next = do
        liftIO $ on el (EventName eventName) $ do
                     eventData <- ask
                     datum <- liftIO $ io el eventData
                     liftIO (fire datum)
        next

virtualEvent
    :: IsEvent event
    => VirtualElement m
    -> EventName Element event
    -> (Element -> event -> IO t)
    -> MomentIO (SEvent t)
virtualEvent velem (EventName eventName) io = do
    events <- liftIO $ readIORef (virtualElementEvents velem)
    case M.lookup eventName events of
        -- This coercion should be safe. The only way this virtual event could
        -- land here is by some other use of this function @virtualEvent@.
        Just (VirtualEvent ev _ _) -> return (unsafeCoerce ev)
        Nothing -> do (ev, fire) <- newEvent
                      let nextEvents :: VirtualElementEvents
                          nextEvents = M.insert eventName (VirtualEvent ev io fire) events
                      liftIO $ writeIORef (virtualElementEvents velem) nextEvents
                      pure (eventToSEvent ev)

wireVirtualReactimates
    :: ( )
    => Element
    -> VirtualElementReactimates
    -> MomentIO ()
wireVirtualReactimates el vreactimates = forM_ vreactimates (wireVirtualReactimate el)
  where
    wireVirtualReactimate :: Element -> VirtualReactimate -> MomentIO ()
    wireVirtualReactimate el (VirtualReactimate ev action) = reactimate (action el <$> ev)

virtualReactimate
    :: ( )
    => VirtualElement m
    -> Event event
    -> (Element -> event -> IO ())
    -> MomentIO ()
virtualReactimate velem ev action = do
    vreactimates <- liftIO $ readIORef (virtualElementReactimates velem)
    let vreactimate = VirtualReactimate ev action
    liftIO $ writeIORef (virtualElementReactimates velem) (vreactimate : vreactimates)

-- We have the sequence of virtual elements... we can compare these under Eq,
-- but that's not enough... we still must recover the actual DOM element
-- associated with each one, in order to remove it.
reactimateChildren
    :: forall document parent .
       ( IsDocument document
       , IsElement parent
       )
    => document
    -> parent
    -> SBehavior [VirtualNode Identity]
    -> MomentIO ()
reactimateChildren document parent children = do
    currentlyRendered <- liftIO $ newIORef []
    -- We don't update until immediately after the children change, to
    -- ensure that the whole subtree in there has been updated.
    -- This has no effect on the immediate part; it's still rendered
    -- immediately by sequenceReactimate.
    -- Careful to choose lag' and not lag, as we do want to force the
    -- children event.
    let delayed = lag' children
    sequenceExecute (fmap Identity . runIdentity)
                    (fmap Identity . runIdentity)
                    (update parent currentlyRendered <$> delayed)
    return ()
  where
    -- A stupid, minimally efficient diff: remove all old, add all new.
    -- We do one check: if everything in the list is JavaScript equal to
    -- its counterpart in the other, we can do nothing.
    {-
    diff :: [VirtualElement] -> [VirtualElement] -> ([VirtualElement], [VirtualElement])
    diff old new =
        if getAll (mconcat (All (length old == length new) : zipWith (\x y -> All (x == y)) old new))
        then ([], [])
        else (old, new)
    -}
    update
        :: parent
        -> IORef [RenderedNode]
        -> [VirtualNode Identity]
        -> MomentIO ()
    update parent current new = do
        currentRendered <- liftIO $ readIORef current
        forM_ currentRendered (removeChild parent . Just . renderedNode)
        newRendered <- forM new (renderVirtualNode document)
        forM_ newRendered (appendChild parent . Just . renderedNode)
        liftIO $ writeIORef current newRendered
        return ()

foreign import javascript unsafe "$1[$2]=$3;" js_setJSProperty
    :: Element -> JSString -> Nullable JSString -> IO ()

foreign import javascript unsafe "delete $1[$2];" js_removeJSProperty
    :: Element -> JSString -> IO (Nullable JSString)

setJSProperty :: Element -> JSString -> Maybe JSString -> IO ()
setJSProperty el x y = js_setJSProperty el x (maybeToNullable y)

removeJSProperty :: Element -> JSString -> IO (Maybe JSString)
removeJSProperty el x = nullableToMaybe <$> js_removeJSProperty el x

reactimateProperties :: Element -> SBehavior Properties -> MomentIO ()
reactimateProperties element sequence = do
    currentProperties <- liftIO $ newIORef mempty
    let changeProperties new = do
            current <- readIORef currentProperties
            let (add, remove) = diffProperties current new
            removeProperties element remove
            addProperties element add
            writeIORef currentProperties new
    sequenceReactimate runIdentity
                       runIdentity
                       (changeProperties <$> sequence)
    return ()

addProperties :: Element -> M.Map JSString JSString -> IO ()
addProperties element properties = do
    let propertiesList :: [(JSString, Maybe JSString)]
        propertiesList = M.foldWithKey (\x y -> (:) (x, Just y)) [] properties
    forM_ propertiesList (\(x, y) -> setJSProperty element x y)

removeProperties :: Element -> M.Map JSString JSString -> IO ()
removeProperties element properties = do
    let propertyNames :: [JSString]
        propertyNames = M.keys properties
    _ :: [Maybe JSString] <- forM propertyNames (removeJSProperty element)
    return ()

diffProperties old new = (toAdd, toRemove)
  where
    toAdd = M.differenceWith justWhenDifferent new old
    toRemove = M.differenceWith justWhenDifferent old new
    justWhenDifferent x y = if x /= y then Just x else Nothing

reactimateStyle :: Element -> SBehavior Style -> MomentIO ()
reactimateStyle element sequence = do
    currentStyle <- liftIO $ newIORef mempty
    let changeStyle new = do
            current <- readIORef currentStyle
            let (add, remove) = diffStyle current new
            removeStyle element remove
            addStyle element add
            writeIORef currentStyle new
    sequenceReactimate runIdentity
                       runIdentity
                       (changeStyle <$> sequence)
    return ()

addStyle :: Element -> M.Map JSString JSString -> IO ()
addStyle element style = do
    let styleList :: [(JSString, Maybe JSString, JSString)]
        styleList = M.foldWithKey (\x y -> (:) (x, Just y, "")) [] style
    Just css <- getStyle element
    forM_ styleList (\(x, y, z) -> setProperty css x y z)

removeStyle :: Element -> M.Map JSString JSString -> IO ()
removeStyle element style = do
    let styleNames :: [JSString]
        styleNames = M.keys style
    Just css <- getStyle element
    -- We bind here because we have to give a type signature in order to
    -- disambiguate.
    _ :: [Maybe JSString] <- forM styleNames (removeProperty css)
    return ()

-- | First component is the new style not present in old.
--   Second is the rules in old not present in new (to be removed).
diffStyle oldStyle newStyle = (toAdd, toRemove)
  where
    toAdd = M.differenceWith justWhenDifferent newStyle oldStyle
    toRemove = M.differenceWith justWhenDifferent oldStyle newStyle
    justWhenDifferent x y = if x /= y then Just x else Nothing

reactimateAttributes :: Element -> SBehavior Attributes -> MomentIO ()
reactimateAttributes element sequence = do
    currentAttributes <- liftIO $ newIORef mempty
    let changeAttributes new = do
            current <- readIORef currentAttributes
            let (add, remove) = diffAttributes current new
            removeAttributes element remove
            addAttributes element add
            writeIORef currentAttributes new
    sequenceReactimate runIdentity
                       runIdentity
                       (changeAttributes <$> sequence)
    return ()

addAttributes :: Element -> M.Map JSString JSString -> IO ()
addAttributes el attrs = do
    let attrList :: [(JSString, JSString)]
        attrList = M.foldWithKey (\x y -> (:) (x, y)) [] attrs
    forM_ attrList (uncurry (setAttribute el))

removeAttributes :: Element -> M.Map JSString JSString -> IO ()
removeAttributes el attrs = do
    let attrNames :: [JSString]
        attrNames = M.keys attrs
    forM_ attrNames (removeAttribute el)

diffAttributes old new = (toAdd, toRemove)
  where
    toAdd = M.differenceWith justWhenDifferent new old
    toRemove = M.differenceWith justWhenDifferent old new
    justWhenDifferent x y = if x /= y then Just x else Nothing

maybeRender
    :: ( IsElement parent
       , IsDocument document
       )
    => document
    -> parent
    -> Maybe RenderedVirtualElement
    -> VirtualElement Identity
    -> MomentIO RenderedVirtualElement
maybeRender document parent maybeRendered velem = case maybeRendered of
    Nothing -> render document parent velem
    Just vrendered -> if renderedFrom vrendered velem
                      then return vrendered
                      else do unrender parent vrendered
                              render document parent velem

render
    :: ( IsElement parent
       , IsDocument document
       )
    => document
    -> parent
    -> VirtualElement Identity
    -> MomentIO RenderedVirtualElement
render document parent vnode = do
    vrendered <- renderVirtualElement document vnode
    parent `appendChild` (Just (renderedVirtualElement vrendered))
    return vrendered

unrender
    :: ( IsElement parent
       )
    => parent
    -> RenderedVirtualElement
    -> MomentIO ()
unrender parent vrendered = do
    parent `removeChild` (Just (renderedVirtualElement vrendered))
    return ()
