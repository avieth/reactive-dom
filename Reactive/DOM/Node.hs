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

-- | 
data VirtualElement (m :: * -> *) = VirtualElement {
      virtualElementUnique :: Unique
    , virtualElementTag :: m JSString
    , virtualElementAttributes :: LiveSequence (m Attributes)
    , virtualElementStyle :: LiveSequence (m Style)
    , virtualElementChildren :: LiveSequence (m [MomentIO (VirtualNode m)])
    , virtualElementEvents :: IORef VirtualElementEvents
    , virtualElementReactimates :: IORef VirtualElementReactimates
    }

instance Eq (VirtualElement m) where
    x == y = virtualElementUnique x == virtualElementUnique y

instance Ord (VirtualElement m) where
    x `compare` y = virtualElementUnique x `compare` virtualElementUnique y

velemTrans
    :: ( Functor m )
    => (forall t . m t -> n t)
    -> VirtualElement m
    -> VirtualElement n
velemTrans trans velem = velem {
      virtualElementTag = trans (virtualElementTag velem)
    , virtualElementAttributes = fmap trans (virtualElementAttributes velem)
    , virtualElementStyle = fmap trans (virtualElementStyle velem)
    , virtualElementChildren = fmap (trans . (fmap . fmap . fmap) (vnodeTrans trans)) (virtualElementChildren velem)
    }

vtextTrans
    :: ( )
    => (forall t . m t -> n t)
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
       , MonadIO m
       )
    => document
    -> VirtualText Identity
    -> m RenderedText
renderText document str = do
    Just text <- document `createTextNode` (runIdentity str)
    return (RenderedText (runIdentity str) text)

type VirtualText m = m JSString

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
    :: m JSString
    -> Sequence (m Attributes)
    -> Sequence (m Style)
    -> Sequence (m [(MomentIO (VirtualNode m))])
    -> MomentIO (VirtualElement m)
virtualElement tagName attrs style kids = do
    lattrs <- liveSequence attrs
    lstyle <- liveSequence style
    lkids <- liveSequence kids
    unique <- liftIO $ newUnique
    refEvent <- liftIO $ newIORef M.empty
    refReactimate <- liftIO $ newIORef []
    return (VirtualElement unique tagName lattrs lstyle lkids refEvent refReactimate)

element
    :: m JSString
    -> Sequence (m Attributes)
    -> Sequence (m Style)
    -> Sequence (m [(MomentIO (VirtualNode m))])
    -> MomentIO (VirtualElement m)
element = virtualElement

renderVirtualElement
    :: ( IsDocument document
       )
    => document
    -> VirtualElement Identity
    -> MomentIO RenderedVirtualElement
renderVirtualElement document velem = do
    Just el <- document `createElement` (Just (runIdentity (virtualElementTag velem)))
    reactimateAttributes el (fmap runIdentity (virtualElementAttributes velem))
    reactimateStyle el (fmap runIdentity (virtualElementStyle velem))
    reactimateChildren document el (fmap runIdentity (virtualElementChildren velem))
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
    -> MomentIO (Event t)
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
                      return ev

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
    -> LiveSequence [MomentIO (VirtualNode Identity)]
    -> MomentIO ()
reactimateChildren document parent sequence = mdo

    -- Use the first element of the sequence to render all children.
    cur <- sequenceCurrent sequence
    firsts <- forM cur ((=<<) (renderVirtualNode document))
    forM_ firsts (appendChild parent . Just . renderedNode)

    -- We need to have the list of currently rendered elements, so that we can
    -- compare them with the new list (from sequence) and so that we have the
    -- actual @Element@ values to use in @removeChild@. One way to grab this is
    -- via @newBehavior@, calling its update mechanism whenever we change the
    -- children here. So in the @update@ function which will be fmapped into
    -- the event in @sequence@, responsible for updating the DOM here, we would
    -- have something like
    --      liftIO $ setCurrent newRendered
    -- but I found that this causes event bindings set up in @wireVirtualEvents@
    -- to fail. The part where they fire the relevant reactive-banana event
    -- would simply quit; no exception, it would just quit, so that a print
    -- statement before this action would run, but a print statement after it
    -- would *not* run!
    -- Anyway, by using an IORef here to manually store the list of current
    -- children, the problem doesn't arise.
    --
    --(currentlyRendered, changeCurrentlyRendered) <- newBehavior firsts
    --let changes :: Event ([RenderedVirtualElement], [VirtualElement])
    --    changes = (,) <$> currentlyRendered <@> (sequenceRest sequence)

    currentlyRendered <- liftIO $ newIORef firsts

    execute (update parent currentlyRendered <$> sequenceNext sequence)

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
        -> [MomentIO (VirtualNode Identity)]
        -> MomentIO ()
    update parent current new = do
        currentRendered <- liftIO $ readIORef current
        forM_ currentRendered (removeChild parent . Just . renderedNode)
        newRendered <- forM new ((=<<) (renderVirtualNode document))
        forM_ newRendered (appendChild parent . Just . renderedNode)
        liftIO $ writeIORef current newRendered
        return ()

type Style = M.Map JSString JSString

reactimateStyle :: Element -> LiveSequence Style -> MomentIO ()
reactimateStyle element sequence = do
    cur <- sequenceCurrent sequence
    liftIO $ addStyle element cur
    currentStyle <- liftIO $ newIORef cur
    let changeStyle new = do
            current <- readIORef currentStyle
            let (add, remove) = diffStyle current new
            removeStyle element remove
            addStyle element add
            writeIORef currentStyle new
    reactimate (changeStyle <$> (sequenceNext sequence))
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

type Attributes = M.Map JSString JSString

reactimateAttributes :: Element -> LiveSequence Attributes -> MomentIO ()
reactimateAttributes element sequence = do
    cur <- sequenceCurrent sequence
    liftIO $ addAttributes element cur
    currentAttributes <- liftIO $ newIORef cur
    let changeAttributes new = do
            current <- readIORef currentAttributes
            let (add, remove) = diffAttributes current new
            removeAttributes element remove
            addAttributes element add
            writeIORef currentAttributes new
    reactimate (changeAttributes <$> (sequenceNext sequence))
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

centred :: Applicative m => Sequence (m (MomentIO (VirtualNode m))) -> MomentIO (VirtualElement m)
centred vnodes = element (pure "div")
                         (always (pure M.empty))
                         (always (pure centredStyle))
                         ((fmap . fmap) pure vnodes)
  where
    centredStyle :: Style
    centredStyle = M.fromList [
          ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        , ("position", "absolute")
        , ("width", "100%")
        , ("height", "100%")
        ]

horizontally
    :: forall m . 
       ( Applicative m )
    => Sequence (m [(MomentIO (VirtualElement m))])
    -> MomentIO (VirtualElement m)
horizontally velems = element (pure "div")
                              (always (pure M.empty))
                              (always (pure flexStyle))
                              ((fmap . fmap) (((fmap . fmap) Left) . alignem) velems)
  where
    flexStyle :: Style
    flexStyle = M.fromList [
          ("display", "flex")
        , ("flex-direction", "row")
        ]
    alignem :: [MomentIO (VirtualElement m)] -> [MomentIO (VirtualElement m)]
    alignem nodes = let share :: Double
                        share = if length nodes == 0
                                then 1
                                else 1 / fromIntegral (length nodes)
                        width = toJSString (show (share * 100) ++ "%")
                        setWidth :: Style -> Style
                        setWidth = M.alter (const (Just width)) "width"
                    in  (fmap . fmap) (mapStyle setWidth) nodes

vertically
    :: forall m . 
       ( Applicative m )
    => Sequence (m [(MomentIO (VirtualElement m))])
    -> MomentIO (VirtualElement m)
vertically velems = element (pure "div")
                            (always (pure M.empty))
                            (always (pure flexStyle))
                            ((fmap . fmap) (((fmap . fmap) Left) . alignem) velems)
  where
    flexStyle :: Style
    flexStyle = M.fromList [
          ("display", "flex")
        , ("flex-direction", "column")
        ]
    alignem :: [MomentIO (VirtualElement m)] -> [MomentIO (VirtualElement m)]
    alignem nodes = let share :: Double
                        share = if length nodes == 0
                                then 1
                                else 1 / fromIntegral (length nodes)
                        height = toJSString (show (share * 100) ++ "%")
                        setHeight :: Style -> Style
                        setHeight = M.alter (const (Just height)) "height"
                    in  (fmap . fmap) (mapStyle setHeight) nodes

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

