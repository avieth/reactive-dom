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

module Reactive.DOM.Node where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
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
data VirtualElement = VirtualElement {
      virtualElementUnique :: Unique
    , virtualElementTag :: JSString
    , virtualElementAttributes :: Sequence Attributes
    , virtualElementStyle :: Sequence Style
    , virtualElementChildren :: Sequence [VirtualNode]
    , virtualElementEvents :: IORef VirtualElementEvents
    , virtualElementReactimates :: IORef VirtualElementReactimates
    }

instance Eq VirtualElement where
    x == y = virtualElementUnique x == virtualElementUnique y

instance Ord VirtualElement where
    x `compare` y = virtualElementUnique x `compare` virtualElementUnique y

mapStyle :: (Style -> Style) -> VirtualElement -> VirtualElement
mapStyle f velem = velem { virtualElementStyle = fmap f (virtualElementStyle velem) }

data RenderedVirtualElement = RenderedVirtualElement {
      renderedVirtualElementUnique :: Unique
    , renderedVirtualElement :: Element
    }

renderedFrom :: RenderedVirtualElement -> VirtualElement -> Bool
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
    -> JSString
    -> m RenderedText
renderText document str = do
    Just text <- document `createTextNode` str
    return (RenderedText str text)

type VirtualNode = Either VirtualElement JSString
type RenderedNode = Either RenderedVirtualElement RenderedText

node :: VirtualElement -> VirtualNode
node = Left

text :: JSString -> VirtualNode
text = Right

renderVirtualNode
    :: ( IsDocument document
       )
    => document
    -> VirtualNode
    -> MomentIO RenderedNode
renderVirtualNode document = either (fmap Left . renderVirtualElement document)
                                    (fmap Right . liftIO . renderText document)

renderedNode :: RenderedNode -> Node
renderedNode = either (toNode . renderedVirtualElement) (toNode . renderedTextNode)

virtualElement
    :: ( MonadIO m )
    => JSString
    -> Sequence Attributes
    -> Sequence Style
    -> Sequence [VirtualNode]
    -> m VirtualElement
virtualElement tagName attrs style kids = do
    unique <- liftIO $ newUnique
    refEvent <- liftIO $ newIORef M.empty
    refReactimate <- liftIO $ newIORef []
    return (VirtualElement unique tagName attrs style kids refEvent refReactimate)

element
    :: ( MonadIO m )
    => JSString
    -> Sequence Attributes
    -> Sequence Style
    -> Sequence [VirtualNode]
    -> m VirtualElement
element = virtualElement

renderVirtualElement
    :: ( IsDocument document
       )
    => document
    -> VirtualElement
    -> MomentIO RenderedVirtualElement
renderVirtualElement document velem = do
    Just el <- document `createElement` (Just (virtualElementTag velem))
    reactimateAttributes el (virtualElementAttributes velem)
    reactimateStyle el (virtualElementStyle velem)
    reactimateChildren document el (virtualElementChildren velem)
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
    => VirtualElement
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
    => VirtualElement
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
    -> Sequence [VirtualNode]
    -> MomentIO ()
reactimateChildren document parent sequence = mdo

    -- Use the first element of the sequence to render all children.
    firsts <- forM (sequenceFirst sequence) (renderVirtualNode document)
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

    execute (update parent currentlyRendered <$> sequenceRest sequence)

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
        -> [VirtualNode]
        -> MomentIO ()
    update parent current new = do
        currentRendered <- liftIO $ readIORef current
        forM_ currentRendered (removeChild parent . Just . renderedNode)
        newRendered <- forM new (renderVirtualNode document)
        forM_ newRendered (appendChild parent . Just . renderedNode)
        liftIO $ writeIORef current newRendered
        return ()

type Style = M.Map JSString JSString

reactimateStyle :: Element -> Sequence Style -> MomentIO ()
reactimateStyle element sequence = do
    liftIO $ addStyle element (sequenceFirst sequence)
    currentStyle <- liftIO $ newIORef (sequenceFirst sequence)
    let changeStyle new = do
            current <- readIORef currentStyle
            let (add, remove) = diffStyle current new
            removeStyle element remove
            addStyle element add
            writeIORef currentStyle new
    reactimate (changeStyle <$> (sequenceRest sequence))
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

reactimateAttributes :: Element -> Sequence Attributes -> MomentIO ()
reactimateAttributes element sequence = do
    liftIO $ addAttributes element (sequenceFirst sequence)
    currentAttributes <- liftIO $ newIORef (sequenceFirst sequence)
    let changeAttributes new = do
            current <- readIORef currentAttributes
            let (add, remove) = diffAttributes current new
            removeAttributes element remove
            addAttributes element add
            writeIORef currentAttributes new
    reactimate (changeAttributes <$> (sequenceRest sequence))
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

centred :: MonadIO m => Sequence VirtualNode -> m VirtualElement
centred vnodes = element "div"
                         (always M.empty)
                         (always centredStyle)
                         (pure <$> vnodes)
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

horizontally :: MonadIO m => Sequence [VirtualElement] -> m VirtualElement
horizontally velems = element "div"
                              (always M.empty)
                              (always flexStyle)
                              (((fmap Left) . alignem) <$> velems)
  where
    flexStyle :: Style
    flexStyle = M.fromList [
          ("display", "flex")
        , ("flex-direction", "row")
        ]
    alignem :: [VirtualElement] -> [VirtualElement]
    alignem nodes = let share :: Double
                        share = if length nodes == 0
                                then 1
                                else 1 / fromIntegral (length nodes)
                        width = toJSString (show (share * 100) ++ "%")
                        setWidth :: Style -> Style
                        setWidth = M.alter (const (Just width)) "width"
                    in  fmap (mapStyle setWidth) nodes

vertically :: MonadIO m => Sequence [VirtualElement] -> m VirtualElement
vertically velems = element "div"
                            (always M.empty)
                            (always flexStyle)
                            (((fmap Left) . alignem) <$> velems)
  where
    flexStyle :: Style
    flexStyle = M.fromList [
          ("display", "flex")
        , ("flex-direction", "column")
        ]
    alignem :: [VirtualElement] -> [VirtualElement]
    alignem nodes = let share :: Double
                        share = if length nodes == 0
                                then 1
                                else 1 / fromIntegral (length nodes)
                        height = toJSString (show (share * 100) ++ "%")
                        setHeight :: Style -> Style
                        setHeight = M.alter (const (Just height)) "height"
                    in  fmap (mapStyle setHeight) nodes

render
    :: ( IsElement parent
       , IsDocument document
       )
    => document
    -> parent
    -> VirtualNode
    -> MomentIO ()
render document parent vnode = do
    rendered <- renderVirtualNode document vnode
    parent `appendChild` (Just (renderedNode rendered))
    return ()
