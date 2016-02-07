{-|
Module      : Reactive.DOM.Node
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

module Reactive.DOM.Node where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.List (delete)
import Data.Semigroup (Semigroup(..))
import Data.Functor.Identity
import Data.Unique
import qualified Data.Map as M
import Data.IORef
import Data.Monoid (All(..), Monoid, mempty, mappend)
import qualified Data.Text as T
import Data.JSString.Text
import Text.HTML.SanitizeXSS (sanitizeXSS)
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
import System.IO.Unsafe

import Debug.Trace

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
computeStyle = foldr (flip (<>)) M.empty . fmap (fst . runIdentifiedMap)

computeProperties :: [Properties] -> M.Map T.Text T.Text
computeProperties = computeStyle

computeAttributes :: [Attributes] -> M.Map T.Text T.Text
computeAttributes = computeStyle

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

-- | TBD make an ADT for this with standard HTML5 tags?
type Tag = T.Text

-- | Contains all information to describe a DOM node.
data VirtualElement = VirtualElement {
      virtualElementUnique :: Unique
    , virtualElementTag :: Tag
    , virtualElementProperties :: Sequence [Properties]
    , virtualElementAttributes :: Sequence [Attributes]
    , virtualElementStyle :: Sequence [Style]
    , virtualElementChildren :: Sequence [VirtualNode]
    , virtualElementEvents :: IORef VirtualElementEvents
    , virtualElementReactimates :: IORef VirtualElementReactimates
    }

instance Eq VirtualElement where
    x == y = virtualElementUnique x == virtualElementUnique y

instance Ord VirtualElement where
    x `compare` y = virtualElementUnique x `compare` virtualElementUnique y

styleVirtualElement
    :: Sequence (Action Style)
    -> VirtualElement
    -> VirtualElement
styleVirtualElement actions velem = velem {
      virtualElementStyle = mergeStyle actions (virtualElementStyle velem)
    }
  where
    mergeStyle :: Sequence (Action Style) -> Sequence [Style] -> Sequence [Style]
    mergeStyle actions seqnc = runAction <$> actions <*> seqnc

attributesVirtualElement
    :: Sequence (Action Attributes)
    -> VirtualElement
    -> VirtualElement
attributesVirtualElement actions velem = velem {
      virtualElementAttributes = mergeAttributes actions (virtualElementAttributes velem)
    }
  where
    mergeAttributes
        :: Sequence (Action Attributes)
        -> Sequence [Attributes]
        -> Sequence [Attributes]
    mergeAttributes actions seqnc = runAction <$> actions <*> seqnc

propertiesVirtualElement
    :: Sequence (Action Properties)
    -> VirtualElement
    -> VirtualElement
propertiesVirtualElement actions velem = velem {
      virtualElementProperties = mergeProperties actions (virtualElementProperties velem)
    }
  where
    mergeProperties
        :: Sequence (Action Properties)
        -> Sequence [Properties]
        -> Sequence [Properties]
    mergeProperties actions seqnc = runAction <$> actions <*> seqnc

type VirtualText = T.Text

data RenderedVirtualElement = RenderedVirtualElement {
      renderedVirtualElementUnique :: Unique
    , renderedVirtualElement :: Element
    }

renderedFrom :: RenderedVirtualElement -> VirtualElement -> Bool
renderedFrom x y = renderedVirtualElementUnique x == virtualElementUnique y

nodeRenderedFrom :: RenderedNode -> VirtualNode -> Bool
nodeRenderedFrom x y = case (x, y) of
    (Left x', Left y') -> renderedFrom x' y'
    _ -> False

data RenderedText = RenderedText {
      renderedTextString :: JSString
    , renderedTextNode :: Text
    }

renderText
    :: ( IsDocument document
       , MonadIO n
       )
    => document
    -> VirtualText
    -> n RenderedText
renderText document txt = do
    let sanitized = sanitizeXSS txt
    let txtJSString :: JSString = textToJSString sanitized
    Just text <- document `createTextNode` txtJSString
    return (RenderedText txtJSString text)

type VirtualNode = Either VirtualElement VirtualText
type RenderedNode = Either RenderedVirtualElement RenderedText

node :: VirtualElement -> VirtualNode
node = Left

text :: T.Text -> VirtualNode
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
    :: T.Text
    -> Sequence [Properties]
    -> Sequence [Attributes]
    -> Sequence [Style]
    -> Sequence [VirtualNode]
    -> MomentIO VirtualElement
virtualElement tagName props attrs style kids = do
    unique <- liftIO $ newUnique
    refEvent <- liftIO $ newIORef M.empty
    refReactimate <- liftIO $ newIORef []
    return (VirtualElement unique tagName props attrs style kids refEvent refReactimate)

renderVirtualElement
    :: ( IsDocument document
       )
    => document
    -> VirtualElement
    -> MomentIO RenderedVirtualElement
renderVirtualElement document velem = do
    Just el <- document `createElement` (Just (textToJSString (virtualElementTag velem)))
    -- Show the unique identifier of the virtual element in the markup.
    let uniq = virtualElementUnique velem
    setAttribute el (textToJSString "virtual_id") (textToJSString (T.pack (show (hashUnique uniq))))
    reactimateProperties el (virtualElementProperties velem)
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
                      pure ev

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
reactimateChildren document parent children = do
    currentlyRendered <- liftIO $ newIORef []
    -- We don't update until immediately after the children change, to
    -- ensure that the whole subtree in there has been updated.
    -- This has no effect on the immediate part; it's still rendered
    -- immediately by sequenceReactimate.
    -- Careful to choose lag' and not lag, as we do want to force the
    -- children event.
    let delayed = lag children
    sequenceCommute (update parent currentlyRendered <$> delayed)
    return ()
  where
    -- A stupid, minimally efficient diff: remove all old, add all new.
    -- We do one check: if everything in the list is JavaScript equal to
    -- its counterpart in the other, we can do nothing.
    diff :: [RenderedNode]
         -> [VirtualNode]
         -> ([RenderedNode], [VirtualNode])
    diff old new =
        if getAll (mconcat (All (length old == length new) : zipWith (\x y -> All (nodeRenderedFrom x y)) old new))
        then ([], [])
        else (old, new)

    update
        :: parent
        -> IORef [RenderedNode]
        -> [VirtualNode]
        -> MomentIO ()
    update parent current new = do
        currentRendered <- liftIO $ readIORef current
        let (toRemove, toAdd) = diff currentRendered new
        forM_ toRemove (removeChild parent . Just . renderedNode)
        newRendered <- forM toAdd  (renderVirtualNode document)
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

maybeRender
    :: ( IsElement parent
       , IsDocument document
       )
    => document
    -> parent
    -> Maybe RenderedVirtualElement
    -> VirtualElement
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
    -> VirtualElement
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
