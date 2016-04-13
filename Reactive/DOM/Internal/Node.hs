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
import Control.Monad.Trans.Reader
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
import GHCJS.DOM.Types hiding (Event, Element, Document, ClientRect)
import qualified GHCJS.DOM.Types as DOM.Types
import GHCJS.DOM.Element hiding (Element, getStyle, getAttributes, ClientRect)
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.ClientRect as ClientRect
import GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Window as Window
import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Document as Document hiding (Document)
import GHCJS.DOM.EventM hiding (event, mouseClientX, mouseClientY)
import qualified GHCJS.DOM.EventM as EventM
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.HTMLInputElement (getValue)
import GHCJS.DOM.MouseEvent (getDataTransfer, getClientX, getClientY, getOffsetX, getOffsetY)
import GHCJS.DOM.DataTransfer
import GHCJS.DOM.TouchEvent (getChangedTouches)
import GHCJS.DOM.Touch (Touch)
import qualified GHCJS.DOM.Touch as Touch
import GHCJS.DOM.TouchList (TouchList)
import qualified GHCJS.DOM.TouchList as TouchList
import GHCJS.DOM.KeyboardEvent (getKeyIdentifier)
import GHCJS.DOM.UIEvent (getKeyCode)
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HM
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Internal.Tag
import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Internal.ChildrenContainer
-- For setTimeout functionality; creating events which fire once after at
-- least a certain amount of time.
import Control.Concurrent (forkIO, threadDelay)
import System.IO.Unsafe
import Unsafe.Coerce

-- | Some UI-global data.
--
--   TODO give a way to unbind everything bound by makeUIEnvironment.
data UIEnvironment = UIEnvironment {
      uiEnvironmentWindowMousemove :: Event MouseEventData
    , uiEnvironmentWindowMouseup :: Event MouseEventData
    , uiEnvironmentWindowKeydown :: Event KeydownData
    , uiEnvironmentWindowKeypress :: Event KeypressData
    , uiEnvironmentWindowKeyup :: Event KeyupData
    , uiEnvironmentAnimationFrame :: Event Double
    -- ^ Value is a decimal timestamp in milliseconds with microsecond precision
    , uiEnvironmentRequestAnimationFrame :: MomentIO (Event Double)
    -- ^ Take one animation frame. The Event will fire precisely once, on the
    -- next frame.
    }

makeUIEnvironment :: Window -> MomentIO UIEnvironment
makeUIEnvironment window = do
    mousemove <- windowMousemove window
    mouseup <- windowMouseup window
    keydown <- windowKeydown window
    keypress <- windowKeypress window
    keyup <- windowKeyup window
    animationFrame <- windowAnimationFrame window
    pure $ UIEnvironment mousemove
                         mouseup
                         keydown
                         keypress
                         keyup
                         animationFrame
                         (windowRequestAnimationFrame window)
  where
    windowMousemove :: Window -> MomentIO (Event MouseEventData)
    windowMousemove window = do
        (ev, fire) <- newEvent
        _ <- liftIO $ bindElementEvent Mousemove window fire
        pure ev
    windowMouseup :: Window -> MomentIO (Event MouseEventData)
    windowMouseup window = do
        (ev, fire) <- newEvent
        _ <- liftIO $ bindElementEvent Mouseup window fire
        pure ev
    windowKeydown :: Window -> MomentIO (Event KeydownData)
    windowKeydown window = do
        (ev, fire) <- newEvent
        _ <- liftIO $ bindElementEvent Keydown window fire
        pure ev
    windowKeypress :: Window -> MomentIO (Event KeypressData)
    windowKeypress window = do
        (ev, fire) <- newEvent
        _ <- liftIO $ bindElementEvent Keypress window fire
        pure ev
    windowKeyup :: Window -> MomentIO (Event KeyupData)
    windowKeyup window = do
        (ev, fire) <- newEvent
        _ <- liftIO $ bindElementEvent Keyup window fire
        pure ev
    windowAnimationFrame :: Window -> MomentIO (Event (Double))
    windowAnimationFrame window = do
        -- The animation frame is bound again and again; the event will fire
        -- on every frame for the life of the web page.
        -- This *should* be no problem, in theory. If there's no work to do
        -- as a result of the event, then reactive-banana ought to consume
        -- virtually no resources. But we shall see about this...
        --
        -- TBD how to implement canceling of the animation frame? Store the
        -- id in a behavior, grab it, cancel it?
        (ev, fire) <- newEvent
        rec { let cb = \ts -> do
                      Window.requestAnimationFrame window (Just rafCb)
                      fire ts
            ; rafCb <- newRequestAnimationFrameCallback cb
        }
        _ <- Window.requestAnimationFrame window (Just rafCb)
        pure ev
    windowRequestAnimationFrame :: Window -> MomentIO (Event Double)
    windowRequestAnimationFrame window = do
        (ev, fire) <- newEvent
        rafCb <- newRequestAnimationFrameCallback fire
        _ <- Window.requestAnimationFrame window (Just rafCb)
        pure ev


-- | A monad in which DOM elements are described. It's parameterized by a
--   Symbol which presumably is one of the W3C standard tag names.
newtype ElementBuilder (tag :: Symbol) t = ElementBuilder {
      runElementBuilder
          :: ReaderT (UIEnvironment)
             (StateT (UIState tag)
             (WriterT (Dual (Endo ElementSchema))
             MomentIO))
             t
    }

deriving instance Functor (ElementBuilder tag)
deriving instance Applicative (ElementBuilder tag)
deriving instance Monad (ElementBuilder tag)
deriving instance MonadFix (ElementBuilder tag)
instance MonadMoment (ElementBuilder tag) where
    liftMoment = ElementBuilder . lift . lift . lift . liftMoment

-- | Run an ElementBuilder.
buildElement
    :: forall tag t .
       ( )
    => ElementBuilder tag t
    -> UIEnvironment
    -> UIState tag
    -> MomentIO (t, UIState tag, Dual (Endo ElementSchema))
buildElement extrinsic env el = do
    ((t, el'), eschemaDualEndo)
        <- runWriterT (runStateT (runReaderT (runElementBuilder extrinsic) env) el)
    pure (t, el', eschemaDualEndo)

liftMomentIO :: MomentIO t -> ElementBuilder tag t
liftMomentIO = ElementBuilder . lift . lift . lift

newtype Child t = Child {
      runChild :: (t, SomeNode, Event (IO ()))
    }

instance Eq (Child t) where
    Child (_, n, _) == Child (_, m, _) = n == m

instance Ord (Child t) where
    Child (_, n, _) `compare` Child (_, m, _) = n `compare` m

childData :: Child t -> t
childData = (\(x,_,_) -> x) . runChild

childNode :: forall t . Child t -> SomeNode
childNode = (\(_,x,_) -> x) . runChild

-- | The effects which will update the child node (local DOM mutations like
--   chaning children or style). Do not export; there is no use for it outside
--   of this module.
childUpdates :: forall t . Child t -> Event (IO ())
childUpdates = (\(_,_,x) -> x) . runChild

data SetChild t where
    SetWidgetChild :: Either (Child t) (UI t) -> SetChild t
    SetTextChild :: T.Text -> SetChild ()

-- | Two SetChild t values are equal if they hold the same existing child (same
--   DOM node under JS equality).
instance Eq (SetChild t) where
    left == right = case (left, right) of
        (SetWidgetChild (Left n), SetWidgetChild (Left m)) -> n == m
        _ -> False

newChild :: UI t -> SetChild t
newChild = SetWidgetChild . Right

existingChild :: Child t -> SetChild t
existingChild = SetWidgetChild . Left

textChild :: T.Text -> SetChild ()
textChild = SetTextChild

runSetChild :: SetChild r -> UIEnvironment -> Document -> Compose MomentIO Child r
runSetChild child env document = Compose $ case child of
    SetTextChild txt -> do
        textNode <- makeText document txt
        node <- liftIO $ someText textNode
        pure (Child ((), node, never))
    SetWidgetChild (Left existing) ->
        pure existing
    SetWidgetChild (Right ui) -> do
        (t, el, ev) <- buildUI env document ui
        node <- liftIO $ someElement el
        pure (Child (t, node, ev))

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
    (lift . lift . lift) (stepper (viewChildrenInitial viewChildren) (viewChildrenEvent viewChildren))

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
    => UIEnvironment
    -> Document
    -> Children f
    -> MomentIO ( ViewChildren f
                , Sequence [ChildrenMutation SomeNode SomeNode]
                , Event (IO ())
                )
makeChildrenInput env document childrenOutput = mdo

    firstChildren :: f Child
        <- functorCommute . functorTrans (\x -> runSetChild x env document) $ childrenInitial childrenOutput

    -- childrenChanges childrenOutput :: Event [Change f SetChild]
    -- flip runSetChild document :: SetChild t -> Compose MomentIO SetChild t
    -- functorTrans (flip runSetChild document)
    --     :: Change f SetChild -> Change f (Compose MomentIO SetChild)
    -- functorCommute . (functorTrans (flip runSetChild document))
    --     :: Change f SetChild -> MomentIO (Change f Child)
    -- traverse (functorCommute . (functorTrans (flip runSetChild document)))
    --     :: [Change f SetChild -> MomentIO [Change f Child]
    sequencedChanges :: Event [Change f Child]
        <- execute (traverse (functorCommute . (functorTrans (\x -> runSetChild x env document))) <$> childrenChanges childrenOutput)

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

    -- Is this derivation correct?
    let firstUpdates :: Event (IO ())
        firstUpdates = unionUpdates . childrenContainerList childUpdates $ firstChildren
    let restUpdates :: Event (Event (IO ()))
        restUpdates = unionUpdates . childrenContainerList childUpdates . fst <$> restChildren
    allUpdates :: Event (IO ())
        <- sequenceSwitchE (firstUpdates |> restUpdates)

    -- This give some indication of which DOM mutations are happening.
    -- Comment it out.
    -- reactimate (Prelude.print <$> restMutations)

    pure ( ViewChildren firstChildren sequencedChanges (fst <$> restChildren)
         , mutationSequence
         , allUpdates
         )
  where

    unionUpdates :: [Event (IO ())] -> Event (IO ())
    unionUpdates = foldr (unionWith (>>)) never

buildWidget
    :: forall tag s t .
       ( W3CTag tag )
    => Widget tag s t
    -> s
    -> UIEnvironment
    -> Document
    -> MomentIO (t, Element, Event (IO ()))
buildWidget (Widget l mk r) s env document = mdo
    Just el <- document `createElement` Just (w3cTagName (Tag :: Tag tag))
    let roelem = UIState el M.empty []
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
                                ; (childrenInput, mutationSequence, childrenUpdates)
                                      <- liftMomentIO $ makeChildrenInput env document childrenOutput
                                }
                          ; (passback, mt) <- r passforward t'
                          }
                      -- This is crucial: the output computation runs outside
                      -- of the recursive knot above. Without this, it would
                      -- be impossible *ever* to be non-lazy in the output
                      -- function with respect to the output of the intrinsic
                      -- part.
                      t <- mt
                      pure (t, mutationSequence, childrenUpdates)
    ((t, mutationSequence, childrenUpdates), roelem', eschemaDualEndo)
        <- buildElement ebuilder env roelem
    let eschema = appEndo (getDual eschemaDualEndo)
                $ emptySchema
    let seqncChildrenIO = reactimateChildren el mutationSequence
    styleseqnc <- buildSequence (elementSchemaStyle eschema)
    seqncSchemaIO <- runElementSchema eschema document el
    let seqncLocalIO = sequenceUnion (>>) seqncChildrenIO seqncSchemaIO
    let initialLocalIO = sequenceFirst seqncLocalIO
    let evLocalIO = sequenceEvent seqncLocalIO
    let evIO = unionWith (>>) evLocalIO childrenUpdates
    liftIO (wireEvents roelem')
    liftIOLater (wireTimeouts roelem')
    liftIO initialLocalIO
    pure (t, el, evIO)

buildUI :: UIEnvironment -> Document -> UI t -> MomentIO (t, Element, Event (IO ()))
buildUI env doc (ClosedWidget _ widget) = buildWidget widget () env doc

-- | Produce a sequence of effects which keep the children of some element up
--   to date according to a sequence of mutations.
reactimateChildren
    :: Element
    -> Sequence [ChildrenMutation SomeNode SomeNode]
    -> Sequence (IO ())
reactimateChildren parent seqnc = flip runChildrenMutationsIO parent <$> seqnc

-- | Renders a sequence of UIs to a document under a given parent.
reactiveDom
    :: ( IsNode parent
       )
    => Document
    -> parent
    -> Sequence (UI t)
    -> MomentIO (Sequence t)
reactiveDom document parent seqncUi = do
    Just window <- getDefaultView document
    env <- makeUIEnvironment window
    -- Build the UIs.
    seqncBuilt :: Sequence (t, Element, Event (IO ()))
        <- sequenceCommute (buildUI env document <$> seqncUi)
    -- Output is the first component.
    let seqncT :: Sequence t
        seqncT = (\(x,_,_) -> x) <$> seqncBuilt
    -- From here we derive the element to place under the parent.
    let seqncElem :: Sequence Element
        seqncElem = (\(_,x,_) -> x) <$> seqncBuilt
    -- From here we derive the effects which must be run in order to make
    -- the element appear reactive.
    let seqncUpdate :: Sequence (Event (IO ()))
        seqncUpdate = (\(_,_,x) -> x) <$> seqncBuilt
    let elFirst = sequenceFirst seqncElem
    evElChanges :: Event (Element, Element)
        <- sequenceChanges seqncElem
    -- Render the first, and swap on changes.
    liftIO (renderIt elFirst)
    reactimate (swapIt <$> evElChanges)
    -- Run the updates.
    evUpdate <- sequenceSwitchE seqncUpdate
    reactimate evUpdate
    -- The two reactimates found here are the only reactimates performed by
    -- use of the reactive-dom package. This is important, for there's no way
    -- to "un-reactimate", so doing local reactimates would result in an
    -- accumulation of outputs in the network with no upper bound.
    pure seqncT
  where
    renderIt :: Element -> IO ()
    renderIt el = parent `appendChild` (Just el) >> pure ()
    unrenderIt :: Element -> IO ()
    unrenderIt el = parent `removeChild` (Just el) >> pure ()
    swapIt :: (Element, Element) -> IO ()
    swapIt (old, new) = unrenderIt old >> renderIt new

-- | For use in ElementBuilder state. Gives access to certain features of
--   a DOM element and holds deferred event bindings.
--
--   TBD take away the tag parameter? Not used anymore, but doesn't hurt to
--   leave it.
data UIState (tag :: Symbol) = UIState {
      getUIStateElement :: Element
    , getUIStateEvents :: M.Map (DOMString, Bool) EventBinding
    , getUIStateTimeouts :: [IO ()]
    }

wireTimeouts :: UIState tag -> IO ()
wireTimeouts uistate = sequence_ (fmap forkIO (getUIStateTimeouts uistate))

timeout :: Int -> ElementBuilder tag (Event ())
timeout i = ElementBuilder $ do
    (ev, fire) <- (lift . lift . lift) newEvent
    let fireThread = threadDelay i >> putStrLn "Firing timeout" >> fire ()
    uistate <- lift get
    let timeouts = getUIStateTimeouts uistate
    lift (put (uistate { getUIStateTimeouts = fireThread : timeouts }))
    pure ev

data EventBinding where
    EventBinding
        :: forall e .
           ( ElementEvent e Element )
        => e
        -> Event (EventData e Element)
        -> EventM Element (DOMEvent e Element) Bool
        -> (EventData e Element -> IO ())
        -> EventBinding

-- | TODO clean this up. Many unused parameters.
runEventBinding
    :: Element
    -> (DOMString, Bool)
    -> EventBinding
    -> IO ()
runEventBinding el _ (EventBinding e _ _ fire) = do
    bindElementEvent e el fire
    pure ()

wireEvents :: UIState tag -> IO ()
wireEvents (UIState el eventMap _) =
    M.foldWithKey (\k v rest -> runEventBinding el k v >> rest) (pure ()) eventMap

elementEvent
    :: forall event tag .
       ElementEvent event Element
    => event
    -> UIState tag
    -> Bool -- True if it should fire even when bubbled.
    -> MomentIO (Event (EventData event Element), UIState tag)
elementEvent event roelement fireWhenBubbled = case existingBinding of
    -- unsafeCoerce is OK. We know ev must have the right type, because the
    -- only way it could have come to be here is if it was inserted for
    -- the same key, and the key is determined by the type @event@.
    Just (EventBinding _ ev _ _) -> pure (unsafeCoerce ev, roelement)
    Nothing -> do
        (ev, fire) <- newEvent
        let binding = EventBinding event ev eventM fire
        let newEvents = M.alter (const (Just binding)) (key, fireWhenBubbled) (getUIStateEvents roelement)
        pure (ev, roelement { getUIStateEvents = newEvents })
  where
    existingBinding = M.lookup (key, fireWhenBubbled) (getUIStateEvents roelement)
    EventName key = eventName (Proxy :: Proxy event) (Proxy :: Proxy Element)
    eventM :: EventM Element (DOMEvent event Element) Bool
    eventM = specialHandler (Proxy :: Proxy event) (Proxy :: Proxy Element)

{-
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
    lift . lift .lift $ (execute (liftIO . (<*>) io . pure <$> ev))
-}

data ClientRect = ClientRect {
      clientRectTop :: Float
    , clientRectLeft :: Float
    , clientRectBottom :: Float
    , clientRectRight :: Float
    , clientRectWidth :: Float
    , clientRectHeight :: Float
    }

deriving instance Show ClientRect

clientRect :: ElementBuilder tag (Behavior ClientRect)
clientRect = ElementBuilder $ do
    roelem <- lift get
    let el = getUIStateElement roelem
    let getIt = do Just rect <- getBoundingClientRect el
                   ClientRect <$> ClientRect.getTop rect
                              <*> ClientRect.getLeft rect
                              <*> ClientRect.getBottom rect
                              <*> ClientRect.getRight rect
                              <*> ClientRect.getWidth rect
                              <*> ClientRect.getHeight rect
    (lift . lift . lift) (fromPull getIt)

scrollHeight :: ElementBuilder tag (Behavior Int)
scrollHeight = ElementBuilder $ do
    el <- lift get
    (lift . lift . lift) (fromPull (getScrollHeight (getUIStateElement el)))

scrollWidth :: ElementBuilder tag (Behavior Int)
scrollWidth = ElementBuilder $ do
    el <- lift get
    (lift . lift . lift) (fromPull (getScrollWidth (getUIStateElement el)))

windowMousemove :: ElementBuilder tag (Event MouseEventData)
windowMousemove = uiEnvironmentWindowMousemove <$> ElementBuilder ask

windowMouseup :: ElementBuilder tag (Event MouseEventData)
windowMouseup = uiEnvironmentWindowMouseup <$> ElementBuilder ask

windowKeydown :: ElementBuilder tag (Event KeydownData)
windowKeydown = uiEnvironmentWindowKeydown <$> ElementBuilder ask

windowKeypress :: ElementBuilder tag (Event KeypressData)
windowKeypress = uiEnvironmentWindowKeypress <$> ElementBuilder ask

windowKeyup :: ElementBuilder tag (Event KeyupData)
windowKeyup = uiEnvironmentWindowKeyup <$> ElementBuilder ask

-- | Event animation frame.
animationFrame :: ElementBuilder tag (Event Double)
animationFrame = uiEnvironmentAnimationFrame <$> ElementBuilder ask

-- | Take the next animation frame. The event will fire once and only once.
requestAnimationFrame :: ElementBuilder tag (Event Double)
requestAnimationFrame = do
    env <- ElementBuilder ask
    let request = uiEnvironmentRequestAnimationFrame env 
    liftMomentIO request

-- | By using event, the tag of the ElementBuilder is constrained, so that if
--   used as a modifier in a Widget, the Widget can no longer be open.
--
event
    :: forall event tag . 
       ( W3CTag tag
       -- TODO be more precise than W3CTag. Maybe  ( HasEvent event tag )
       , ElementEvent event Element
       -- Must be an event which works on Element.
       )
    => event
    -> ElementBuilder tag (Event (EventData event Element))
event ev = ElementBuilder $ do
    roelem <- lift get
    (ev, roelem') <- (lift . lift . lift) (elementEvent ev roelem False)
    lift (put roelem')
    pure ev

class
    ( PToJSVal (DOMEvent event target)
    , IsEvent (DOMEvent event target)
    , IsEventTarget target
    ) => ElementEvent event target
  where
    type EventData event target :: *
    type DOMEvent event target :: *
    eventName :: Proxy event -> Proxy target -> EventName target (DOMEvent event target)
    eventData :: event -> target -> DOMEvent event target -> IO (EventData event target)
    -- In case you need to do special effects in the event handler.
    -- Motivating case: the Submit event *always* prevents the default action.
    -- Without this, the page will reload.
    -- Default special handler: do nothing (return False) if the event is bubbled.
    specialHandler :: Proxy event -> Proxy target -> EventM target (DOMEvent event target) Bool
    specialHandler e tag = not <$> isBubbled e tag

isBubbled
    :: ElementEvent event target
    => Proxy event
    -> Proxy target
    -> EventM event (DOMEvent event target) Bool
isBubbled _ _ = do
    domEvent <- EventM.event
    -- The "bubbled" property is set in bindElementEvent.
    mbubbled <- liftIO $ getJSProperty domEvent "bubbled"
    pure $ case mbubbled of
        Just "true" -> True
        _ -> False

-- | Use an ElementEvent instance to bind a callback (banana event handler
--   trigger) to an event at some particular target.
--
--   Output is an IO to unbind it.
bindElementEvent
    :: forall event target .
       ( ElementEvent event target
       )
    => event
    -> target
    -> (EventData event target -> IO ())
    -> IO (IO ())
bindElementEvent event target fire = do
    let action = do
            continue <- eventM
            if not continue
            then pure ()
            else do
                  domEvent <- EventM.event
                  d <- liftIO (eventData event target domEvent)
                  liftIO $ setJSProperty domEvent "bubbled" (Just "true")
                  liftIO $ fire d
                  pure ()
    on target (eventName proxyEvent proxyTarget) action
  where
    proxyEvent :: Proxy event
    proxyEvent = Proxy
    proxyTarget :: Proxy target
    proxyTarget = Proxy
    eventM :: EventM target (DOMEvent event target) Bool
    eventM = specialHandler proxyEvent proxyTarget

data MouseEventData = MouseEventData {
      mouseClientX :: Int
    , mouseClientY :: Int
    , mouseOffsetX :: Int
    , mouseOffsetY :: Int
    }

data Click = Click
instance ElementEvent Click Element where
    type EventData Click Element = MouseEventData
    type DOMEvent Click Element = MouseEvent
    eventName _ _ = Element.click
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev
instance ElementEvent Click Window where
    type EventData Click Window = MouseEventData
    type DOMEvent Click Window = MouseEvent
    eventName _ _ = Window.click
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev

data Mousedown = Mousedown
instance ElementEvent Mousedown Element where
    type EventData Mousedown Element = MouseEventData
    type DOMEvent Mousedown Element = MouseEvent
    eventName _ _ = Element.mouseDown
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev
instance ElementEvent Mousedown Window where
    type EventData Mousedown Window = MouseEventData
    type DOMEvent Mousedown Window = MouseEvent
    eventName _ _ = Window.mouseDown
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev

data Mouseup = Mouseup
instance ElementEvent Mouseup Element where
    type EventData Mouseup Element = MouseEventData
    type DOMEvent Mouseup Element = MouseEvent
    eventName _ _ = Element.mouseUp
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev
instance ElementEvent Mouseup Window where
    type EventData Mouseup Window = MouseEventData
    type DOMEvent Mouseup Window = MouseEvent
    eventName _ _ = Window.mouseUp
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev

data Mousemove = Mousemove
instance ElementEvent Mousemove Element where
    type EventData Mousemove Element = MouseEventData
    type DOMEvent Mousemove Element = MouseEvent
    eventName _ _ = Element.mouseMove
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev
instance ElementEvent Mousemove Window where
    type EventData Mousemove Window = MouseEventData
    type DOMEvent Mousemove Window = MouseEvent
    eventName _ _ = Window.mouseMove
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev



data Mouseenter = Mouseenter
instance ElementEvent Mouseenter Element where
    type EventData Mouseenter Element = MouseEventData
    type DOMEvent Mouseenter Element = MouseEvent
    eventName _ _ = Element.mouseEnter
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev
instance ElementEvent Mouseenter Window where
    type EventData Mouseenter Window = MouseEventData
    type DOMEvent Mouseenter Window = MouseEvent
    eventName _ _ = Window.mouseEnter
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev

data Mouseleave = Mouseleave
instance ElementEvent Mouseleave Element where
    type EventData Mouseleave Element = MouseEventData
    type DOMEvent Mouseleave Element = MouseEvent
    eventName _ _ = Element.mouseLeave
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev
instance ElementEvent Mouseleave Window where
    type EventData Mouseleave Window = MouseEventData
    type DOMEvent Mouseleave Window = MouseEvent
    eventName _ _ = Window.mouseLeave
    eventData _ _ ev = MouseEventData <$> getClientX ev
                                      <*> getClientY ev
                                      <*> getOffsetX ev
                                      <*> getOffsetY ev

data Keypress = Keypress
data KeypressData = KeypressData {
      keypressDataKeycode :: Int
    , keypressDataKey :: T.Text
    }
instance ElementEvent Keypress Element where
    type EventData Keypress Element = KeypressData
    type DOMEvent Keypress Element = KeyboardEvent
    eventName _ _ = Element.keyPress
    eventData _ _ ev = KeypressData <$> getKeyCode ev <*> getKey ev
instance ElementEvent Keypress Window where
    type EventData Keypress Window = KeypressData
    type DOMEvent Keypress Window = KeyboardEvent
    eventName _ _ = Window.keyPress
    eventData _ _ ev = KeypressData <$> getKeyCode ev <*> getKey ev

data Keydown = Keydown
data KeydownData = KeydownData {
      keydownDataKeycode :: Int
    , keydownDataKey :: T.Text
    }
instance ElementEvent Keydown Element where
    type EventData Keydown Element = KeydownData
    type DOMEvent Keydown Element = KeyboardEvent
    eventName _ _ = Element.keyDown
    eventData _ _ ev = KeydownData <$> getKeyCode ev <*> getKey ev
instance ElementEvent Keydown Window where
    type EventData Keydown Window = KeydownData
    type DOMEvent Keydown Window = KeyboardEvent
    eventName _ _ = Window.keyDown
    eventData _ _ ev = KeydownData <$> getKeyCode ev <*> getKey ev

data Keyup = Keyup
data KeyupData = KeyupData {
      keyupDataKeycode :: Int
    , keyupDataKey :: T.Text
    }
instance ElementEvent Keyup Element where
    type EventData Keyup Element = KeyupData
    type DOMEvent Keyup Element = KeyboardEvent
    eventName _ _ = Element.keyUp
    eventData _ _ ev = KeyupData <$> getKeyCode ev <*> getKey ev
instance ElementEvent Keyup Window where
    type EventData Keyup Window = KeyupData
    type DOMEvent Keyup Window = KeyboardEvent
    eventName _ _ = Window.keyUp
    eventData _ _ ev = KeyupData <$> getKeyCode ev <*> getKey ev

-- Seems ghcjs-dom does not offer a getter for the 'key' property of
-- KeyboardEvent. It's a rather cutting edge feature, which Safari does not
-- implement, so we fall back to keyIdentifier
foreign import javascript safe
    "$1[\"key\"]"
    js_getKey :: UIEvent -> IO (Nullable JSString)
getKeyUnsafe :: KeyboardEvent -> IO (Maybe T.Text)
getKeyUnsafe ev = fmap textFromJSString . nullableToMaybe <$> js_getKey (toUIEvent ev)

getKey :: KeyboardEvent -> IO T.Text
getKey ev = do
    maybeKey <- getKeyUnsafe ev
    case maybeKey of
        Nothing -> getKeyIdentifier ev
        Just x -> pure x

data Submit = Submit
instance ElementEvent Submit Element where
    type EventData Submit Element = ()
    type DOMEvent Submit Element = DOM.Types.Event
    eventName _ _ = Element.submit
    eventData _ _ _ = pure ()
    specialHandler e t = preventDefault >> (not <$> isBubbled e t)

data Input = Input
instance ElementEvent Input Element where
    type EventData Input Element = T.Text
    type DOMEvent Input Element = DOM.Types.Event
    eventName _ _ = Element.input
    eventData _ el _ = maybe "" id <$> getValue (castToHTMLInputElement el)

data Scroll = Scroll
data ScrollData = ScrollData {
      scrollDataTop :: Int
    , scrollDataLeft :: Int
    }
instance ElementEvent Scroll Element where
    type EventData Scroll Element = ScrollData
    type DOMEvent Scroll Element = DOM.Types.UIEvent
    eventName _ _ = Element.scroll
    eventData _ el _ = ScrollData <$> getScrollTop el <*> getScrollLeft el

{-
data Transitioned
instance W3CTag tag => ElementEvent Transitioned tag where
    type EventData Transitioned = ()
    type DOMEvent Transitioned = DOM.Types.TransitionEvent
    eventName _ _ = unsafeEventName (toJSString ("transitioned" :: T.Text))
    eventData _ _ _ _ = pure ()
    elementEventTarget _ _ = pure
-}

{-
data Pull = Pull
data PullData = PullData {
      dragDataClientX :: Int
    , dragDataClientY :: Int
    }
instance W3CTag tag => ElementEvent Drag tag where
    type EventData Drag = DragData
    type DOMEvent Drag = DOM.Types.MouseEvent
    type ElementEventTarget Drag = Document
    eventName _ _ = Document.dragOver
    eventData _ _ el ev = do
        x <- getClientX ev
        y <- getClientY ev
        putStrLn (show x ++ " : " ++ show y)
        pure (DragData x y)
        --DragData <$> getClientX ev <*> getClientY ev
    elementEventTarget _ _ el = do
        putStrLn "Drag event binding 1"
        Just doc <- getOwnerDocument el
        putStrLn "Drag event binding 2"
        pure doc
    -- Always fire, even if the event is bubbled.
    specialHandler _ _ = pure True

data DragStart = DragStart Bool
data DragStartData = DragStartData {
      dragstartDataClientX :: Int
    , dragstartDataClientY :: Int
    }
instance W3CTag tag => ElementEvent DragStart tag where
    type EventData DragStart = DragStartData
    type DOMEvent DragStart = DOM.Types.MouseEvent
    eventName _ _ = Element.dragStart
    eventData (DragStart showDragGhost) _ el ev = do
        firefoxCompatibility ev
        when (not showDragGhost) (disableDragGhost el ev)
        DragStartData <$> getClientX ev <*> getClientY ev
      where
        -- Firefox won't start a drag unless you set some data.
        -- Firefox also won't report mouse position on drag events, which is
        -- why we must bind on the document dragover in order to retrieve them.
        --
        -- NB I cannot figure out how to make firefox stop showing an ugly
        -- "helpful" drag and drop visualization. Sure, we get rid of the drag
        -- ghost, but it'll still show a file icon or some other crap next to
        -- your cursor. Only way around this I can think of is to roll out our
        -- own drag and drop via mousemove bindings on the window.
        -- 
        -- And indeed this is what we must do. Let's make the pull API like
        -- we have in JS widgets.
        --
        --   pull :: ElementBuilder tag (Event (Event (Int, Int)))
        --
        -- That'll be great. We shall require
        --
        --   1. an event binding on the window.
        --   2. an event binding on the element's mousedown.
        --   3. an event binding on the window's mouseup.
        --
        -- Right, use 2 and 3 to make a gate: True between mousedown and
        -- mouseup (stepper False (unionWith const mouseup mousedown)).
        -- Put the window's mousemove event behind that gate.
        -- OK No problem. Let me just verify that firefox will actually support
        -- this. Indeed it does, to my great surprise.
        firefoxCompatibility ev = do
            Just dt <- getDataTransfer ev
            setData dt ("text/plain" :: T.Text) ("" :: T.Text)
        disableDragGhost el ev = do
            Just dt <- getDataTransfer ev
            Just doc <- getOwnerDocument el
            Just emptyThing <- doc `createElement` (Just ("div" :: T.Text))
            setDragImage dt (Just emptyThing) 0 0
            setEffectAllowed dt ("none" :: T.Text)
            pure ()
    specialHandler e tag = not <$> isBubbled e tag
    elementEventTarget _ _ = pure
-}

data TouchStart = TouchStart
data TouchStartData = TouchStartData {
      touchStartChangedTouches :: HM.HashMap Int (Int, Int)
    }
instance ElementEvent TouchStart Element where
    type EventData TouchStart Element = TouchStartData
    type DOMEvent TouchStart Element = DOM.Types.TouchEvent
    eventName _ _ = Element.touchStart
    eventData TouchStart _ ev = do
        Just cts <- getChangedTouches ev
        hm <- elimTouchList cts
        pure (TouchStartData hm)

data TouchMove = TouchMove
data TouchMoveData = TouchMoveData {
      touchMoveChangedTouches :: HM.HashMap Int (Int, Int)
    }
instance ElementEvent TouchMove Element where
    type EventData TouchMove Element = TouchMoveData
    type DOMEvent TouchMove Element = DOM.Types.TouchEvent
    eventName _ _ = Element.touchMove
    eventData TouchMove _ ev = do
        Just cts <- getChangedTouches ev
        hm <- elimTouchList cts
        pure (TouchMoveData hm)
    -- TODO should be optional, no?
    specialHandler e t = preventDefault >> (not <$> isBubbled e t)

-- | Make a hashmap giving client x/y coordinates of each touch, eliminating
--   the TouchList (and its IO-only interface).
elimTouchList :: TouchList -> IO (HM.HashMap Int (Int, Int))
elimTouchList tl = do
    len <- TouchList.getLength tl
    assocs <- forM [0..len] $ \i -> do
        mtouch <- TouchList.item tl i
        case mtouch of
            Nothing -> pure Nothing
            Just touch -> do
                x <- Touch.getClientX touch
                y <- Touch.getClientY touch
                pure (Just (fromIntegral i, (x, y)))
    pure (HM.fromList (mapMaybe id assocs))

-- | Make something pullable using mouse or touch
--   The event you get fires whenever a user starts to pull on something.
--   It gives an event which fires whenever a pull happens and it contains the
--   deltas of that drag (incremental; change since the last time it fired).
--   From this the deltas of the entire drag can be derived.
--   Touch events used only when there's precisely one touch.
touchPull :: W3CTag tag => Modifier tag () (Event (Event (Int, Int)))
touchPull = modifier $ \_ -> do

    -- Set up the mouse-based pull event.
    -- We use the window's mousemove event to give the mouse position, the
    -- window's mouseup event to determine when it's finished (works even if
    -- the mouse leaves the browser window), and of course the element's
    -- mousedown event to determine when to start the pull.
    evMousedown <- event Mousedown
    let evPullStartCoords = (\x -> (mouseClientX x, mouseClientY x)) <$> evMousedown
    env <- ElementBuilder ask
    let getClientCoords :: MouseEventData -> (Int, Int)
        getClientCoords md = (mouseClientX md, mouseClientY md)
    let wmm = getClientCoords <$> uiEnvironmentWindowMousemove env
    let wmu = getClientCoords <$> uiEnvironmentWindowMouseup env
    -- Must know when to actually fire the pull events: only between element
    -- mousedown and window mouseup.
    let openGate = const True <$> evMousedown
    let closeGate = const False <$> wmu
    gate :: Behavior Bool
        <- stepper False (unionWith const openGate closeGate)
    let evPullCoords :: Event (Int, Int)
        evPullCoords = whenE gate wmm

    -- Set up the touch-based pull event. Easy compared to the mouse-based one,
    -- because W3C gives a somewhat reasonable element-centric way to do this.
    evTouchStart <- event TouchStart
    evTouchMove <- event TouchMove
    let evTouchStartCoords = filterJust (pickSingleTouchStart <$> evTouchStart)
    let evTouchMoveCoords = filterJust (pickSingleTouchMove <$> evTouchMove)

    let evMousePull = observeE (makePullEvent evPullCoords <$> evPullStartCoords)
    let evTouchPull = observeE (makePullEvent evTouchMoveCoords <$> evTouchStartCoords)

    pure (unionWith const evMousePull evTouchPull)
  where
    makeDelta (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
    -- Given an event which fires with mouse coordinates, and the initial
    -- mouse coordinates, produce an event which fires with incremental
    -- deltas.
    makePullEvent :: Event (Int, Int) -> (Int, Int) -> Moment (Event (Int, Int))
    makePullEvent evDragCoords initial = do
        beCoords <- stepper initial evDragCoords
        pure (makeDelta <$> beCoords <@> evDragCoords)
    -- Identify the touch start events which have a single touch point.
    pickSingleTouchStart :: TouchStartData -> Maybe (Int, Int)
    pickSingleTouchStart (TouchStartData hm) = case (HM.size hm, HM.lookup 0 hm) of
        (1, Just coords) -> Just coords
        _ -> Nothing
    -- Identify the touch move events which have a single touch point.
    pickSingleTouchMove :: TouchMoveData -> Maybe (Int, Int)
    pickSingleTouchMove (TouchMoveData hm) = case (HM.size hm, HM.lookup 0 hm) of
        (1, Just coords) -> Just coords
        _ -> Nothing

type Document = DOM.Types.Document
type Element = DOM.Types.Element

newtype IdentifiedMap k v = IdentifiedMap {
      runIdentifiedMap :: (M.Map k v, Unique)
    }

getIdentifiedMap :: IdentifiedMap k v -> M.Map k v
getIdentifiedMap = fst . runIdentifiedMap

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

instance Ord k => Semigroup (IdentifiedMap k v) where
    left <> right = makeIdentifiedMap (getIdentifiedMap left `mappend` getIdentifiedMap right)

instance Ord k => Monoid (IdentifiedMap k v) where
    mempty = makeIdentifiedMap mempty
    left `mappend` right = makeIdentifiedMap (getIdentifiedMap left `mappend` getIdentifiedMap right)

data Action t = Set t | Unset t | NoOp

runAction :: Eq t => Action t -> Endo [t]
runAction action = Endo $ case action of
    Set t -> \ts -> if elem t ts then ts else t : ts
    Unset t -> delete t
    NoOp -> id

newtype Style = Style { getStyle :: IdentifiedMap T.Text T.Text }
deriving instance Show Style
deriving instance Eq Style
deriving instance Semigroup Style
deriving instance Monoid Style

newtype Properties = Properties { getProperties :: IdentifiedMap T.Text T.Text }
deriving instance Show Properties
deriving instance Eq Properties
deriving instance Semigroup Properties
deriving instance Monoid Properties

newtype Attributes = Attributes { getAttributes :: IdentifiedMap T.Text T.Text }
deriving instance Show Attributes
deriving instance Eq Attributes
deriving instance Semigroup Attributes
deriving instance Monoid Attributes

{-# NOINLINE makeStyle #-}
makeStyle :: [(T.Text, T.Text)] -> Style
makeStyle = Style . makeIdentifiedMap . M.fromList

makeProperties :: [(T.Text, T.Text)] -> Properties
makeProperties = Properties . makeIdentifiedMap . M.fromList

makeAttributes :: [(T.Text, T.Text)] -> Attributes
makeAttributes = Attributes . makeIdentifiedMap . M.fromList

computeStyle :: [Style] -> Style
computeStyle = mconcat

computeProperties :: [Properties] -> Properties
computeProperties = mconcat

computeAttributes :: [Attributes] -> Attributes
computeAttributes = mconcat

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

-- | From an ElementSchema derive a sequence of effects which must be realized
--   in order for the element to respect the schema.
runElementSchema
    :: ( MonadMoment m )
    => ElementSchema
    -> Document
    -> Element
    -> m (Sequence (IO ()))
runElementSchema eschema document el = do
    propsSequence <- buildSequence (elementSchemaProperties eschema)
    attrsSequence <- buildSequence (elementSchemaAttributes eschema)
    styleSequence <- buildSequence (elementSchemaStyle eschema)
    postsSequence <- buildSequence (elementSchemaPostprocess eschema)
    propsSequenceIO <- reactimateProperties el propsSequence
    attrsSequenceIO <- reactimateAttributes el attrsSequence
    styleSequenceIO <- reactimateStyle el styleSequence
    let ppSequenceIO = reactimatePostprocess document el postsSequence
    let sequenceIO = sequenceUnion (>>) propsSequenceIO
                   $ sequenceUnion (>>) attrsSequenceIO
                   $ sequenceUnion (>>) styleSequenceIO
                                        ppSequenceIO
    pure sequenceIO

  where

    reactimatePostprocess
        :: Document
        -> Element
        -> Sequence Postprocess
        -> Sequence (IO ())
    reactimatePostprocess document element sequence =
        (\x -> runPostprocess x document element) <$> sequence

    reactimateProperties
        :: ( MonadMoment m )
        => Element
        -> Sequence [Properties]
        -> m (Sequence (IO ()))
    reactimateProperties element sequence = do
        let seqncFirst = sequenceFirst sequence
        let initial = mconcat seqncFirst
        let seqncEvent = sequenceEvent sequence
        let evProperties = mconcat <$> seqncEvent
        beProperties <- stepper initial evProperties
        let changes = (,) <$> beProperties <@> evProperties
        let changeProperties :: Properties -> Properties -> IO ()
            changeProperties old new = do
                let propsOld :: M.Map T.Text T.Text
                    propsOld = getIdentifiedMap (getProperties old)
                let propsNew :: M.Map T.Text T.Text
                    propsNew = getIdentifiedMap (getProperties new)
                removeProperties element propsOld
                addProperties element propsNew
                pure ()
        let initialIO = changeProperties mempty initial
        let restIO = uncurry changeProperties <$> changes
        pure (initialIO |> restIO)

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

    reactimateAttributes
        :: ( MonadMoment m )
        => Element
        -> Sequence [Attributes]
        -> m (Sequence (IO ()))
    reactimateAttributes element sequence = do
        let seqncFirst = sequenceFirst sequence
        let initial = mconcat seqncFirst
        let seqncEvent = sequenceEvent sequence
        let evAttributes = mconcat <$> seqncEvent
        beAttributes <- stepper initial evAttributes
        let changes = (,) <$> beAttributes <@> evAttributes
        let changeAttributes :: Attributes -> Attributes -> IO ()
            changeAttributes old new = do
                let attrsOld :: M.Map T.Text T.Text
                    attrsOld = getIdentifiedMap (getAttributes old)
                let attrsNew :: M.Map T.Text T.Text
                    attrsNew = getIdentifiedMap (getAttributes new)
                let (add, remove) = diffAttributes attrsOld attrsNew
                removeAttributes element remove
                addAttributes element add
                pure ()
        let initialIO = changeAttributes mempty initial
        let restIO = uncurry changeAttributes <$> changes
        pure (initialIO |> restIO)

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

    reactimateStyle
        :: ( MonadMoment m )
        => Element
        -> Sequence [Style]
        -> m (Sequence (IO ()))
    reactimateStyle element sequence = do
        let seqncFirst = sequenceFirst sequence
        let initial = mconcat seqncFirst
        let seqncEvent = sequenceEvent sequence
        let evStyle = mconcat <$> seqncEvent
        beStyle <- stepper initial evStyle
        let changes = (,) <$> beStyle <@> evStyle
        let changeStyle :: Style -> Style -> IO ()
            changeStyle old new = do
                let styleOld :: M.Map T.Text T.Text
                    styleOld = getIdentifiedMap (getStyle old)
                let styleNew :: M.Map T.Text T.Text
                    styleNew = getIdentifiedMap (getStyle new)
                let (add, remove) = diffStyle styleOld styleNew
                removeStyle element remove
                addStyle element add
                pure ()
        let initialIO = changeStyle mempty initial
        let restIO = uncurry changeStyle <$> changes
        pure (initialIO |> restIO)

    addStyle :: Element -> M.Map T.Text T.Text -> IO ()
    addStyle element style = do
        let styleList :: [(T.Text, Maybe T.Text, T.Text)]
            styleList = M.foldWithKey (\x y -> (:) (x, Just y, "")) [] style
        Just css <- Element.getStyle element
        forM_ styleList (\(x, y, z) -> setProperty css (textToJSString x) (textToJSString <$> y) (textToJSString z))

    removeStyle :: Element -> M.Map T.Text T.Text -> IO ()
    removeStyle element style = do
        let styleNames :: [T.Text]
            styleNames = M.keys style
        Just css <- Element.getStyle element
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
style s = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaStyle (fmap pure s)))))

style'
    :: W3CTag tag
    => Sequence [Action Style]
    -> ElementBuilder tag ()
style' s = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaStyle s))))

styleHover
    :: ( W3CTag tag
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
attributes a = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaAttributes (fmap pure a)))))

attributes'
    :: W3CTag tag
    => Sequence [Action Attributes]
    -> ElementBuilder tag ()
attributes' a = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaAttributes a))))

properties
    :: W3CTag tag
    => Sequence (Action Properties)
    -> ElementBuilder tag ()
properties p = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaProperties (fmap pure p)))))

properties'
    :: W3CTag tag
    => Sequence [Action Properties]
    -> ElementBuilder tag ()
properties' p = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaProperties p))))

postprocess
    :: W3CTag tag
    => Sequence Postprocess
    -> ElementBuilder tag ()
postprocess p = ElementBuilder $ (lift . lift) (tell (Dual (Endo (schemaPostprocess p))))

makeText :: Document -> T.Text -> MomentIO Text
makeText document txt = do
    let txtJSString :: JSString = textToJSString txt
    Just txt <- document `createTextNode` txtJSString
    pure txt
