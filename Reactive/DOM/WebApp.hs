{-|
Module      : Reactive.DOM.WebApp
Description : Types and functions for defining single-page web apps.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.WebApp (

      Root
    , Piece
    , Capture
    , (:</>)
    , Route(..)
    , IsRoute
    , RouterStructure
    , Router
    , MakeRouter
    , makeRouter
    , (:<|>)
    , HasRoute
    , getRoute
    , MatchRoute
    , matchRoute

    , WebAppM(..)
    , WebAppN(..)
    , wa
    , jumpTo
    , dischargeWebAppM
    , dischargeWebAppN

    , routeUrl

    , WebAppTransition

    , standardWebApp

    ) where

import Prelude hiding ((.), id, div)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Control.Category
import Control.Arrow
import Control.Arrow.Flow
import Control.Monad (join, (>=>))
import Data.Void
import Data.Proxy
import Data.Algebraic.Product
import Data.List (intersperse)
import qualified Data.Text as T
import Data.IORef
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.WidgetFlow
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.DOM
import GHCJS.DOM.Window hiding (error, print, getWindow)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Location (getPathname)
import GHCJS.DOM.History
import GHCJS.DOM.EventM hiding (event)
import qualified GHCJS.DOM.EventM as EventM
import GHCJS.DOM.PopStateEvent hiding (getState)
import qualified GHCJS.DOM.PopStateEvent as PopStateEvent
import Data.JSString.Text
import Web.HttpApiData

-- Types for defining individual routes: sequences of static and variable
-- parts separated by :</> or just Root.
data Root
data Piece (sym :: Symbol)
data Capture (t :: *)
infixr 1 :</>
data left :</> right

-- | Proxy for a Route.
data Route (route :: *) = Route

-- | The input to the route is discovered from its type-level form.
--   Every Capture t induces a t term in a Data.Algebraic.Product type.
class IsRoute route where
    type RouteInput route :: *
    routePathParts :: Route route -> RouteInput route -> [T.Text]
    matchRoutePath :: Route route -> [T.Text] -> Maybe (RouteInput route)

instance IsRoute Root where
    type RouteInput Root = ()
    routePathParts _ _ = []
    matchRoutePath _ ts = case ts of
        [] -> Just ()
        _ -> Nothing

instance
    ( KnownSymbol name
    , IsRoute rest
    ) => IsRoute (Piece name :</> rest)
  where
    type RouteInput (Piece name :</> rest) = RouteInput rest
    routePathParts _ input = T.pack (symbolVal (Proxy :: Proxy name)) : routePathParts (Route :: Route rest) input
    matchRoutePath _ ts = case ts of
        t : rest -> if t == T.pack (symbolVal (Proxy :: Proxy name))
                    then matchRoutePath (Route :: Route rest) rest
                    else Nothing
        _ -> Nothing

instance
    ( ToHttpApiData t
    , FromHttpApiData t
    , IsRoute rest
    ) => IsRoute (Capture t :</> rest)
  where
    type RouteInput (Capture t :</> rest) = t :*: RouteInput rest
    routePathParts _ (t :*: rest) = toUrlPiece t : routePathParts (Route :: Route rest) rest
    matchRoutePath _ txts = case txts of
        txt : rest -> case parseUrlPiece txt of
            Right t -> (.*.) <$> pure t <*> matchRoutePath (Route :: Route rest) rest
            Left _ -> Nothing
        _ -> Nothing

data RouterStructure (a :: * -> * -> *) router routesSoFar where
    RouterStructureSingle
        :: Route route
        -> Flow (WebAppM router a) (RouteInput route) Void
        -> RouterStructure a router (Route route)
    RouterStructureCons
        :: Route route
        -> Flow (WebAppM router a) (RouteInput route) Void
        -> RouterStructure a router routesSoFar
        -> RouterStructure a router (Route route :<|> routesSoFar)

type Router a router = RouterStructure a router router

infixr 1 :<|>
data left :<|> right = left :<|> right

class MakeRouterStructure a router routes thing where
    makeRouterStructure
        :: Proxy a
        -> Proxy router
        -> Proxy routes
        -> thing
        -> RouterStructure a router routes

instance
    ( RouteInput route ~ s
    ) => MakeRouterStructure a router (Route route) (Flow (WebAppM router a) s Void)
  where
    makeRouterStructure _ _ _ flow =
        RouterStructureSingle (Route :: Route route) flow

instance {-# OVERLAPS #-}
    ( MakeRouterStructure a router routes rest
    , RouteInput route ~ s
    ) => MakeRouterStructure a router (Route route :<|> routes) (Flow (WebAppM router a) s Void :<|> rest)
  where
    makeRouterStructure a router route (flow :<|> rest) =
        RouterStructureCons (Route :: Route route) flow (makeRouterStructure a router (Proxy :: Proxy routes) rest)

class MakeRouter a router thing where
    makeRouter
        :: Proxy a
        -> Proxy router
        -> thing
        -> Router a router

instance
    ( MakeRouterStructure a router router thing
    ) => MakeRouter a router thing
  where
    makeRouter a router = makeRouterStructure a router router

class HasRoute a router routes route where
    getRoute
        :: RouterStructure a router routes
        -> Route route
        -> Flow (WebAppM router a) (RouteInput route) Void

instance
    (
    ) => HasRoute a router (Route route) route
  where
    getRoute (RouterStructureSingle _ flow) _ = flow

instance {-# OVERLAPS #-}
    (
    ) => HasRoute a router (Route route :<|> routes) route
  where
    getRoute (RouterStructureCons _ flow _) _ = flow

instance {-# OVERLAPS #-}
    ( HasRoute a router routes route
    ) => HasRoute a router (Route route' :<|> routes) route
  where
    getRoute (RouterStructureCons _ _ rest) route = getRoute rest route

class MatchRoute a router routes where
    matchRoute
        :: RouterStructure a router routes
        -> [T.Text]
        -> Maybe (Flow (WebAppM router a) () Void)

instance (IsRoute route) => MatchRoute a router (Route route) where
    matchRoute (RouterStructureSingle route flowPiece) txts =
        case matchRoutePath route txts of
            Just input -> Just (arr (const input) >>> flowPiece)
            Nothing -> Nothing

instance {-# OVERLAPS #-}
    ( MatchRoute a router routes
    , IsRoute route
    ) => MatchRoute a router (Route route :<|> routes)
  where
    matchRoute (RouterStructureCons route flowPiece rest) txts =
        case matchRoutePath route txts of
            Just input -> Just (arr (const input) >>> flowPiece)
            Nothing -> matchRoute rest txts

data WebAppM router m s t where
    WebAppEmbedM :: m s t -> WebAppM router m s t
    WebAppJumpM
        :: ( IsRoute route
           , HasRoute m router router route
           )
        => Route route
        -> WebAppM router m (RouteInput route) Void

data WebAppN router n t where
    WebAppEmbedN :: n t -> WebAppN router n t
    WebAppJumpN
        :: ( IsRoute route )
        => Route route
        -> RouteInput route
        -> WebAppN router n (FlowContinuation (WebAppN router n) t)
        -> WebAppN router n t

instance Functor n => Functor (WebAppN router n) where
    fmap f term = case term of
        WebAppEmbedN n -> WebAppEmbedN (fmap f n)
        WebAppJumpN r i n -> WebAppJumpN r i ((fmap . fmap) f n)

-- |
-- = WebApp introduction

wa :: m s t -> WebAppM router m s t
wa = WebAppEmbedM

jumpTo
    :: ( IsRoute route, HasRoute m router router route )
    => Route route
    -> WebAppM router m (RouteInput route) Void
jumpTo = WebAppJumpM

-- |
-- = WebApp elimination

-- | Discharge a WebAppM constructor through a KleisliFlow.
--   The routes are resolved according to some router, and the metadata (whether
--   it's a jump) is preserved by throwing a WebAppN on top of the underlying
--   functor n. Since WebAppN n is a functor whenever n is a functor, the
--   FlowContinuation over this thing is a monad and therefore the Kleisli arrow
--   over the FlowContinuation is an arrow, and so suitable for use in, say,
--   runFlow.
dischargeWebAppM
    :: forall m n router .
       ( Functor n )
    => Router m router
    -> KleisliFlowTransformation m n
    -> KleisliFlowTransformation (WebAppM router m) (WebAppN router n)
dischargeWebAppM router trans = \term s -> case term of
    WebAppEmbedM sub -> WebAppEmbedN (trans sub s)
    WebAppJumpM (route :: Route route) ->
        let resolved :: Flow (WebAppM router m) (RouteInput route) Void
            resolved = getRoute router route
            next :: Flow (KleisliFlow (WebAppN router n)) (RouteInput route) Void
            next = flowTrans (kleisliFlow (dischargeWebAppM router trans)) resolved
            fk :: FlowContinuation (WebAppN router n) Void
            fk = runKleisli (runFlow next) s
            n :: WebAppN router n (FlowContinuation (WebAppN router n) Void)
            n = elimFlowContinuation absurd id fk
        in  WebAppJumpN route s n

-- |
-- = Some utilities for wiring up a web app to the browser navigation
--   mechanisms.

type Origin = T.Text

-- | Extract the path parts from the Window's location: the URL path part
--   separated on '/'.
getPathParts :: Window -> IO [T.Text]
getPathParts window = do
    Just location <- getLocation window
    pathname <- textFromJSString <$> getPathname location
    -- pathname does not include the part after ? so it's all good.
    let splitParts = T.split (== '/') pathname
    -- Whenever an empty string lies to the left or right of a /, we get a
    -- "" entry. So a pathname that begins with a / (all of them) will induce
    -- an initial "", and any // will induce another empty "". If the path
    -- ends in a /, we get a tailing "". Solution: drop all empty strings.
    let isNonEmpty = (/=) 0 . T.length
    let parts = filter isNonEmpty splitParts
    pure parts

-- | Set the Window history to a given path.
--
--   TODO in order to determine forward/backward we shall put data into the
--   state.
setHistory :: Window -> T.Text -> JSVal -> IO ()
setHistory window urlpath val = do
    --liftIO (putStrLn (show (mconcat ["Setting history to ", urlpath])))
    Just history <- getHistory window
    pushState history val ("" :: T.Text) urlpath
    pure ()

-- | Get the path part of a given route. See makeRouteUrl for the full URL.
routePath
    :: ( IsRoute route )
    => Route route
    -> RouteInput route
    -> T.Text
routePath route = T.cons '/' . mconcat . intersperse (T.pack "/") . routePathParts route

-- | Make the full URL of a given route.
routeUrl
    :: (IsRoute route)
    => Origin
    -> Route route
    -> RouteInput route
    -> T.Text
routeUrl origin route routeInput =
    let path = routePath route routeInput
    in  mconcat [origin, path]

-- | Run a WebApp in a standard way: update the browser history on jumps, and
--   respond to the browser's forward/back buttons.
standardWebApp
    :: forall router a fixed transition .
       ( a ~ WidgetM fixed transition
       , MatchRoute a router router
       )
    => Window
    -> Router a router
    -> (forall fixed t . UI (Event (Union fixed (Transition (Either WebAppTransition transition) (FlowContinuation (WidgetN fixed (Either WebAppTransition transition)) t)))) -> UI (Event (Union fixed (Transition (Either WebAppTransition transition) t))))
    -> Flow (WebAppM router a) () Void -- ^ Not found.
    -> KleisliFlowTransformation (WidgetM fixed transition) (WidgetN fixed transition)
    -> MomentIO (Event fixed)
standardWebApp window router makeUI notFound kftrans = do

    let makeFlowContinuation
            :: Flow (WebAppM router (WidgetM fixed transition)) () Void
            -> FlowContinuation (WebAppN router (WidgetN fixed transition)) Void
        makeFlowContinuation = \flow ->
            let transed = flowTrans (kleisliFlow (dischargeWebAppM router kftrans)) flow
            in  runKleisli (runFlow transed) ()

    -- Obtain forwards and backwards events.
    -- We use an IORef with an Int to tag the history states, so that we can
    -- determine whether a pop state is forwards or backwards.
    navCounter <- liftIO $ newIORef (0 :: Int)
    (evForwards, fireForwardsEvent) <- newEvent
    (evBackwards, fireBackwardsEvent) <- newEvent
    let onPopState :: EventM Window PopStateEvent ()
        onPopState = do
            pathParts <- liftIO $ getPathParts window
            let resolvedFlow = matchRoute router pathParts
            let flow = maybe notFound id resolvedFlow
            let fk = makeFlowContinuation flow
            popStateEvent <- EventM.event
            current <- liftIO $ readIORef navCounter
            found :: JSVal <- PopStateEvent.getState popStateEvent
            let found' :: Int
                found' = maybe 0 id (pFromJSVal found)
            if current < found'
            then liftIO $ fireBackwardsEvent fk
            else liftIO $ fireForwardsEvent fk
            liftIO $ writeIORef navCounter found'
            pure ()
    unbind <- liftIO $ on window popState onPopState

    -- Use the window's current path to find the first flow to use.
    pathParts <- liftIO $ getPathParts window
    let match = matchRoute router pathParts
    let initialFlow :: Flow (WebAppM router (WidgetM fixed transition)) () Void
        initialFlow = maybe notFound id match
    let fkWebApp :: FlowContinuation (WebAppN router (WidgetN fixed transition)) Void
        fkWebApp = makeFlowContinuation initialFlow

    let fkWidget :: FlowContinuation (WidgetN (Union fixed T.Text) (Either WebAppTransition transition)) Void
        fkWidget = flowContinuationTrans' (dischargeWebAppN evForwards evBackwards) fkWebApp
    let widget :: UI (Event (Union (Union fixed T.Text) (Transition (Either WebAppTransition transition) Void)))
        widget = either absurd id (runWidgetFlow' makeUI fkWidget)

    -- Render the UI
    Just document <- liftIO $ webViewGetDomDocument window
    Just body <- liftIO $ getBody document
    seqnc <- reactiveDom document body (always widget)
    ev :: Event (Union (Union fixed T.Text) (Transition (Either WebAppTransition transition) Void))
        <- sequenceSwitchE seqnc

    -- Use the text event to update the browser history.
    let historyEvent :: Event T.Text
        historyEvent = filterJust (fmap (pickULeft >=> pickURight) ev)
    let updateHistory :: T.Text -> IO ()
        updateHistory txt = do
            -- With every history update we associate an integer, so that we
            -- can tell on pop state whether we're going backwards or forwards.
            current <- readIORef navCounter
            let next = current + 1
            writeIORef navCounter next
            setHistory window txt (pToJSVal next)
    reactimate (updateHistory <$> historyEvent)

    -- Derive and return the output event.
    let out :: Event fixed
        out = filterJust (fmap (pickULeft >=> pickULeft) ev)

    pure out

-- | Discharge the WebAppN from atop a WidgetN by throwing on forward/backward
--   events and bringing out an event which fires on jumps.
dischargeWebAppN
    :: forall router n fixed transition t .
       ( )
    => (Event (FlowContinuation (WebAppN router (WidgetN fixed transition)) Void))
    -> (Event (FlowContinuation (WebAppN router (WidgetN fixed transition)) Void))
    -> WebAppN router (WidgetN fixed transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t)
    -> WidgetN (Union fixed T.Text) (Either WebAppTransition transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t)
dischargeWebAppN evForwards evBackwards term = case term of

    -- Route and route input are unused here. They are used in the embed case,
    -- whenever the next term is a jump.
    WebAppJumpN _ _ webAppN -> dischargeWebAppN evForwards evBackwards (join <$> webAppN)

    WebAppEmbedN (WidgetN ui) ->
        -- 
        let alterEvent :: forall t . Union fixed (Transition transition (FlowContinuation (WebAppN router (WidgetN fixed transition)) t))
                    -> Union (Union fixed T.Text) (Transition (Either WebAppTransition transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t))
            alterEvent union =
                let withJumpEvent = uGrowLeft takeJumpEvent union
                    withTransition = fmap (transTransition takeTransition) withJumpEvent
                in  withTransition

            takeJumpEvent :: forall t . Transition transition (FlowContinuation (WebAppN router (WidgetN fixed transition)) t)
              -> Maybe T.Text
            takeJumpEvent (Transition (_, next)) = case next of
                FlowNext (WebAppJumpN route routeInput _) -> Just (routePath route routeInput)
                _ -> Nothing

            takeTransition :: transition -> Either WebAppTransition transition
            takeTransition = Right

            navForwards :: forall t . Event (Transition (Either WebAppTransition transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t))
            navForwards = fmap (Transition . (,) (Left WebAppTransitionForwards) . fmap absurd) evForwards

            navBackwards :: forall t . Event (Transition (Either WebAppTransition transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t))
            navBackwards = fmap (Transition . (,) (Left WebAppTransitionBackwards) . fmap absurd) evBackwards

            navTransitions :: forall s t . Event (Union s (Transition (Either WebAppTransition transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t)))
            navTransitions = fmap URight (unionWith const navForwards navBackwards)

            ui' :: UI (Event (Union (Union fixed T.Text) (Transition (Either WebAppTransition transition) (FlowContinuation (WebAppN router (WidgetN fixed transition)) t))))
            ui' = fmap (unionWith const navTransitions) ((fmap . fmap) alterEvent ui)

        in  WidgetN ui'

data WebAppTransition where
    WebAppTransitionForwards :: WebAppTransition
    WebAppTransitionBackwards :: WebAppTransition
