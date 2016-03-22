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

      WebApp(..)
    , WebAppFlow
    , webApp
    , webAppFlow
    , webAppFlow_
    , webAppGetState
    , webAppPutState
    , webAppModifyState
    , webAppEnv
    , embedWebAppFlow
    , jumpTo
    , getUrl
    , liftFlowF
    , Router
    , Root
    , Piece
    , Capture
    , type (:</>)
    , (:<|>)(..)
    --, WebAppFlowExplicit
    --, alterWebAppFlowExplicit
    --, RouterStructure(..)
    --, MakeRouterStructure
    --, makeRouterStructure
    --, MakeRouter
    --, makeRouter
    --, start

    ) where

import Prelude hiding ((.), id, div)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Control.Category
import Control.Arrow
import Control.Arrow.Operations
--import Control.Arrow.Transformer.State
import Control.Arrow.ReactiveState
import Control.Arrow.Transformer.Reader
import Data.Void
import Data.Proxy
import Data.Profunctor
import Data.Algebraic.Product
import Data.List (intersperse)
import Data.Semigroup
import qualified Data.Text as T
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Flow
import Reactive.DOM.Children.Single
import Reactive.DOM.Widget.Common
import GHCJS.Types
import GHCJS.DOM
import GHCJS.DOM.Window hiding (error, print, getWindow)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Location (getPathname)
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.History
import GHCJS.DOM.EventM
import Data.JSString.Text
import Web.HttpApiData

-- Seems we do need a special reactive state arrow transformer. Must obtain
-- from it a Behavior state. Ultimately we shall switchB on these when a
-- web app flow is run.

-- | A Flow () s t with a DOM Window and some router hidden under a reader.
--   Those data are necessary to manipulate browser history state and to
--   resolve type-level strings to other Flows.
newtype WebAppFlowExplicit router env state o s t = WebAppFlowExplicit {
      runWebAppFlowExplicit
          :: ReaderArrow ( Window
                         , Router router env state
                         , env
                         )
                         (ReactiveStateArrow state (Flow o)) s t
    }

instance Profunctor (WebAppFlowExplicit router env state o) where
    dimap l r waflow = arr l >>> waflow >>> arr r
instance Functor (WebAppFlowExplicit router env state o s) where
    fmap = rmap
deriving instance Category (WebAppFlowExplicit router env state o)
deriving instance Arrow (WebAppFlowExplicit router env state o)
deriving instance ArrowChoice (WebAppFlowExplicit router env state o)

instance ArrowApply (WebAppFlowExplicit router env state o) where
    app = WebAppFlowExplicit $ proc (flow, s) -> do
              app -< (runWebAppFlowExplicit flow, s)

instance ArrowReader env (WebAppFlowExplicit router env state o) where
    readState = WebAppFlowExplicit $ proc x -> do
        (_, _, x) <- readState -< x
        returnA -< x
    newReader flow = WebAppFlowExplicit $ proc (x, r) -> do
        runWebAppFlowExplicit flow -< x

{-
-- TODO can we get this? I don't think so. Solution may be instead to
-- parameterize experience flow on an Event t -> Moment (Event t)
liftFlowF'
    :: (Flow o1 s1 t1 -> Flow o2 s2 t2)
    -> WebAppFlowExplicit router env state o1 s1 t1
    -> WebAppFlowExplicit router env state o2 s2 t2
liftFlowF' f waflow = undefined
-}

liftFlowF
    :: (forall s t . Flow o1 s t -> Flow o2 s t)
    -> WebAppFlowExplicit router env state o1 s t
    -> WebAppFlowExplicit router env state o2 s t
liftFlowF f = WebAppFlowExplicit . ReaderArrow . ReactiveStateArrow . f . runReactiveStateArrow . runReader . runWebAppFlowExplicit

-- DO NOT EXPORT
getWindow :: WebAppFlowExplicit router env state o () Window
getWindow = WebAppFlowExplicit $ proc () -> do
    (w,_,_) <- readState -< ()
    returnA -< w

-- DO NOT EXPORT
getOrigin :: WebAppFlowExplicit router env state o () T.Text
getOrigin = proc () -> do
    window <- getWindow -< ()
    Just location <- webAppFlow_ . impureFlow $ getLocation -< window
    webAppFlow_ . impureFlow $ Location.getOrigin -< location

-- DO NOT EXPORT
getRouter :: WebAppFlowExplicit router env state o () (Router router env state)
getRouter = WebAppFlowExplicit $ proc () -> do
    (_,r,_) <- readState -< ()
    returnA -< r

webAppGetState :: WebAppFlowExplicit router env state o anything state
webAppGetState = WebAppFlowExplicit (liftReader reactiveStateArrowGet)

webAppPutState :: WebAppFlowExplicit router env state o state ()
webAppPutState = WebAppFlowExplicit (liftReader reactiveStateArrowPut)

webAppModifyState :: WebAppFlowExplicit router env state o (state -> state) state
webAppModifyState = WebAppFlowExplicit (liftReader reactiveStateArrowModify)

webAppEnv :: WebAppFlowExplicit router env state o anything env
webAppEnv = readState

webAppFlow_ :: Flow o s t -> WebAppFlowExplicit router env state o s t
webAppFlow_ = WebAppFlowExplicit . liftReader . statelessArrow

webAppFlow :: Flow o (s, env, state) (t, state) -> WebAppFlowExplicit router env state o s t
webAppFlow flow = proc s -> do
    env <- readState -< ()
    state <- webAppGetState -< ()
    (t, state) <- WebAppFlowExplicit (liftReader (statelessArrow flow)) -< (s, env, state)
    _ <- webAppPutState -< state
    returnA -< t

-- Types for defining individual routes: sequences of static and variable
-- parts separated by :</> or just Root.
data Root
data Piece (sym :: Symbol)
data Capture (t :: *)
infixr 1 :</>
data left :</> right

-- | The input to the route is discovered from its type-level form.
--   Every Capture t induces a t term in a Data.Algebraic.Product type.
class IsRoute route where
    type RouteInput route :: *
    routePathParts :: Proxy route -> RouteInput route -> [T.Text]
    matchRoutePath :: Proxy route -> [T.Text] -> Maybe (RouteInput route)

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
    routePathParts _ input = T.pack (symbolVal (Proxy :: Proxy name)) : routePathParts (Proxy :: Proxy rest) input
    matchRoutePath _ ts = case ts of
        t : rest -> if t == T.pack (symbolVal (Proxy :: Proxy name))
                    then matchRoutePath (Proxy :: Proxy rest) rest
                    else Nothing
        _ -> Nothing

instance
    ( ToHttpApiData t
    , FromHttpApiData t
    , IsRoute rest
    ) => IsRoute (Capture t :</> rest)
  where
    type RouteInput (Capture t :</> rest) = t :*: RouteInput rest
    routePathParts _ (t :*: rest) = toUrlPiece t : routePathParts (Proxy :: Proxy rest) rest
    matchRoutePath _ txts = case txts of
        txt : rest -> case parseUrlPiece txt of
            Right t -> (.*.) <$> pure t <*> matchRoutePath (Proxy :: Proxy rest) rest
            Left _ -> Nothing
        _ -> Nothing

data RouterStructure router env state routesSoFar where
    RouterStructureSingle
        :: Proxy route
        -> WebAppFlowExplicit router env state o (RouteInput route) Void
        -> RouterStructure router env state route
    RouterStructureCons
        :: Proxy route
        -> WebAppFlowExplicit router env state o (RouteInput route) Void
        -> RouterStructure router env state routesSoFar
        -> RouterStructure router env state (route :<|> routesSoFar)

type Router router env state = RouterStructure router env state router

infixr 1 :<|>
data left :<|> right = left :<|> right

class MakeRouterStructure router env state routes thing where
    makeRouterStructure
        :: Proxy router
        -> Proxy env
        -> Proxy state
        -> thing
        -> RouterStructure router env state routes

instance
    ( RouteInput route ~ s
    ) => MakeRouterStructure router env state route (WebAppFlowExplicit router env state o s Void)
  where
    makeRouterStructure _ _ _ waflow = RouterStructureSingle Proxy waflow

instance {-# OVERLAPS #-}
    ( MakeRouterStructure router env state routes rest
    , RouteInput route ~ s
    ) => MakeRouterStructure router env state (route :<|> routes) (WebAppFlowExplicit router env state o s Void :<|> rest)
  where
    makeRouterStructure router env state (waflow :<|> rest) =
        RouterStructureCons Proxy waflow (makeRouterStructure router env state rest)

class MakeRouter router env state routes where
    makeRouter
        :: Proxy router
        -> Proxy env
        -> Proxy state
        -> routes
        -> Router router env state

instance
    ( MakeRouterStructure router env state router thing
    ) => MakeRouter router env state thing
  where
    makeRouter = makeRouterStructure


class HasRoute router env state routes route where
    getRoute
        :: RouterStructure router env state routes
        -> Proxy route
        -> forall o . WebAppFlowExplicit router env state o (RouteInput route) Void

instance
    (
    ) => HasRoute router env state route route
  where
    -- Must use flowTrans to kill the side-channel value, for
    -- RouterStructureSingle hides it.
    -- Consequently, we cannot observe a side-channel for a web app flow which
    -- is discovered from a router.
    getRoute (RouterStructureSingle _ waflow) _ = liftFlowF (flowTrans (const (pure Nothing))) waflow

instance {-# OVERLAPS #-}
    (
    ) => HasRoute router env state (route :<|> routes) route
  where
    getRoute (RouterStructureCons _ waflow _) _ = liftFlowF (flowTrans (const (pure Nothing))) waflow

instance {-# OVERLAPS #-}
    ( HasRoute router env state routes route
    ) => HasRoute router env state (route' :<|> routes) route
  where
    getRoute (RouterStructureCons _ _ rest) route = getRoute rest route

class MatchRoute router env state routes where
    matchRoute
        :: RouterStructure router env state routes
        -> [T.Text]
        -> forall o . Maybe (WebAppFlowExplicit router env state o () Void)

instance IsRoute route => MatchRoute router env state route where
    matchRoute (RouterStructureSingle route waflow) txts =
        case matchRoutePath route txts of
            Just input -> Just (arr (const input) >>> liftFlowF (flowTrans (const (pure Nothing))) waflow)
            Nothing -> Nothing

instance {-# OVERLAPS #-}
    ( MatchRoute router env state routes
    , IsRoute route
    ) => MatchRoute router env state (route :<|> routes)
  where
    matchRoute (RouterStructureCons route waflow rest) txts =
        case matchRoutePath route txts of
            Just input -> Just (arr (const input) >>> liftFlowF (flowTrans (const (pure Nothing))) waflow)
            Nothing -> matchRoute rest txts

-- Kick off a flow using a Window object, from which the browser's navigation
-- bar's path name is retrieved.
-- It's intended that this be used on page load, and on history pop/push events.
-- From these ingredients a Sequence (Flow o () Void) may be derived, and
-- from that we can derive children of a widget.
start
    :: ( MatchRoute router env state router )
    => Proxy router
    -> WebAppFlowExplicit router env state o () Void
    -> WebAppFlowExplicit router env state o () Void
start _ notFound = proc () -> do
    window <- getWindow -< ()
    router <- getRouter -< ()
    pathParts <- webAppFlow_ getPathParts -< window
    let match = matchRoute router pathParts
    case match of
        Nothing -> do notFound -< ()
        Just found -> do app -< (found, ())

getPathParts :: forall o . Flow o Window [T.Text]
getPathParts = impureFlow $ \window -> do
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

setHistory :: forall o . Flow o (Window, T.Text) ()
setHistory = impureFlow $ \(window, urlpath) -> do
    --liftIO (putStrLn (show (mconcat ["Setting history to ", urlpath])))
    Just history <- getHistory window
    pushState history nullRef ("" :: T.Text) urlpath
    pure ()

-- | Make the full URL of a given route. The window's origin is used.
getUrl
    :: (IsRoute route, HasRoute router env state router route)
    => WebAppFlowExplicit router env state o (Proxy route, RouteInput route) T.Text
getUrl = proc (proxy, inp) -> do
    origin <- getOrigin -< ()
    path <- arr (uncurry routePath) -< (proxy, inp)
    returnA -< mconcat [origin, path]

routePath
    :: ( IsRoute route )
    => Proxy route
    -> RouteInput route
    -> T.Text
routePath route = T.cons '/' . mconcat . intersperse (T.pack "/") . routePathParts route

-- | Go to the flow in a route under a particular name, prefixed by an
--   effectful flow which pushes the window's history to the route path under
--   the given name.
jumpTo
    :: forall router route env state o .
       ( IsRoute route
       , HasRoute router env state router route
       )
    => Proxy router
    -> Proxy route
    -> WebAppFlowExplicit router env state o (RouteInput route) Void
jumpTo _ name = proc inp -> do
    window <- getWindow -< ()
    router <- getRouter -< ()
    () <- webAppFlow_ setHistory -< (window, path inp)
    let nextFlow = getRoute router name
    app -< (nextFlow, inp)
  where
    path = routePath (Proxy :: Proxy route)

-- | Run a WebApp. Do not use this more than once. It does mutations through 
--   the W3C history API and binds events on it too.
webApp'
    :: forall router env state o .
       ( MatchRoute router env state router )
    => Window
    -> Router router env state
    -> WebAppFlowExplicit router env state o () Void -- route doesn't match, use this.
    -> OpenWidget (env, ReactiveState state) (Sequence (Maybe o))
webApp' window router notFound = widget $ \(~((env, reactiveState), viewChildren)) -> do
    --liftMomentIO (liftIO (putStrLn "webApp : setting up"))
    (rest, fire) <- liftMomentIO newEvent
    let fullEnv = (window, router, env)
    let onPopState :: IO ()
        onPopState = do
            Just location <- getLocation window
            pathname <- textFromJSString <$> getPathname location
            putStrLn ("webApp : window.onPopState fires with path name " ++ show pathname)
            -- We need a way to get the current state of the flow at this point
            -- in time!
            let flow = arr (\i -> ((i, fullEnv), reactiveState))
                    >>> runReactiveStateArrow (runReader (runWebAppFlowExplicit (start Proxy notFound)))
            fire flow
    -- Oddly enough, popstate is also fired when history is pushed.
    -- The web is weird.
    unbind <- liftMomentIO . liftIO $ on window popState (liftIO onPopState)
    let first = arr (\i -> ((i, fullEnv), reactiveState))
            >>> runReactiveStateArrow (runReader (runWebAppFlowExplicit (start Proxy notFound)))
    let seqnc :: Sequence (Flow o () Void)
        seqnc = first |> rest
    --liftMomentIO (sequenceReactimate (const (putStrLn "webApp : flow changing") <$> seqnc))
    -- Make every flow div inherit all style from the webApp div.
    let inheritAll = always . Set $ makeStyle [("all", "inherit")]
    let inheritAllModifier = modifier $ \x -> style inheritAll >> pure x
    let makeUI :: Flow o () Void -> UI (Sequence (Maybe o))
        makeUI flow = ui (div (runFlow flow) `modifyr` inheritAllModifier)
        -- NB the following will not typecheck.
        --   makeUI = ui . div . runFlow
        -- Why?
        --   runFlow :: Flow o s Void -> (forall tag . Widget tag s (Sequence o))
        --   div :: (forall tag . Widget tag s t) -> Widget "div" s t
        --   ui :: forall tag t . W3CTag  tag => Widget tag () t -> UI t
        -- Could it be a bug? Surely if
        --   f (g x)
        -- is well typed then so too is
        --   f . g
    let childrenSequence :: forall inp out . Sequence (Single (Sequence (Maybe o)) inp out SetChild)
        childrenSequence = Single . newChild . makeUI <$> seqnc
    let ~(firstChild, restChild) = runSequence childrenSequence
    let viewChildrenSequence = viewChildrenInitial viewChildren |> viewChildrenEvent viewChildren
    outSeqnc :: Sequence (Maybe o)
        <- buildSequence (sequenceSwitch (childData . runSingle <$> viewChildrenSequence))
    pure (outSeqnc, children firstChild (pure <$> restChild))

-- | Given a complete web app flow with a side-channel event, embed it into
--   another web app flow on the same routes, environment, and state, by
--   using its side-channel as its control event.
--
--   It's just openFlow but with the extra web app decorations.
embedWebAppFlow
    :: forall routes env state s t anything .
       WebAppFlowExplicit routes env state (Event t) s Void
    -> WebAppFlowExplicit routes env state anything s t
embedWebAppFlow =
      WebAppFlowExplicit
    . ReaderArrow
    . ReactiveStateArrow
    . openIt
    . runReactiveStateArrow
    . runReader
    . runWebAppFlowExplicit
  where
    -- To use openFlow the output must be Void. Here we have (Void, state)
    -- so that's just as well.
    openIt flow = openFlow (flow >>> arr absurd)

class 
    ( MatchRoute (WebAppRoutes app) (WebAppEnvironment app) (WebAppState app) (WebAppRoutes app)
    ) => WebApp (app :: *)
  where
    -- | Description of routes.
    type WebAppRoutes app :: *
    -- | Read-only data for the WebApp.
    type WebAppEnvironment app :: *
    -- | Read/write data for the WebApp.
    type WebAppState app :: *

type WebAppFlow app o = WebAppFlowExplicit (WebAppRoutes app) (WebAppEnvironment app) (WebAppState app) o

-- | Since a WebApp needs control of the browser history, it's not suitable
--   for embedding in other UIs. Thus we don't give you an OpenWidget, but
--   instead close it up as a div and render it in the body.
--   Its style is "all: inherit" so whatever you do to the body, you do to the
--   div.
webApp
    :: ( WebApp app
       , MakeRouter (WebAppRoutes app) (WebAppEnvironment app) (WebAppState app) router
       )
    => Proxy app
    -> Window
    -> WebAppEnvironment app
    -> WebAppState app
    -> WebAppFlow app o () Void
    -> router
    -> MomentIO ()
webApp app window env state notFound router = do
    reactiveState <- newReactiveState state
    let widget = div (webApp' window router' notFound)
                 `modify_`
                 (modifier $ \_ -> style' (always [Set inheritAll]))
    Just document <- liftIO (webViewGetDomDocument window)
    Just body <- getBody document
    _ <- render document body (closeWidget Tag (lmap (const (env, reactiveState)) widget))
    pure ()
  where
    router' = makeRouter Proxy Proxy Proxy router
    inheritAll = makeStyle [("all", "inherit")]
