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

module Reactive.DOM.WebApp (

      WebAppFlow
    , webAppFlow
    , alterWebAppFlow
    , Root
    , Piece
    , Capture
    , type (:</>)
    , (:<|>)(..)
    , RouterStructure(..)
    , Router
    , MakeRouterStructure
    , makeRouterStructure
    , MakeRouter
    , makeRouter
    , jumpTo
    , start
    , webApp

    ) where

import Prelude hiding ((.), id, div)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Control.Category
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Reader
import Data.Void
import Data.Proxy
import Data.Algebraic.Product
import Data.List (intersperse)
import qualified Data.Text as T
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Flow
import Reactive.DOM.Children.NodeList
import Reactive.DOM.Widget.Primitive
import GHCJS.Types
import GHCJS.DOM.Window hiding (error, print, getWindow)
import GHCJS.DOM.Location (getPathname)
import GHCJS.DOM.History
import GHCJS.DOM.EventM
import Data.JSString.Text
import Web.HttpApiData

{-
import GHCJS.DOM.Document hiding (getLocation)
import Reactive.DOM.Widget
import Reactive.DOM.Node (render)
import Reactive.Banana.Combinators
import GHCJS.DOM
-}

-- | A Flow () s t with a DOM Window and some router hidden under a reader.
--   Those data are necessary to manipulate browser history state and to
--   resolve type-level strings to other Flows.
newtype WebAppFlow router s t = WebAppFlow {
      runWebAppFlow :: ReaderArrow (Window, Router router) (Flow ()) s t
    }

deriving instance Category (WebAppFlow router)
deriving instance Arrow (WebAppFlow router)
deriving instance ArrowChoice (WebAppFlow router)
instance ArrowApply (WebAppFlow router) where
    app = WebAppFlow $ proc (flow, s) -> do
              app -< (runWebAppFlow flow, s)
deriving instance ArrowReader (Window, Router router) (WebAppFlow router)
instance ArrowAddReader (Window, Router router) (WebAppFlow router) (Flow ()) where
    liftReader = WebAppFlow . liftReader
    elimReader = elimReader . runWebAppFlow

webAppFlow :: Flow () s t -> WebAppFlow router s t
webAppFlow = liftReader

-- | Any alteration of a Flow which is polymorphic in the input can be used
--   to modify a WebAppFlow. That poymorphism is needed because inside the
--   flow we have actually a reader-ful arrow carrying a Window and router.
alterWebAppFlow
    :: (forall s . Flow () s t -> Flow () s t')
    -> WebAppFlow router s t
    -> WebAppFlow router s t'
alterWebAppFlow f = WebAppFlow . ReaderArrow . f . runReader . runWebAppFlow

-- DO NOT EXPORT
getWindow :: WebAppFlow router () Window
getWindow = readState >>> arr fst

-- DO NOT EXPORT
getRouter :: WebAppFlow router () (Router router)
getRouter = readState >>> arr snd

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

data RouterStructure router routesSoFar where
    RouterStructureSingle
        :: Proxy route
        -> WebAppFlow router (RouteInput route) Void
        -> RouterStructure router route
    RouterStructureCons
        :: Proxy route
        -> WebAppFlow router (RouteInput route) Void
        -> RouterStructure router routesSoFar
        -> RouterStructure router (route :<|> routesSoFar)

type Router router = RouterStructure router router

infixr 1 :<|>
data left :<|> right = left :<|> right

class MakeRouterStructure router routes thing where
    makeRouterStructure :: Proxy router -> thing -> RouterStructure router routes

instance
    ( RouteInput route ~ s
    ) => MakeRouterStructure router route (WebAppFlow router s Void)
  where
    makeRouterStructure _ waflow = RouterStructureSingle Proxy waflow

instance {-# OVERLAPS #-}
    ( MakeRouterStructure router routes rest
    , RouteInput route ~ s
    ) => MakeRouterStructure router (route :<|> routes) (WebAppFlow router s Void :<|> rest)
  where
    makeRouterStructure router (waflow :<|> rest) =
        RouterStructureCons Proxy waflow (makeRouterStructure router rest)

class MakeRouter router routes where
    makeRouter :: Proxy router -> routes -> Router router

instance
    ( MakeRouterStructure router router thing
    ) => MakeRouter router thing
  where
    makeRouter = makeRouterStructure


class HasRoute router routes route where
    getRoute
        :: RouterStructure router routes
        -> Proxy route 
        -> WebAppFlow router (RouteInput route) Void

instance
    (
    ) => HasRoute router route route
  where
    getRoute (RouterStructureSingle _ waflow) _ = waflow

instance {-# OVERLAPS #-}
    (
    ) => HasRoute router (route :<|> routes) route
  where
    getRoute (RouterStructureCons _ waflow _) _ = waflow

instance {-# OVERLAPS #-}
    ( HasRoute router routes route
    ) => HasRoute router (route' :<|> routes) route
  where
    getRoute (RouterStructureCons _ _ rest) route = getRoute rest route


class MatchRoute router routes where
    matchRoute
        :: RouterStructure router routes
        -> [T.Text]
        -> Maybe (WebAppFlow router () Void)

instance IsRoute route => MatchRoute router route where
    matchRoute (RouterStructureSingle route waflow) txts =
        case matchRoutePath route txts of
            Just input -> Just (arr (const input) >>> waflow)
            Nothing -> Nothing

instance {-# OVERLAPS #-}
    ( MatchRoute router routes
    , IsRoute route
    ) => MatchRoute router (route :<|> routes)
  where
    matchRoute (RouterStructureCons route waflow rest) txts =
        case matchRoutePath route txts of
            Just input -> Just (arr (const input) >>> waflow)
            Nothing -> matchRoute rest txts


-- Kick off a flow using a Window object, from which the browser's navigation
-- bar's path name is retrieved.
-- It's intended that this be used on page load, and on history pop/push events.
-- From these ingredients a Sequence (Flow o () Void) may be derived, and
-- from that we can derive children of a widget.
start
    :: ( MatchRoute router router )
    => Proxy router
    -> WebAppFlow router () Void
    -> WebAppFlow router () Void
start _ notFound = proc () -> do
    window <- getWindow -< ()
    router <- getRouter -< ()
    pathParts <- liftReader getPathParts -< window
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
    liftIO (putStrLn (show (mconcat [T.pack "Setting history to ", urlpath])))
    Just history <- getHistory window
    pushState history nullRef "" urlpath
    pure ()

-- | Go to the flow in a route under a particular name, prefixed by an
--   effectful flow which pushes the window's history to the route path under
--   the given name.
jumpTo
    :: forall router route .
       ( IsRoute route
       , HasRoute router router route
       )
    => Proxy router
    -> Proxy route
    -> WebAppFlow router (RouteInput route) Void
jumpTo _ name = proc inp -> do
    window <- getWindow -< ()
    router <- getRouter -< ()
    () <- liftReader setHistory -< (window, path inp)
    let nextFlow = getRoute router name
    app -< (nextFlow, inp)
  where
    path = routePath (Proxy :: Proxy route)

routePath
    :: ( IsRoute route )
    => Proxy route
    -> RouteInput route
    -> T.Text
routePath route = T.cons '/' . mconcat . intersperse (T.pack "/") . routePathParts route

webApp
    :: ( MatchRoute router router )
    => Window
    -> Router router
    -> WebAppFlow router () Void -- route doesn't match, use this.
    -> OpenWidget () ()
webApp window router notFound = widget $ \_ -> do
    liftMomentIO (liftIO (putStrLn "webApp : setting up"))
    (rest, fire) <- liftMomentIO newEvent
    let state = (window, router)
    let onPopState :: IO ()
        onPopState = do
            Just location <- getLocation window
            pathname <- textFromJSString <$> getPathname location
            putStrLn ("webApp : window.onPopState fires with path name " ++ show pathname)
            let flow = arr (\i -> (i, state)) >>> runReader (runWebAppFlow (start Proxy notFound))
            fire flow
    -- Oddly enough, popstate is also fired when history is pushed.
    -- The web is weird.
    unbind <- liftMomentIO . liftIO $ on window popState (liftIO onPopState)
    let first = arr (\i -> (i, state)) >>> runReader (runWebAppFlow (start Proxy notFound))
    let seqnc :: Sequence MomentIO (Flow () () Void)
        seqnc = first |> rest
    liftMomentIO (sequenceReactimate (const (putStrLn "webApp : flow changing") <$> seqnc))
    let makeUI :: Flow () () Void -> UI (Sequence MomentIO ())
        makeUI flow = ui (div (runFlow flow))
        -- NB the following will not typecheck.
        --   makeUI = ui . div . runFlow
        -- Why?
        --   runFlow :: Flow o s Void -> (forall tag . Widget tag s (Sequence MomentIO o))
        --   div :: (forall tag . Widget tag s t) -> Widget "div" s t
        --   ui :: forall tag t . W3CTagName tag => Widget tag () t -> UI t
        -- Could it be a bug? Surely if
        --   f (g x)
        -- is well typed then so too is
        --   f . g
    let childrenSequence :: Sequence MomentIO (NodeList (Sequence MomentIO ()) SetChild)
        childrenSequence = nodeList . pure . newChild . makeUI <$> seqnc
    (firstChild, restChild) <- liftMomentIO (runSequence childrenSequence)
    pure ((), children firstChild (pure <$> restChild))


{-
type Ex1 = Piece "a" :</> Capture Int :</> Root
type Ex2 = Piece "b" :</> Root
type Ex3 = Piece "c" :</> Root
type Ex4 = Root

type Combined =
         Ex1
    :<|> Ex2
    :<|> Ex3
    :<|> Ex4

ex1 :: WebAppFlow Combined (RouteInput Ex1) Void
ex1 = proc (i :*: ()) -> do
    () <- webAppFlow w -< i
    jumpTo Proxy (Proxy :: Proxy Ex2) -< ()
  where
    w = widgetFlow1 $ \i -> do
            let l = withEvent Click const
                  $ label (always (T.pack (show i)))
            (ev, velem) <- runWidget <$> l
            pure $ (Widget ((), velem), ev)

ex2 :: WebAppFlow Combined (RouteInput Ex2) Void
ex2 = proc () -> do
    () <- webAppFlow w -< ()
    jumpTo Proxy (Proxy :: Proxy Ex1) -< (42 .*. ())
  where
    w = widgetFlow1 $ \() -> do
            (ev, velem) <- runWidget <$> withEvent Click const (label (always (T.pack "B")))
            pure (Widget ((), velem), ev)

ex3 :: WebAppFlow Combined (RouteInput Ex3) Void
ex3 = undefined -- impureFlow $ \() -> liftIO (putStrLn "Gotcha") >> undefined

ex4 :: WebAppFlow Combined () Void
ex4 = undefined

combinedRouter :: Router Combined
combinedRouter = makeRouter (Proxy :: Proxy Combined) combined
  where
    combined =
             ex1
        :<|> ex2
        :<|> ex3
        :<|> ex4

main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let notFound = liftReader $ widgetFlow1 $ \_ -> do
            w <- label (always (T.pack "Not found"))
            pure (w, never)

    let networkDescription = do

            ui <- webApp webView combinedRouter notFound
            (_, velem) <- runWidget <$> runFlow ui ()
            _ <- render document body velem
            pure ()

    network <- compile networkDescription
    actuate network
    pure ()

-}
