{-|
Module      : Reactive.DOM.WebApp
Description : 
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

module Reactive.DOM.WebApp (

      End
    , type (:</>)
    , NamedRoute(..)
    , named
    , type (:<|>)
    , jumpTo
    , webApp

    ) where

import Prelude hiding ((.), id)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Control.Category
import Control.Arrow
import Control.Arrow.Operations (readState)
import Control.Arrow.Transformer.Reader
import Data.Void
import Data.Proxy
import Data.Algebraic.Product
import Data.List (intersperse)
import qualified Data.Text as T
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Flow
import GHCJS.Types
import GHCJS.DOM.Window hiding (print)
import GHCJS.DOM.Location (getPathname)
import GHCJS.DOM.History
import GHCJS.DOM.EventM
import Data.JSString.Text
import Web.HttpApiData

import Debug.Trace
import Data.Maybe (isJust)

data End
data Piece (sym :: Symbol)
data Capture (t :: *)
infixr 1 :</>
data left :</> right

data NamedRoute name route where
    NamedRoute
        :: ( KnownSymbol name
           , IsRoute route
           )
        => Proxy name
        -> Proxy route
        -> ReaderArrow Window (Flow ()) (RouteInput route) Void
        -> NamedRoute name route

named
    :: ( KnownSymbol name
       , IsRoute route
       )
    => Proxy route
    -> ReaderArrow Window (Flow ()) (RouteInput route) Void
    -> NamedRoute name route
named route flow = NamedRoute Proxy route flow

infixr 1 :<|>
data left :<|> right = left :<|> right

type family GetRoute router (name :: Symbol) :: * where
    GetRoute (NamedRoute name route) name = route
    GetRoute (NamedRoute name route :<|> rest) name = route
    GetRoute (NamedRoute name' route :<|> rest) name = GetRoute rest name

class HasRoute router (name :: Symbol) where
    getRoute :: router -> Proxy name -> ReaderArrow Window (Flow ()) (RouteInput (GetRoute router name)) Void

instance
    ( GetRoute (NamedRoute name route) name ~ route
    ) => HasRoute (NamedRoute name route) name
  where
    getRoute (NamedRoute _ _ flow) _ = flow

instance
    ( GetRoute (NamedRoute name route :<|> rest) name ~ route
    ) => HasRoute (NamedRoute name route :<|> rest) name
  where
    getRoute (NamedRoute _ _ flow :<|> rest) _ = flow

instance {-# OVERLAPS #-}
    ( HasRoute rest name
    -- Obviously true, but GHC can't tell because we're overlapped.
    -- TODO rework to avoid overlap. A new class, which has GetRoute
    -- associated, but a third parameter indicating whether name' and name are
    -- equal.
    , GetRoute (NamedRoute name' route :<|> rest) name ~ GetRoute rest name
    ) => HasRoute (NamedRoute name' route :<|> rest) name
  where
    getRoute (nope :<|> rest) name = getRoute rest name

class MatchRoute router where
    matchRoute :: router -> [T.Text] -> Maybe (ReaderArrow Window (Flow ()) () Void)

instance MatchRoute (NamedRoute name route) where
    matchRoute (NamedRoute name route flow) txts = case matchRoutePath route txts of
        Just input -> Just (arr (const input) >>> flow)
        Nothing -> Nothing

instance {-# OVERLAPS #-}
    ( MatchRoute rest
    ) => MatchRoute (NamedRoute name route :<|> rest)
  where
    matchRoute (NamedRoute name route flow :<|> rest) txts = case matchRoutePath route txts of
        Just input -> Just (arr (const input) >>> flow)
        Nothing -> matchRoute rest txts

-- | The input to the route is discovered from its type-level form.
--   Every Capture t induces a t term in a Data.Algebraic.Product type.
class IsRoute route where
    type RouteInput route :: *
    routePathParts :: Proxy route -> RouteInput route -> [T.Text]
    matchRoutePath :: Proxy route -> [T.Text] -> Maybe (RouteInput route)

instance IsRoute End where
    type RouteInput End = ()
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

-- | Go to the flow in a route under a particular name, prefixed by an
--   effectful flow which pushes the window's history to the route path under
--   the given name.
jumpTo
    :: forall router name .
       ( IsRoute (GetRoute router name)
       , HasRoute router name
       )
    => router
    -> Proxy name
    -> ReaderArrow Window (Flow ()) (RouteInput (GetRoute router name)) Void
jumpTo router name = proc inp -> do
    window <- readState -< ()
    () <- liftReader setHistory -< (window, path inp)
    getRoute router name -< inp
  where
    path = routePath (Proxy :: Proxy (GetRoute router name))

setHistory :: forall o . Flow o (Window, T.Text) ()
setHistory = impureFlow $ \(window, urlpath) -> do
    liftIO (putStrLn (show (mconcat [T.pack "Setting history to ", urlpath])))
    Just history <- getHistory window
    pushState history nullRef "" urlpath
    pure ()

-- Kick off a flow using a Window object, from which the browser's navigation
-- bar's path name is retrieved.
-- It's intended that this be used on page load, and on history pop/push events.
-- From these ingredients a Sequence (Flow o () Void) may be derived, suitable
-- for use by varyingFlow.
start
    :: ( MatchRoute router )
    => router
    -> ReaderArrow Window (Flow ()) () Void
    -> ReaderArrow Window (Flow ()) () Void
start router notFound = proc () -> do
    window <- readState -< ()
    pathParts <- liftReader getPathParts -< window
    let match = matchRoute router pathParts
    _ <- liftReader (impureFlow (\(s,r ) -> liftIO (print (isJust s) >> print r))) -< (match, pathParts)
    case match of
        Nothing -> do notFound -< trace ("NOT FOUND " ++ show pathParts) $ ()
        Just found -> do app -< trace ("FOUND " ++ show pathParts) $ (found, ())

getPathParts :: forall o . Flow o Window [T.Text]
getPathParts = impureFlow $ \window -> do
    Just location <- getLocation window
    pathname <- textFromJSString <$> getPathname location
    -- pathname does not include the part after ? so it's all good.
    let splitParts = T.split (== '/') pathname
    -- If pathname starts with a / (maybe it always does?) then we'll get an
    -- empty text as the first element. Let's remove it.
    let parts = case splitParts of
            first : rest -> if T.length first == 0 then rest else first : rest
            otherwise -> otherwise
    pure parts

routePath
    :: ( IsRoute route )
    => Proxy route
    -> RouteInput route
    -> T.Text
routePath route = T.cons '/' . mconcat . intersperse (T.pack "/") . routePathParts route

webApp
    :: ( MatchRoute router )
    => Window
    -> router
    -> ReaderArrow Window (Flow ()) () Void -- route doesn't match, use this.
    -> MomentIO (Flow () () Void)
webApp window router notFound = do
    (rest, fire) <- newEvent
    let onPopState :: IO ()
        onPopState = do
            let flow = arr (\i -> (i, window)) >>> runReader (start router notFound)
            fire flow
    -- Oddly enough, popstate is also fired when history is pushed.
    -- The web is weird.
    unbind <- liftIO $ on window popState (liftIO onPopState)
    let first = arr (\i -> (i, window)) >>> runReader (start router notFound)
    let seqnc = first |> rest
    varyingFlow seqnc

{-
type Ex1 = Piece "ClientRoute.jsexe" :</> Piece "index.html" :</> Piece "a" :</> Capture Int :</> End
type Ex2 = Piece "ClientRoute.jsexe" :</> Piece "index.html" :</> Piece "b" :</> End
type Ex3 = Piece "c" :</> End

ex1 :: ReaderArrow Window (Flow ()) (RouteInput Ex1) Void
ex1 = proc (i :*: ()) -> do
    () <- liftReader w -< i
    jumpTo combined (Proxy :: Proxy "ex2") -< ()
  where
    w = widgetFlow1 $ \i -> do
            let l = withEvent Click const
                  $ label (always (T.pack (show i)))
            (ev, velem) <- runWidget <$> l
            pure $ (Widget ((), velem), ev)
    

ex2 :: ReaderArrow Window (Flow ()) (RouteInput Ex2) Void
ex2 = proc () -> do
    () <- liftReader w -< ()
    jumpTo combined (Proxy :: Proxy "ex1") -< (42 .*. ())
  where
    w = widgetFlow1 $ \() -> do
            (ev, velem) <- runWidget <$> withEvent Click const (label (always (T.pack "B")))
            pure (Widget ((), velem), ev)

ex3 :: ReaderArrow Window (Flow ()) (RouteInput Ex3) Void
ex3 = undefined -- impureFlow $ \() -> liftIO (putStrLn "Gotcha") >> undefined

type Combined = NamedRoute "ex1" Ex1 :<|> (NamedRoute "ex2" Ex2 :<|> NamedRoute "ex3" Ex3)

combined :: Combined
combined =
         named (Proxy :: Proxy Ex1) ex1
    :<|> named (Proxy :: Proxy Ex2) ex2
    :<|> named (Proxy :: Proxy Ex3) ex3


main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let router :: Combined
        router = combined
    let notFound = liftReader $ widgetFlow1 $ \_ -> do
            w <- label (always (T.pack "Not found"))
            pure (w, never)

    let networkDescription = do

            ui <- webApp webView router notFound
            (_, velem) <- runWidget <$> runFlow ui ()
            _ <- render document body velem
            pure ()

    network <- compile networkDescription
    actuate network
    pure ()
-}

{-
main = do
    print $ routePath (Proxy :: Proxy Ex1) (42 .*. ())
    print $ routePath (Proxy :: Proxy Ex2) ()
    let theFlow = matchRoute combined (Proxy :: Proxy ()) [T.pack "a", T.pack "42"]
    case theFlow of
        Nothing -> putStrLn "Didn't get it"
        Just flow -> do
            putStrLn "Got it"
-}
