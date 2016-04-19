{-|
Module      : Example.Main
Description : Show various examples.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding ((.), id, div)
import Control.Category
import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import Data.Void
import Data.Profunctor
import Data.Functor.Compose
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Control.Arrow.Flow
import Reactive.DOM.WidgetFlow
import Reactive.DOM.Widget.Common
import Reactive.DOM.Widget.CardinalTransition
--import Examples.Calculator
import System.Mem

main :: IO ()
main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = do

            let makeDirection :: KeydownData -> Maybe (Direction ())
                makeDirection kd =
                    let key = keydownDataKey kd
                    in  if key == "Up"
                        then Just (North ())
                        else if key == "Down"
                        then Just (South ())
                        else if key == "Left"
                        then Just (West ())
                        else if key == "Right"
                        then Just (East ())
                        else Nothing

            let theLabel :: Widget "div" () (Event (Union Void (Direction ())))
                theLabel = lmap (const "Hello world!") (div constantText)
                           `modifyr`
                           (modifier $ \_ -> do
                               -- cl <- event Click
                               kp <- windowKeydown
                               pure (eunion never (filterJust (makeDirection <$> kp)))
                           )

            let labelFlowItem :: Flow (WidgetM Void Direction) () ()
                labelFlowItem = FlowM (WidgetM (closeWidget Tag theLabel))

            let theFlow :: Flow (WidgetM Void Direction) () Void
                theFlow = proc () -> do
                    _ <- labelFlowItem -< ()
                    theFlow -< ()

            let theFlowK :: Flow (FlowKleisli (WidgetN Void Direction)) () Void
                theFlowK = flowTrans transWidgetMN theFlow

            let fk :: FlowContinuation (WidgetN Void Direction) Void
                fk = runKleisli (runFlow theFlowK) ()

            let out :: Either Void (UI (Event (Union Void (Direction Void))))
                out = runWidgetFlow (closeWidget (Tag :: Tag "div") . runDirectedWidgetFlow) fk

            let out' :: UI (Event (Union Void (Direction Void)))
                out' =  either absurd id out

           {-
            let out :: Either Void (UI (Event (Union Void Void)))
                out = runWidgetFlow runSimpleFlow (flowContinuationTrans (transWidgetN unionTrans) fk)
            let out' :: UI (Event (Union Void Void))
                out' = either absurd id out
             

            -}
            _ <- reactiveDom document body (always out')

            {-
            closed <- runDirectedFlow id theFlow ()
            case closed of
                Left t -> pure ()
                Right w -> do
                    _ <- reactiveDom document body (always w)
                    pure ()-}

            {-
            let picker = lmap (const (always Nothing)) datetimeLocalInputVarying
                         `modifyr`
                         (modifier $ \_ -> event Keydown)
            seqnc <- reactiveDom document body (always (closeWidget Tag picker))
            ev <- sequenceSwitchE seqnc
            reactimate (Prelude.print . keydownDataKey <$> ev)
            reactimate (const (Prelude.print ()) <$> (filterE ((==) "U+0041" . keydownDataKey) ev))
            -}

            pure ()

    network <- compile networkDescription
    actuate network

    pure ()
