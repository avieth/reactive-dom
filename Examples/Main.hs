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

import Control.Arrow
import Data.Void
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Reactive.DOM.Flow
import Data.Functor.Identity
import Data.Foldable

type One = Identity

data Two t = Two t t

instance Foldable Two where
    foldr f b (Two t1 t2) = f t1 (f t2 b)

main :: IO ()
main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = do

            -- The first parameter, One, constrains the form of the children of
            -- this widget: there will always be precisely one.
            let button :: Widget One (Event ())
                button = knot $ \(~clickEvent) ->
                    tag "button" >>>= \() ->
                    text "Click me!" >>>= \clickMeText ->
                    text "Good job!" >>>= \goodJobText ->
                    children (\_ -> (Identity clickMeText) |> (const (Identity goodJobText) <$> clickEvent)) >>>= \() ->
                    event Click >>>= \clickEvent ->
                    ixpure (clickEvent, clickEvent)

            (_, c) <- render document body button
            reactimate (Prelude.print <$> c)

            -- Whereas button is guaranteed to have precisely one child,
            -- elem1 is guaranteed to have precisely two.
            let elem1 :: Widget Two (Event Bool)
                elem1 =
                    tag "div" >>>= \_ ->
                    widget (makeRed >>>> button) >>>= \(btn1, click1) ->
                    widget (makeBlue >>>> button) >>>= \(btn2, click2) ->
                    children (\_ -> always (Two btn1 btn2)) >>>= \_ ->
                    let clicks = unionWith const (const True <$> click1) (const False <$> click2)
                    in  ixpure clicks
                  where
                    makeRed = style (always (Set (makeStyle [("color", "red")])))
                    makeBlue = style (always (Set (makeStyle [("color", "blue")])))

            (_, c') <- render document body elem1
            reactimate (Prelude.print <$> c')

            -- Ok this is cool, BUT there's still the risk of using the
            -- same ElementSchemaChild in two different places.
            -- No way around that, though, as far as I can tell.

            -- Here we use clientWidth and clientHeight. Very cool.
            -- BUT suppose we wanted to instead show the width/height of the
            -- button which was clicked, not the composite. Is it possible?
            -- Well, here we just don't know about the buttons; they're
            -- abstracted by elem1. And that's in some sense a good thing.
            let elem2 :: Widget Two (Event (Double, Double))
                elem2 =
                    elem1 >>>= \click ->
                    clientWidth >>>= \getClientWidth ->
                    clientHeight >>>= \getClientHeight ->
                    let getBoth :: IO (Double, Double)
                        getBoth = (,) <$> getClientWidth <*> getClientHeight
                        eventBoth :: Event (MomentIO (Double, Double))
                        eventBoth = const (liftIO getBoth) <$> click
                    in  momentIO (execute eventBoth)

            (_, c'') <- render document body elem2
            reactimate (Prelude.print <$> c'')

            -- An example of a flow: click the button, and 2 buttons appear.
            -- Click either of those, and we go back to the start.
            let flow1 :: Flow () () Void
                flow1 = proc () -> do
                            () <- widgetFlow' button -< ()
                            _ <- widgetFlow' elem2 -< ()
                            flow1 -< ()

            let flow1Widget = runFlow flow1 ()
            (_, _) <- render document body flow1Widget

            pure ()

    network <- compile networkDescription
    actuate network

    pure ()
