{-|
Module      : Examples.Calculator
Description : Simple 2-place calculator example.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE OverloadedStrings #-}

module Examples.Calculator where

import Prelude hiding (span)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Widget.Primitive
import Reactive.DOM.Children.NodeList
import Reactive.DOM.Children.Algebraic
import qualified Data.Text as T

-- | A reactive calculator Widget. The sum of 2 numbers is shown.
--
--   We start with a composite of 4 Widgets, compute in a MonadMoment with
--   their output via rmap', then loop that output back to its input via
--   tie (we tie the knot).
calculator :: Widget () (Event (Maybe Double))
calculator = tie (rmap' makeOutput composite) (pure . tieKnot)

  where

    -- A composite of 4 Widgets: number input, plus sign, number input,
    -- number output. Their inputs and outputs are clumsily bundled into
    -- tuples.
    composite :: Widget ((), ((), ((), (T.Text, Event T.Text))))
                        (Event (Maybe Double), ((), (Event (Maybe Double), ())))
    composite =
        (liftUI numberInput)
        `widgetProduct`
        ((liftUI plusSign)
        `widgetProduct`
        (((liftUI numberInput)
        `widgetProduct`
        numberOutput)))

    -- From the output of the composite we can derive our desired final output:
    -- an event giving the sum.
    makeOutput :: MonadMoment m
               => (Event (Maybe Double), ((), (Event (Maybe Double), ())))
               -> m (Event (Maybe Double))
    makeOutput (evLeft, (_, (evRight, _))) = do
        beLeft <- stepper Nothing evLeft
        beRight <- stepper Nothing evRight
        -- For each input change event, we use the current value from the
        -- other box to compute a new sum.
        let leftSum = (\x y -> (+) <$> x <*> y) <$> beRight <@> evLeft
        let rightSum = (\x y -> (+) <$> x <*> y) <$> beLeft <@> evRight
        pure (unionWith const leftSum rightSum)

    -- Since the composite has nontrivial input--namely a sequence of T.Text
    -- values for the numberOutput--we must tie the knot before we can use it
    -- to make a UI. That's to say, we must describe how to produce the input
    -- of the Widget from its own output. But that's easy: we can derive
    -- the text from the sum event.
    tieKnot :: Event (Maybe Double) -> ((), ((), ((), (T.Text, Event T.Text))))
    tieKnot ev = ((), ((), ((), ("", maybe "Bad input" (T.pack . show) <$> ev))))

    numberInput :: UI (Event (Maybe Double))
    numberInput = (fmap . fmap) readDouble textInput

    plusSign :: UI ()
    plusSign = span (constantText " + ")

    numberOutput :: Widget (T.Text, Event T.Text) ()
    numberOutput = widget $ \(~(initialTxt, evTxt), _) -> pure (
          ()
        , children (nodeList [newChild (span (constantText initialTxt))])
                   (pure . nodeList . pure . newChild . span . constantText <$> evTxt)
        )

    readDouble :: T.Text -> Maybe Double
    readDouble txt = case reads (T.unpack txt) of
        [(i, "")] -> Just i
        _ -> Nothing
