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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Examples.Calculator where

import Prelude hiding (span)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Widget.Common
import Reactive.DOM.Children.NodeList
import Reactive.DOM.Children.Algebraic
import qualified Data.Text as T
import Data.Profunctor

-- | A reactive calculator Widget. The sum of 2 numbers is shown.
--
--   We start with a composite of 4 Widgets, compute in a MonadMoment with
--   their output via rmap', then loop that output back to its input via
--   tie (we tie the knot).
calculator :: OpenWidget () (Event (Maybe Double))
calculator = tieKnot (rmap' makeOutput composite) loop

  where

    -- A composite of 4 Widgets: number input, plus sign, number input,
    -- number output. Their inputs and outputs are clumsily bundled into
    -- tuples.
    composite :: OpenWidget ((), ((), ((), (Event T.Text))))
                            (Event (Maybe Double), ((), (Event (Maybe Double), ())))
    composite =
        (openWidget (closeWidget Tag numberInput))
        `widgetProduct`
        ((openWidget (closeWidget Tag plusSign))
        `widgetProduct`
        (((openWidget (closeWidget Tag numberInput))
        `widgetProduct`
        -- Must set the initial text input here. We can't feed it back
        -- through from the output, since it is not used lazily. The event,
        -- however, *is* used lazily, and indeed must be derived from the
        -- output of the composite.
        lmap (\ev -> ("", ev)) numberOutput)))

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
    loop :: Applicative m => Event (Maybe Double) -> () -> m ((), ((), ((), (Event T.Text))))
    loop ev _ = pure ((), ((), ((), (maybe "Bad input" (T.pack . show) <$> ev))))

    numberInput :: Widget "input" () (Event (Maybe Double))
    numberInput = (fmap . fmap) readDouble textInput

    plusSign :: Widget "span" () ()
    plusSign = span (lmap (const " + ") constantText)

    numberOutput :: OpenWidget (T.Text, Event T.Text) ()
    numberOutput = widget $ \(~(initialTxt, evTxt), _) -> pure (
          ()
        , children (nodeList [makeChild initialTxt])
                   (pure . nodeList . pure . makeChild <$> evTxt)
        )
      where
        makeChild :: T.Text -> SetChild ()
        makeChild txt = newChild (ui (span (lmap (const txt) constantText)))

    readDouble :: T.Text -> Maybe Double
    readDouble txt = case reads (T.unpack txt) of
        [(i, "")] -> Just i
        _ -> Nothing
