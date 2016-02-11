{-|
Module      : Examples.Counter
Description : Construction of a counter widget.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Counter where

import Data.Monoid (mempty)
import qualified Data.Text as T (pack)
import Reactive.DOM.Widget
import Reactive.Sequence
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import GHCJS.Types (JSString)

counter :: Int -> WidgetConstructor (Event Int)
counter initial = knot $ \(~(currentValueBehavior, incrEvent, decrEvent)) -> do

    -- Every change to the counter value: either an increment or a
    -- decrement.
    let changes :: Event (Int -> Int)
        changes = unionWith const (const ((+) 1) <$> incrEvent) (const (flip (-) 1) <$> decrEvent)

    let currentValueEvent :: Event Int
        currentValueEvent = flip ($) <$> currentValueBehavior <@> changes

    let countSequence :: Sequence Int
        countSequence = initial |> currentValueEvent

    -- A label to show the count.
    let display :: WidgetConstructor ()
        display = label (T.pack . show <$> countSequence)

    -- Two buttons: one showing plus and one showing minus.
    -- withEvent extract their click events; the parameter const determines
    -- how we combine that event with the value contained in button:
    --   button :: Sequence Text -> WidgetConstructor ()
    -- so in this case
    --   const :: Event () -> () -> Event ()
    -- i.e. we throw away the useless () that comes from button.
    let bplus :: WidgetConstructor (Event ())
        bplus = withEvent Click const (button (always (T.pack "+")))
    let bminus :: WidgetConstructor (Event ())
        bminus = withEvent Click const (button (always (T.pack "-")))

    -- Roll up the three WidgetConstructors into one WidgetConstructor, via the
    -- applicative variant.
    let props = always mempty
    let attrs = always mempty
    let style = always mempty
    let combined :: WidgetConstructor (Event (), Event ())
        combined = runApplicativeWidget (T.pack "div") props attrs style
                   $ (,) <$> applicativeWidget bplus
                         <*  applicativeWidget display
                         <*> applicativeWidget bminus

    -- combined is the constructor we wish to give back, but with a different
    -- value inside (an Event Int rather than (Event (), Event ())). We also
    -- need to come up with those recursive parameters currentValueBehavior,
    -- incrEvent, and decrEvent. To that end, we use
    --
    --   withValue :: WidgetConstructor t
    --             -> (t -> MomentIO r)
    --             -> WidgetConstructor r
    --
    -- which is kind of like a monadic bind but not quite, since the second
    -- parameter does not give a Widget, just an r. Here's how it's used:
    withValue combined $ \(incrEvent, decrEvent) -> do

        -- behavior :: Sequence t -> (Behavior t, Event t).
        (currentValueBehavior, valueChangeEvent) <- behavior countSequence

        pure (valueChangeEvent, (currentValueBehavior, incrEvent, decrEvent))
