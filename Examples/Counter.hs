{-|
Module      : Examples.Counter
Description : Construction of a counter component.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Counter where

import Data.String (fromString)
import Data.Semigroup (First(..))
import Reactive.Sequence
import Reactive.DOM.Component
import Reactive.DOM.Component.Label
import Reactive.DOM.Component.Button
import GHCJS.Types (JSString)
-- We use the product type :*: from Algebraic. It's just like a tuple, but
-- more useful at the type level.
import Data.Algebraic.Product

-- What we have in mind is a component with two buttons--increment and
-- decrement--and a label showing the current value. This falls into the
-- Simultaneous type. What we have here makes the following statements:
--
--   - No input is required (just give ()).
--   - The counter is composed of three subcomponents: a Label and two Buttons.
--   - In order to make a counter, you have to describe how to compute inputs
--     for the subcomponents from outputs for those subcomponents. This
--     recursive treatment is essential: the input for the label depends upon
--     the outputs of the buttons, namely their click events.
--
type Counter = Simultaneous () (Label :*: Button :*: Button)

-- With the type given, we have bounds for our implementation.
-- The second parameter of Simultaneous is just the implementations of the
-- subcomponents, in the appropriate order. No problem.
-- The first parameter, however, contains the essence of the counter. It
-- routes outputs of subcomponents to inputs of subcomponents, giving rise
-- to the desired reactive behaviour.
counter :: Counter
counter = Simultaneous (const wireItUp) (label .*. button .*. button)
  where
    -- Note the lazy pattern! That's VERY important. Without it, we'll diverge.
    wireItUp :: (SBehavior JSString :*: SEvent () :*: SEvent ())
             -> (SBehavior JSString :*: SBehavior JSString :*: SBehavior JSString)
    wireItUp ~(Product (_, Product (incr, decr))) = labelBehavior .*. incrLabel .*. decrLabel
      where
        -- The label for the increment button is always "+".
        incrLabel = always "+"
        -- Similarly, the label for the decrement button never changes from "-".
        decrLabel = always "-"
        -- From the increment event (it's the output of the first button in
        -- the product) we can produce an event which increments something by
        -- one.
        incrEvent :: SEvent (Int -> Int)
        incrEvent = (const ((+) 1)) <$> incr
        -- Similarly for the decrement event.
        decrEvent :: SEvent (Int -> Int)
        decrEvent = (const (\x -> x - 1)) <$> decr
        -- Here we merge the increment and decrement events. We appeal to the
        -- semigroup First to disambiguate simultaneous evnts (which probably
        -- won't ever happen, but it's good to be safe). The way we've
        -- written it here, the increment wins over the decrement in case they
        -- happen at the same time.
        changes :: SEvent (Int -> Int)
        changes = getFirst <$> ((First <$> incrEvent) <||> (First <$> decrEvent))
        -- From changes, we can describe the sequence of counter values. We
        -- do so recursively: assume we have the Behavior for this thing,
        -- and use it to define the SBehavior (see reactive-sequence to learn
        -- about the difference). All we say here is that it starts at 0,
        -- and whenever changes gives us an event with (Int -> Int), apply
        -- it to the current value.
        countBehavior :: SBehavior Int
        countBehavior = fixSBehavior' $ \behavior ->
            sEventToSBehavior 0 (($) <$> changes %> behavior)
        -- Finally, the label text is had by dumping the current value to
        -- a JSString.
        labelBehavior :: SBehavior JSString
        labelBehavior = fromString . show <$> countBehavior
