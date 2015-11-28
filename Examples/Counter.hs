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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.Counter where

import Data.String (fromString)
import Data.Semigroup (First(..))
import Reactive.Sequence
import Reactive.DOM.Component
import Reactive.DOM.Component.Label
import Reactive.DOM.Component.Button
import Reactive.Banana.Frameworks
import GHCJS.Types (JSString)
-- We use the product type :*: from Algebraic. It's just like a tuple, but
-- more useful at the type level.
import Data.Algebraic.Product

-- We want to say that a counter is built from *ANY* component which has
-- a particular input/output form:
--
--     input : (SBehavior Int)
--     output : (SEvent () :*: SEvent ())
--
-- Thus the user can freely construct some component with this signature, and
-- all the counter component shall do is state the wiring from output to
-- input.
--
-- The Counter then gives back the input of the subcomponent, so that its
-- users can observe the counter.
data Counter sub where
    Counter
        :: ( IsComponent sub
           , ComponentInputT sub ~ SBehavior Int
           , ComponentOutputT sub ~ (SEvent () :*: SEvent ())
           )
        -- It's important to take the SBehavior input here, so that the
        -- Knot's routing function can use it.
        => (SBehavior Int -> Knot sub)
        -> Counter sub

instance IsComponent sub => IsComponent (Counter sub) where
    type ComponentInputT (Counter sub) = SBehavior Int
    type ComponentOutputT (Counter sub) = SBehavior Int
    makeComponent (Counter makeKnot) inputBehavior = do
        ((input, output), velem) <- makeComponent (makeKnot inputBehavior) ()
        -- We choose the input of the subcomponent, recursively determined by
        -- its output, as the output of the Counter.
        pure (input, velem)

counter
    :: ( IsComponent sub
       , ComponentInputT sub ~ SBehavior Int
       , ComponentOutputT sub ~ (SEvent () :*: SEvent ())
       )
    => sub
    -> Counter sub
counter sub = Counter (\reset -> Knot sub (wireItUp reset))
  where
    -- Note the lazy pattern! That's VERY important. Without it, we'll diverge.
    wireItUp :: SBehavior Int
             -> (SEvent () :*: SEvent ())
             -> SBehavior Int
    wireItUp reset ~(Product (incr, decr)) =
        let incrEvent :: SEvent (Int -> Int)
            incrEvent = (const (\x -> x + 1)) <$> incr
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
        -- about the difference). All we say here is that it starts with
        -- whatever value the inputs start, and whenever changes gives us an
        -- event with (Int -> Int), apply it to the current value. Later values
        -- of the input behavior will reset the counter.
            countBehavior :: SBehavior Int
            countBehavior = fixSBehavior $ \sbehavior ->
                let bumps :: SEvent Int
                    -- We must trim the sequence (drop the first element)
                    -- so that we are lazy in the recursive input sbehavior.
                    -- NB the following, in which we trim the sequence directly,
                    -- doesn't work. Somehow the event is discarded and never
                    -- fires. Weird! Could be a problem with %> (bundle).
                    --bumps = ($) <$> changes %> (sequenceTrim sbehavior)
                    bumps = sequenceTrim $ ($) <$> changes %> sbehavior
                in  getFirst <$> ((First <$> reset) <||> (First <$> bumps))
        in  countBehavior
