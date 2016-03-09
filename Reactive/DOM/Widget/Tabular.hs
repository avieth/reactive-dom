{-|
Module      : Reactive.DOM.Widget.Tabular
Description : Definition of tabs, for tabular widgets.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.DOM.Widget.Tabular where

import Reactive.Sequence
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Children.Algebraic
import Data.Profunctor
import Data.Semigroup
import Data.List.NonEmpty as NonEmpty

-- | Given a total function on some bounded enum, pack the resulting widgets
--   as siblings. It's called tabs because, if the resulting OpenWidget is
--   closed and styled with flex rows, you'll get something resembling tabs.
--
--   The widgets in the image of the function on the bounded enum take an
--   Event Bool, which will be synthesized here, to indciate whether they are
--   the "active" tab, i.e. the one whose output event has most recently fired.
--   You may want to use this event to derive the widget's style.
--
--   The input of the resulting OpenWidget includes an enum to indicate the
--   initial choice.
--
--   Useful in combination with oneOf from Reactive.DOM.Widget.OneOf, which
--   lends itself well to defining the content which the tabs control.
tabs
    :: forall enum s .
       ( Enum enum
       , Bounded enum
       , Ord enum
       )
    => (enum -> ClosedWidget (s, (Bool, Event Bool)) (Event ()))
    -> OpenWidget (s, enum) (Event enum)
tabs mkThing = tieKnot (rmap getOutput contents) loop

  where

    things :: [(enum, ClosedWidget (s, (Bool, Event Bool)) (Event ()))]
    things = (\e -> (e, mkThing e)) <$> [minBound..maxBound]

    -- Ok so here we need the s input to be uniform... aha, we must first
    -- generalize the Event Bool to an Event enum.
    contents :: OpenWidget (s, enum, Event enum) (SemigroupEvent (First enum))
    contents = widgetProductUniform (withEnumEvent <$> things)

    -- Use the input to wire up the Event Bool, and make the output event
    -- give the enum and wrap it in SemigroupEvent so we can use it in
    -- widgetProductUniform.
    withEnumEvent
        :: forall s .
           (enum, ClosedWidget (s, (Bool, Event Bool)) (Event ()))
        -> ClosedWidget (s, enum, Event enum) (SemigroupEvent (First enum))
    withEnumEvent (enum, ClosedWidget tag w) =
        ClosedWidget tag w'
      where
        w' = dimap' input output w
        output ev = pure (SemigroupEvent (fmap (const (First enum)) ev))
        input (s, initial, evenum) = do
            -- tupled is an Event which fires with the previous and current enum
            -- values whenever the input event fires.
            be <- stepper initial evenum
            let tupled = (,) <$> be <@> evenum
            let pickTrue (old, new) = old /= new && new == enum
            let pickFalse (old, new) = old /= new && old == enum
            let evboolTrue = const True <$> filterE pickTrue tupled
            let evboolFalse = const False <$> filterE pickFalse tupled
            let evbool = unionWith const evboolTrue evboolFalse
            pure (s, (initial == enum, evbool))

    -- Use this to tie the output of contents back to its input.
    loop
        :: forall tag .
           Event enum
        -> (s, enum)
        -> ElementBuilder tag (s, enum, Event enum)
    loop ev (s, initial) = pure (s, initial, ev)

    getOutput :: SemigroupEvent (First enum) -> Event enum
    getOutput = fmap getFirst . runSemigroupEvent
