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
--   The widgets in the image of the function on the bounded enum take a
--   Sequence Bool, which will be synthesized here, to indciate whether they are
--   the "active" tab, i.e. the one whose output event has most recently fired.
--   You may want to use this event to derive the widget's style.
--
--   Useful in combination with oneOf from Reactive.DOM.Widget.OneOf, which
--   lends itself well to defining the content which the tabs control.
tabs
    :: forall enum s .
       ( Enum enum
       , Bounded enum
       , Ord enum
       )
    => (enum -> ClosedWidget (s, Sequence Bool) (Event ()))
    -> OpenWidget (s, enum) (Event enum)
tabs mkThing = tieKnot (rmap getOutput contents) loop

  where

    things :: [(enum, ClosedWidget (s, Sequence Bool) (Event ()))]
    things = (\e -> (e, mkThing e)) <$> [minBound..maxBound]

    -- Ok so here we need the s input to be uniform... aha, we must first
    -- generalize the Event Bool to an Event enum.
    contents :: OpenWidget (s, Sequence enum) (SemigroupEvent (First enum))
    contents = widgetProductUniform (withEnumEvent <$> things)

    -- Use the input to wire up the Event Bool, and make the output event
    -- give the enum and wrap it in SemigroupEvent so we can use it in
    -- widgetProductUniform.
    withEnumEvent
        :: forall s .
           (enum, ClosedWidget (s, Sequence Bool) (Event ()))
        -> ClosedWidget (s, Sequence enum) (SemigroupEvent (First enum))
    withEnumEvent (enum, ClosedWidget tag w) =
        ClosedWidget tag w'
      where
        w' = modify (modifier input) (modifier output) w
        output ev = pure (SemigroupEvent (fmap (const (First enum)) ev))
        input (s, seqnc) = do
            let initial = sequenceFirst seqnc
            chngs <- sequenceChanges seqnc
            let pickTrue (old, new) = old /= new && new == enum
            let pickFalse (old, new) = old /= new && old == enum
            let evboolTrue = const True <$> filterE pickTrue chngs
            let evboolFalse = const False <$> filterE pickFalse chngs
            let evbool = unionWith const evboolTrue evboolFalse
            pure (s, (initial == enum) |> evbool)

    -- Use this to tie the output of contents back to its input.
    loop
        :: forall tag .
           Event enum
        -> (s, enum)
        -> ElementBuilder tag (s, Sequence enum)
    loop ev (s, initial) = pure (s, initial |> ev)

    getOutput :: SemigroupEvent (First enum) -> Event enum
    getOutput = fmap getFirst . runSemigroupEvent
