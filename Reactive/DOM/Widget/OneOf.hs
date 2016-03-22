{-|
Module      : Reactive.DOM.Widget.OneOf
Description : The oneOf combinator.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactive.DOM.Widget.OneOf where

import Reactive.DOM.Node
import Reactive.DOM.Children.Algebraic
import Reactive.Sequence
import Reactive.Banana

-- | A list of widgets such that precisely one is shown at any given time.
--   The choices are determined by some bounded enum with order.
--   Which one is shown is determined by the second component of the input:
--   the initial one to show, and an event indicating which one should be
--   shown. This combinator takes care of the show/hide CSS logic. All outputs
--   of the widgets are monoidally combined.
oneOf
    :: forall enum s t .
       ( Monoid t
       , Enum enum
       , Bounded enum
       , Ord enum
       )
    => (enum -> ClosedWidget s t)
    -> OpenWidget (s, Sequence enum) t
oneOf mkThing = contents `modifyl` modifier getInput

  where

    things :: [(enum, ClosedWidget s t)]
    things = (\e -> (e, mkThing e)) <$> [minBound..maxBound]

    contents :: OpenWidget (s, enum, Event (enum, enum)) t
    contents = widgetProductUniform (withFocusEvent <$> things)

    getInput
        :: forall tag .
           (s, Sequence enum)
        -> ElementBuilder tag (s, enum, Event (enum, enum))
    getInput (s, seqnc) = do
        let initial = sequenceFirst seqnc
        changes <- sequenceChanges seqnc
        pure (s, initial, changes)

    withFocusEvent
        :: forall s t .
           (enum, ClosedWidget s t)
        -> ClosedWidget (s, enum, Event (enum, enum)) t
    withFocusEvent (enum, ClosedWidget tag w) =
        ClosedWidget tag (w `modifyl` modifier (focus enum))

    -- Given an index, and an event indicating which indexes should be hidden
    -- and shown, style a widget to respect the hide/show via the CSS display
    -- directive.
    focus
        :: forall s tag .
           W3CTag tag
        => enum
        -> (s, enum, Event (enum, enum))
        -> ElementBuilder tag s
    focus myIndex (s, initial, ev) = do
        let hideMe = filterE ((==) myIndex . fst) ev
        let showMe = filterE ((==) myIndex . snd) ev
        let combined = unionWith const
                                (const (Unset hideStyle) <$> showMe)
                                (const (Set hideStyle) <$> hideMe)
        -- Everything but the 0th is initially set to hidden.
        -- We must ensure that this style overrides any other display style
        -- directives. Possible?
        let initialStyle = if myIndex /= initial then Set hideStyle else NoOp
        style (initialStyle |> combined)
        pure s

    hideStyle = makeStyle [("display", "none")]
