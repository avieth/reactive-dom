{-|
Module      : 
Description : 
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
import Reactive.DOM.Node
import Reactive.DOM.Children.Algebraic
import Data.Profunctor
import Data.Semigroup
import Data.List.NonEmpty as NonEmpty

-- | A tabular widget. The input NonEmpty gives pairs of widgets: first is
--   the tab, second is the associated content. When the tag is clicked, the
--   associated content is shown and all other contents are hidden.
--
--   TODO permit custom styling for active tabs.
--   TBD maybe factor out the clicking. Perhaps just demand that the tab
--   parts give some Event () ? Then they can style themselves.
tabular
    :: forall tag1 tag2 s1 s2 t1 t2 .
       ( ElementEvent Click tag1
       , W3CTag tag1
       , W3CTag tag2
       , Monoid t1
       , Monoid t2
       )
    => NonEmpty (Widget tag1 s1 t1, Widget tag2 s2 t2)
    -> OpenWidget (s1, s2) (t1, t2)
tabular things = rmap outputter (tieKnot composite knotTyer)

  where

    tabPart :: OpenWidget s1 (t1, SemigroupEvent (First Int))
    tabPart = widgetProductUniform (ClosedWidget (Tag :: Tag tag1) <$> tabs)

    contentPart :: OpenWidget (s2, Event (Int, Int)) t2
    contentPart = widgetProductUniform (ClosedWidget (Tag :: Tag tag2) <$> contents)

    composite :: OpenWidget (s1, (s2, Event (Int, Int)))
                            ((t1, SemigroupEvent (First Int)), t2)
    composite = tabPart `widgetProduct` contentPart

    -- Define input in terms of one piece of the output, for use by
    -- tieKnot.
    knotTyer
        :: MonadMoment m =>
           ((t1, SemigroupEvent (First Int)), t2)
        -> (s1, s2)
        -> m (s1, (s2, Event (Int, Int)))
    knotTyer ~((_, sev), _) ~(s1, s2) = do
        let ev = getFirst <$> runSemigroupEvent sev
        be <- stepper 0 ev
        let ev' = (,) <$> be <@> ev
        pure (s1, (s2, ev'))

    -- Get rid of the semigroup event from the output. Once it is used by
    -- tieKnot via knotTyer it's of no use anymore.
    outputter :: ((t1, SemigroupEvent (First Int)), t2) -> (t1, t2)
    outputter ((t1, _), t2) = (t1, t2)

    tabs :: NonEmpty (Widget tag1 s1 (t1, SemigroupEvent (First Int)))
    tabs = withClickEvent <$> NonEmpty.zip (fst <$> things) (0 :| [1..])

    withClickEvent
        :: (Widget tag1 s1 t1, Int)
        -> Widget tag1 s1 (t1, SemigroupEvent (First Int))
    withClickEvent (w, i) = w `modify` clickEvent i

    clickEvent :: Int -> Modifier tag1 t1 (t1, SemigroupEvent (First Int))
    clickEvent i = modifier $ \x -> do
        click <- event Click
        pure (x, SemigroupEvent (fmap (const (First i)) click))

    contents :: NonEmpty (Widget tag2 (s2, Event (Int, Int)) t2)
    contents = withFocusEvent <$> NonEmpty.zip (snd <$> things) (0 :| [1..])

    withFocusEvent
        :: (Widget tag2 s2 t2, Int)
        -> Widget tag2 (s2, Event (Int, Int)) t2
    withFocusEvent (w, i) = lmap' (focus i) w

    -- Given an index, and an event indicating which indexes should be hidden
    -- and shown, style a widget to respect the hide/show via the CSS display
    -- directive.
    focus :: Int -> (s2, Event (Int, Int)) -> ElementBuilder tag2 s2
    focus myIndex (s2, ev) = do
        let hideMe = filterE ((==) myIndex . fst) ev
        let showMe = filterE ((==) myIndex . snd) ev
        let combined = unionWith const
                                (const (Unset hideStyle) <$> showMe)
                                (const (Set hideStyle) <$> hideMe)
        -- Everything but the 0th is initially set to hidden.
        let initial = if myIndex /= 0 then Set hideStyle else NoOp
        style (initial |> combined)
        pure s2

    hideStyle = makeStyle [("display", "none")]
