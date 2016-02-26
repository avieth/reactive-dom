{-|
Module      : Reactive.DOM.Children.MonotoneList
Description : Definition of the MonotoneList children container.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Reactive.DOM.Children.MonotoneList where

import Reactive.DOM.Internal.Mutation
import Reactive.DOM.Children.Cardinality

data MonotoneList t = MonotoneListNil | MonotoneListAppend (MonotoneList t) [t]

monotoneList :: [t] -> Mutation t (Cardinality Zero) MonotoneList
monotoneList ts = Mutation $ \_ ->
    (MonotoneListAppend MonotoneListNil ts, AppendChild <$> ts)

monotoneListAppend :: [t] -> Automutation t MonotoneList
monotoneListAppend ts = Mutation $ \mlist ->
    (MonotoneListAppend mlist ts, AppendChild <$> ts)
