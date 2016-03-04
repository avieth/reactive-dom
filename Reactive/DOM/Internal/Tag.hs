{-|
Module      : Reactive.DOM.Internal.Tag
Description : Definition of W3C HTML tags in types.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Reactive.DOM.Internal.Tag where

import GHC.TypeLits
import Data.Proxy

-- | Just proxy but with a more suggestive name.
data Tag (name :: Symbol) = Tag

class KnownSymbol tag => W3CTagName tag where
    w3cTagName :: Tag tag -> String

--type Div = Tag "div"
instance W3CTagName "div" where
    w3cTagName _ = "div"

--type P = Tag "p"
instance W3CTagName "p" where
    w3cTagName _ = "p"

--type Span = Tag "span"
instance W3CTagName "span" where
    w3cTagName _ = "span"

--type H1 = Tag "h1"
instance W3CTagName "h1" where
    w3cTagName _ = "h1"

instance W3CTagName "h2" where
    w3cTagName _ = "h2"

instance W3CTagName "h3" where
    w3cTagName _ = "h3"

instance W3CTagName "h4" where
    w3cTagName _ = "h4"

--type Input = Tag "input"
instance W3CTagName "input" where
    w3cTagName _ = "input"

--type Form = Tag "form"
instance W3CTagName "form" where
    w3cTagName _ = "form"

--type Button = Tag "button"
instance W3CTagName "button" where
    w3cTagName _ = "button"

