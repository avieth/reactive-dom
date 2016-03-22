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

class KnownSymbol tag => W3CTag tag where
    w3cTagName :: Tag tag -> String

--type Div = Tag "div"
instance W3CTag "div" where
    w3cTagName _ = "div"

instance W3CTag "a" where
    w3cTagName _ = "a"

--type P = Tag "p"
instance W3CTag "p" where
    w3cTagName _ = "p"

--type Span = Tag "span"
instance W3CTag "span" where
    w3cTagName _ = "span"

--type H1 = Tag "h1"
instance W3CTag "h1" where
    w3cTagName _ = "h1"

instance W3CTag "h2" where
    w3cTagName _ = "h2"

instance W3CTag "h3" where
    w3cTagName _ = "h3"

instance W3CTag "h4" where
    w3cTagName _ = "h4"

instance W3CTag "hr" where
    w3cTagName _ = "hr"

--type Input = Tag "input"
instance W3CTag "input" where
    w3cTagName _ = "input"

--type Form = Tag "form"
instance W3CTag "form" where
    w3cTagName _ = "form"

--type Button = Tag "button"
instance W3CTag "button" where
    w3cTagName _ = "button"

instance W3CTag "img" where
    w3cTagName _ = "img"

instance W3CTag "br" where
    w3cTagName _ = "br"
