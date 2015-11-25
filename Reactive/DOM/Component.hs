{-|
Module      : Reactive.DOM.Component
Description : Definition of components.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Reactive.DOM.Component where

import Data.Void
import Data.String (fromString)
import Data.Functor.Identity
import Data.Semigroup (First(..), getFirst)
import Data.Monoid hiding (Product, First(..), getFirst)
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import GHCJS.Types (JSString)
import qualified GHCJS.DOM.Element as Element
import Data.Algebraic.Product hiding (Component)
import qualified Data.Algebraic.Product as Product
import Data.Algebraic.Sum

class Component t where
    type ComponentInput t :: *
    type ComponentOutput t :: *
    runComponent
        :: t
        -> ComponentInput t
        -> MomentIO (ComponentOutput t, VirtualElement Identity)

-- | A simple component contains direct instructions on how to make a
--   virtual element and output.
data Simple inp out = Simple (inp -> MomentIO (out, VirtualElement Identity))

instance Component (Simple inp out) where
    type ComponentInput (Simple inp out) = inp
    type ComponentOutput (Simple inp out) = out
    runComponent (Simple mk) inp = mk inp

-- | A composite just shuffles the input and output of some other component.
data Composite inp sub out = Composite (inp -> ComponentInput sub)
                                       (sub)
                                       (ComponentOutput sub -> out)

instance Component sub => Component (Composite inp sub out) where
    type ComponentInput (Composite inp sub out) = inp
    type ComponentOutput (Composite inp sub out) = out
    runComponent (Composite mkInput sub mkOutput) input = do
        subComponent <- runComponent sub (mkInput input)
        let subOutput :: ComponentOutput sub
            subOutput = fst subComponent
        let subElem :: VirtualElement Identity
            subElem = snd subComponent
        pure (mkOutput subOutput, subElem)

-- | Represents the application of a sequence of styles. They're merged
--   into the subcomponent's element's existing style.
--
--   TODO similar for properties and attributes.
data ComponentStyle sub where
    ComponentStyle
        -- Need these constraints so we can do velemMergeStyle
        :: ( Unionable f Identity g Identity
           , UnionsTo (Sequence f g) (SBehavior) ~ SBehavior
           )
        => (Sequence f g Style)
        -> sub
        -> ComponentStyle sub

instance Component sub => Component (ComponentStyle sub) where
    type ComponentInput (ComponentStyle sub) = ComponentInput sub
    type ComponentOutput (ComponentStyle sub) = ComponentOutput sub
    runComponent (ComponentStyle styleSequence sub) input = do
        subComponent <- runComponent sub input
        let velem = snd subComponent
        let velem' = velemMergeStyle styleSequence velem
        return (fst subComponent, velem')

-- | Indicates that a subcomponents input should be defined recursively from
--   its output.
data Simultaneous cs where
    Simultaneous :: cs -> (ComponentOutput cs -> ComponentInput cs) -> Simultaneous cs

instance Component cs => Component (Simultaneous cs) where
    type ComponentInput (Simultaneous cs) = ()
    type ComponentOutput (Simultaneous cs) = ComponentOutput cs
    runComponent (Simultaneous cs makeInput) () = mdo
        let input = makeInput output
        subComponent <- runComponent cs input
        let output = fst subComponent
        let velem = snd subComponent
        return (output, velem)

instance (Component c, Component cs) => Component (c :*: cs) where
    type ComponentInput (c :*: cs) = (ComponentInput c) :*: (ComponentInput cs)
    type ComponentOutput (c :*: cs) = (ComponentOutput c) :*: (ComponentOutput cs)
    runComponent (Product (c, cs)) (Product (i, is)) = do
        thisComponent <- runComponent c i
        thatComponent <- runComponent cs is
        let thisOutput = fst thisComponent
        let thatOutput = fst thatComponent
        let thisVelem = snd thisComponent
        let thatVelem = snd thatComponent
        velem <- virtualElement (pure "div")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always [pure (node thisVelem), pure (node thatVelem)]))
        return (thisOutput .*. thatOutput, velem)



type Label = Simple (SBehavior JSString) (SBehavior JSString)

label :: Label
label = Simple makeLabel
  where
    makeLabel :: SBehavior JSString -> MomentIO (SBehavior JSString, VirtualElement Identity)
    makeLabel textSequence = do
        velem <- virtualElement (pure "span")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . pure . text . pure <$> textSequence))
        return (textSequence, velem)

type Button = Simple (SBehavior JSString) (SEvent ())

button :: Button
button = Simple makeButton
  where
    makeButton :: SBehavior JSString -> MomentIO (SEvent (), VirtualElement Identity)
    makeButton textSequence = do
        velem <- virtualElement (pure "button")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . pure . text . pure <$> textSequence))
        click <- virtualEvent velem Element.click (\_ _ -> return ())
        return (eventToSEvent click, velem)

type LabelAndButton = Label :*: Button

labelAndButton :: LabelAndButton
labelAndButton = label .*. button

type Counter = Simultaneous (Label :*: Button :*: Button)

-- This is an excellent example. See how we wire everything up, assuming the
-- outputs and from that giving the inputs. VERY cool!
counter :: Counter
counter = Simultaneous (label .*. button .*. button) wireItUp
  where
    -- Note the lazy match! That's VERY important. Without it, we'll diverge.
    wireItUp :: (SBehavior JSString :*: SEvent () :*: SEvent ())
             -> (SBehavior JSString :*: SBehavior JSString :*: SBehavior JSString)
    wireItUp ~(Product (_, Product (incr, decr))) = Product (labelBehavior, Product (incrLabel, decrLabel))
      where
        incrLabel = always "+"
        decrLabel = always "-"
        incrEvent :: SEvent (Int -> Int)
        incrEvent = (const ((+) 1)) <$> incr
        decrEvent :: SEvent (Int -> Int)
        decrEvent = (const (\x -> x - 1)) <$> decr
        changes :: SEvent (Int -> Int)
        changes = getFirst <$> ((First <$> incrEvent) <||> (First <$> decrEvent))
        countBehavior :: SBehavior Int
        countBehavior = fixSBehavior' $ \behavior ->
            sEventToSBehavior 0 (($) <$> changes %> behavior)
        labelBehavior :: SBehavior JSString
        labelBehavior = fromString . show <$> countBehavior
