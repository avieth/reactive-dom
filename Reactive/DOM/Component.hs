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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactive.DOM.Component where

import Data.Void
import Data.Proxy
import Data.String (fromString)
import Data.Functor.Identity
import Data.Semigroup (First(..), getFirst)
import Data.Monoid hiding (Product, First(..), getFirst, Sum)
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import GHCJS.Types (JSString)
import GHCJS.DOM.Event (IsEvent)
import GHCJS.DOM.EventTargetClosures (EventName)
import qualified GHCJS.DOM.Element as Element
import Data.Algebraic.Index
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

-- TODO this is too coarse. There's no ADT for all different types of events,
-- so we end up saying MouseEvent, KeyboardEvent, etc. when we should be saying
-- Click, Keypress, etc.
data WithEvent ev t sub = WithEvent (EventName Element.Element ev)
                                    (Element.Element -> ev -> IO t)
                                    sub

instance (IsEvent ev, Component sub) => Component (WithEvent ev t sub) where
    type ComponentInput (WithEvent ev t sub) = ( ComponentInput sub )
    type ComponentOutput (WithEvent ev t sub) = ( SEvent t
                                                , ComponentOutput sub
                                                )
    runComponent (WithEvent ev mk sub) (subInput) = do
        (out, velem) <- runComponent sub subInput
        event <- virtualEvent velem ev mk
        pure ((event, out), velem)

-- | ComponentBehavior sub takes an SBehavior of the input of sub, and gives
--   an SBehavior of its output. Whenever the input SBehavior changes, the
--   component is recomputed, its output is emitted, and its display refreshed.
--
--   This gives another way to make a reactive label: just give a nonreactive
--   label and then use ComponentBehavior. Any advantages either way?
--   Consider the reactive button in the same scenario. Its output is
--   an SEvent (). If we put it into ComponentBehavior then we have to
--   switch the output event... not really a big deal I guess.
--   So, should a component ever take an SBehavior? I suppose only if it has
--   a subcomponent ComponentBehavior which calls for it.
data ComponentBehavior sub = ComponentBehavior sub

instance Component sub => Component (ComponentBehavior sub) where
    type ComponentInput (ComponentBehavior sub) = SBehavior (ComponentInput sub)
    type ComponentOutput (ComponentBehavior sub) = SBehavior (ComponentOutput sub)
    runComponent (ComponentBehavior sub) sbheavior = do
        let components :: SBehavior (MomentIO (ComponentOutput sub, VirtualElement Identity))
            components = runComponent sub <$> sbheavior

        let commuted = sequenceCommute (fmap Identity . runIdentity)
                                       (fmap Identity . runIdentity)
                                       components
        let outputs :: SBehavior (ComponentOutput sub)
            outputs = fst <$> commuted
        let children :: SBehavior (VirtualElement Identity)
            children = snd <$> commuted
        velem <- virtualElement (pure "div")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure ((pure . node <$> children)))
        pure (outputs, velem)

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
--
--   TBD why do we not have an output parameter? Just seems weird that we
--   need the input parameter but not the output.
data Simultaneous inp sub where
    Simultaneous
        :: (inp -> (ComponentOutput sub -> ComponentInput sub))
        -> sub
        -> Simultaneous inp sub

instance Component sub => Component (Simultaneous inp sub) where
    type ComponentInput (Simultaneous inp sub) = inp
    type ComponentOutput (Simultaneous inp sub) = ComponentOutput sub
    runComponent (Simultaneous makeInput cs) input = mdo
        let inputRec = makeInput input output
        subComponent <- runComponent cs inputRec
        let output = fst subComponent
        let velem = snd subComponent
        return (output, velem)

instance {-# OVERLAPS #-} (Component c, Component cs) => Component (c :*: cs) where
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
                                (pure (always [node thisVelem, node thatVelem]))
        return (thisOutput .*. thatOutput, velem)

-- ComponentSum p, where p is a product, is a component.
data ComponentSum sub = ComponentSum sub

-- An auxiliary class to circumvent type family overlap which would otherwise
-- arise when giving instances for Component (ComponentSum sub).
class ComponentSumParameter sum isProduct where
    type ComponentSumParameterInput sum isProduct :: *
    type ComponentSumParameterOutput sum isProduct :: *
    runComponentSum
        :: sum
        -> Proxy isProduct
        -> ComponentSumParameterInput sum isProduct
        -> MomentIO (ComponentSumParameterOutput sum isProduct, VirtualElement Identity)

instance {-# OVERLAPS #-}
    ( Component c
    ) => ComponentSumParameter c 'False
  where
    type ComponentSumParameterInput c 'False = ComponentInput c
    type ComponentSumParameterOutput c 'False = ComponentOutput c
    runComponentSum c _ i = runComponent c i

instance {-# OVERLAPs #-}
    ( Component c
    , ComponentSumParameter cs (IsProduct cs)
    ) => ComponentSumParameter (c :*: cs) 'True
  where
    type ComponentSumParameterInput (c :*: cs) 'True = (ComponentInput c) :+: (ComponentSumParameterInput cs (IsProduct cs))
    type ComponentSumParameterOutput (c :*: cs) 'True = (ComponentOutput c) :+: (ComponentSumParameterOutput cs (IsProduct cs))
    runComponentSum (Product (c, cs)) _ (Sum sum) = case sum of
        Left i -> do
            (out, velem) <- runComponent c i
            pure (Sum (Left out), velem)
        Right is -> do
            (out, velem) <- runComponentSum cs (Proxy :: Proxy (IsProduct cs)) is
            pure (Sum (Right out), velem)

instance
    ( ComponentSumParameter sub (IsProduct sub)
    ) => Component (ComponentSum sub)
  where
    type ComponentInput (ComponentSum sub) = ComponentSumParameterInput sub (IsProduct sub)
    type ComponentOutput (ComponentSum sub) = ComponentSumParameterOutput sub (IsProduct sub)
    runComponent (ComponentSum sub) input = runComponentSum sub proxy input
      where
        proxy :: Proxy (IsProduct sub)
        proxy = Proxy
 
data Switched inp sub where
    Switched
        :: (inp -> ComponentInput sub)
        -> (ComponentOutput sub -> SEvent (ComponentInput sub))
        -> sub
        -> Switched inp sub

instance (Component sub) => Component (Switched inp sub) where
    type ComponentInput (Switched inp sub) = inp
    type ComponentOutput (Switched inp sub) = SBehavior (ComponentOutput sub)
    runComponent (Switched makeInitial transition sub) input = mdo

        initial :: (ComponentOutput sub, VirtualElement Identity)
            <- runComponent sub (makeInitial input)

        let outputBehavior :: SBehavior (ComponentOutput sub, VirtualElement Identity)
            outputBehavior = initial |> outputEvent

        let outputLagged :: SBehavior (ComponentOutput sub, VirtualElement Identity)
            outputLagged = lag outputBehavior

        let inputEvent :: SEvent (ComponentInput sub)
            inputEvent = switch (flip const) ((transition . fst) <$> outputLagged)

        -- This must go after inputEvent, since it forces both parts of it.
        let outputEvent :: SEvent (ComponentOutput sub, VirtualElement Identity)
            outputEvent = sequenceCommute (const (pure (Const ())))
                                          (fmap Identity . runIdentity)
                                          (runComponent sub <$> inputEvent)

        let children :: SBehavior (VirtualElement Identity)
            children = snd <$> outputBehavior

        let output :: SBehavior (ComponentOutput sub)
            output = fst <$> outputBehavior

        velem <- virtualElement (pure "div")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . node <$> children))

        pure (output, velem)
