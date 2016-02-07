{-|
Module      : Reactive.DOM.Component
Description : Definition of IsComponent, Component.
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

module Reactive.DOM.Component (

      IsComponent(..)
    , Component
    , component
    , runComponent
    , componentEvent
    , componentStyle
    , componentAttributes
    , ComponentBehavior(..)
    , ComponentProduct(..)
    , ComponentSum(..)
    , Knot(..)
    , Switched(..)

    ) where

import GHC.TypeLits
import Data.Void
import Data.Proxy
import Data.String (fromString)
import Data.Functor.Identity
import Data.Profunctor
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

class IsComponent t where
    type ComponentInputT t :: *
    type ComponentOutputT t :: *
    makeComponent
        :: t
        -> ComponentInputT t
        -> MomentIO (ComponentOutputT t, VirtualElement Identity)

data Component s t where
    Component
        :: ( IsComponent component
           )
        => component
        -> (s -> ComponentInputT component)
        -- Output is a Kleisli arrow so that we can, for instance, tack on
        -- DOM events as part of the output. Only benign side-effects should
        -- be performed here.
        -> ((ComponentOutputT component, VirtualElement Identity) -> MomentIO t)
        -> SBehavior Attributes
        -> SBehavior Style
        -> Component s t

instance Profunctor Component where
    dimap l r component = case component of
        Component c f g a s -> Component c (f . l) (fmap r . g) a s

instance Functor (Component s) where
    fmap = rmap

-- | This instance allows us to nest Components.
instance IsComponent (Component s t) where
    type ComponentInputT (Component s t) = s
    type ComponentOutputT (Component s t) = t
    makeComponent = runComponent

component
    :: forall component .
       ( IsComponent component
       )
    => component
    -> Component (ComponentInputT component) 
                 (ComponentOutputT component)
component c = Component c id (pure . fst) (always mempty) (always mempty)

runComponent :: Component s t -> s -> MomentIO (t, VirtualElement Identity)
runComponent (Component component inp out attributes style) input = do
    (output, velem) <- makeComponent component (inp input)
    let attributesVelem = velemMergeAttributes attributes velem
    let styledVelem = velemMergeStyle style velem
    output' <- out (output, velem)
    pure (output', styledVelem)

{-
-- | Instructions on which DOM events to bind, and how to merge these events
--   with the output of a component.
data ComponentEvents s t where
    NoEvents :: ComponentEvents s s
    HandleEvent
        :: ( IsEvent event
           )
        => EventName Element.Element event
        -> (Element.Element -> event -> IO r)
        -> (u -> SEvent r -> t)
        -> ComponentEvents s u
        -> ComponentEvents s t

-- | This wires all of the events from a ComponentEvents datatype, producing
--   ultimately a function which will transform the original output of the
--   component into one which is a function of those events.
wireComponentEvents
    :: forall s t . 
       ( )
    => ComponentEvents s t
    -> VirtualElement Identity
    -> MomentIO (s -> t)
wireComponentEvents cevents velem = case cevents of
    NoEvents -> pure id
    HandleEvent eventName mk (f :: u -> SEvent r -> t) rest -> do
        vevent :: SEvent r <- virtualEvent velem eventName mk
        vevents :: s -> u <- wireComponentEvents rest velem
        pure ((flip f vevent) . vevents)
-}

componentEvent
    :: forall event s u r t .
       ( IsEvent event
       )
    => EventName Element.Element event
    -> (Element.Element -> event -> IO r)
    -> (u -> SEvent r -> t)
    -> Component s u
    -> Component s t
componentEvent eventName mk merge (Component c f g a s) = Component c f g' a s
  where
    g' (u, velem) = do
        vevent <- virtualEvent velem eventName mk
        rest <- g (u, velem)
        pure (merge rest vevent)

componentStyle
    :: ( Bundleable f Identity g Identity
       , BundlesTo (Sequence f g) (SBehavior) ~ SBehavior
       )
    => Sequence f g Style
    -> Component s t
    -> Component s t
componentStyle sequence (Component c f g a s) = Component c f g a s'
  where
    s' :: SBehavior Style
    s' = (<>) <$> sequence <%> s

componentAttributes
    :: ( Bundleable f Identity g Identity
       , BundlesTo (Sequence f g) (SBehavior) ~ SBehavior
       )
    => Sequence f g Attributes
    -> Component s t
    -> Component s t
componentAttributes sequence (Component c f g a s) = Component c f g a' s
  where
    a' :: SBehavior Attributes
    a' = (<>) <$> sequence <%> a

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

instance IsComponent sub => IsComponent (ComponentBehavior sub) where
    type ComponentInputT (ComponentBehavior sub) = SBehavior (ComponentInputT sub)
    type ComponentOutputT (ComponentBehavior sub) = SBehavior (ComponentOutputT sub)
    makeComponent (ComponentBehavior sub) sbheavior = do
        let components :: SBehavior (MomentIO (ComponentOutputT sub, VirtualElement Identity))
            components = makeComponent sub <$> sbheavior

        let commuted = sequenceCommute (fmap Identity . runIdentity)
                                       (fmap Identity . runIdentity)
                                       components
        let outputs :: SBehavior (ComponentOutputT sub)
            outputs = fst <$> commuted
        let children :: SBehavior (VirtualElement Identity)
            children = snd <$> commuted
        velem <- virtualElement (pure "div")
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure ((pure . node <$> children)))
        pure (outputs, velem)

data ComponentProduct sub = ComponentProduct sub

instance
    ( IsComponentProductParameter sub (IsProduct sub)
    ) => IsComponent (ComponentProduct sub)
  where
    type ComponentInputT (ComponentProduct sub) = ComponentProductParameterInput sub (IsProduct sub)
    type ComponentOutputT (ComponentProduct sub) = ComponentProductParameterOutput sub (IsProduct sub)
    makeComponent (ComponentProduct sub) input = do
        (output, velems) <- makeComponentProduct sub proxy input
        velem <- virtualElement (pure "div")
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (always (node <$> velems)))
        pure (output, velem)
      where
        proxy :: Proxy (IsProduct sub)
        proxy = Proxy

class IsComponentProductParameter product isProduct where
    type ComponentProductParameterInput product isProduct :: *
    type ComponentProductParameterOutput product isProduct :: *
    makeComponentProduct
        :: product
        -> Proxy isProduct
        -> ComponentProductParameterInput product isProduct
        -> MomentIO (ComponentProductParameterOutput product isProduct, [VirtualElement Identity])

instance {-# OVERLAPS #-}
    ( IsComponent c
    ) => IsComponentProductParameter c 'False
  where
    type ComponentProductParameterInput c 'False = ComponentInputT c
    type ComponentProductParameterOutput c 'False = ComponentOutputT c
    makeComponentProduct c _ i = do
        (out, velem) <- makeComponent c i
        pure (out, [velem])

instance {-# OVERLAPs #-}
    ( IsComponent c
    , IsComponentProductParameter cs (IsProduct cs)
    ) => IsComponentProductParameter (c :*: cs) 'True
  where
    type ComponentProductParameterInput (c :*: cs) 'True = (ComponentInputT c) :*: (ComponentProductParameterInput cs (IsProduct cs))
    type ComponentProductParameterOutput (c :*: cs) 'True = (ComponentOutputT c) :*: (ComponentProductParameterOutput cs (IsProduct cs))
    makeComponentProduct (Product (c, cs)) _ (Product (l, r)) = do
        (outl, veleml) <- makeComponent c l
        (outr, velemr) <- makeComponentProduct cs (Proxy :: Proxy (IsProduct cs)) r
        pure (outl .*. outr, veleml : velemr)

-- | ComponentSum p, where p is a product, is a component.
--   It's called ComponentSum because its Component instance demands only a
--   sum of input, producing a sum of output, but still demands a product of
--   the components.
data ComponentSum sub = ComponentSum sub

instance
    ( IsComponentSumParameter sub (IsProduct sub)
    ) => IsComponent (ComponentSum sub)
  where
    type ComponentInputT (ComponentSum sub) = ComponentSumParameterInput sub (IsProduct sub)
    type ComponentOutputT (ComponentSum sub) = ComponentSumParameterOutput sub (IsProduct sub)
    makeComponent (ComponentSum sub) input = makeComponentSum sub proxy input
      where
        proxy :: Proxy (IsProduct sub)
        proxy = Proxy

-- An auxiliary class to circumvent type family overlap which would otherwise
-- arise when giving instances for Component (ComponentSum sub).
class IsComponentSumParameter sum isProduct where
    type ComponentSumParameterInput sum isProduct :: *
    type ComponentSumParameterOutput sum isProduct :: *
    makeComponentSum
        :: sum
        -> Proxy isProduct
        -> ComponentSumParameterInput sum isProduct
        -> MomentIO (ComponentSumParameterOutput sum isProduct, VirtualElement Identity)

instance {-# OVERLAPS #-}
    ( IsComponent c
    ) => IsComponentSumParameter c 'False
  where
    type ComponentSumParameterInput c 'False = ComponentInputT c
    type ComponentSumParameterOutput c 'False = ComponentOutputT c
    makeComponentSum c _ i = makeComponent c i

instance {-# OVERLAPs #-}
    ( IsComponent c
    , IsComponentSumParameter cs (IsProduct cs)
    ) => IsComponentSumParameter (c :*: cs) 'True
  where
    type ComponentSumParameterInput (c :*: cs) 'True = (ComponentInputT c) :+: (ComponentSumParameterInput cs (IsProduct cs))
    type ComponentSumParameterOutput (c :*: cs) 'True = (ComponentOutputT c) :+: (ComponentSumParameterOutput cs (IsProduct cs))
    makeComponentSum (Product (c, cs)) _ (Sum sum) = case sum of
        Left i -> do
            (out, velem) <- makeComponent c i
            pure (Sum (Left out), velem)
        Right is -> do
            (out, velem) <- makeComponentSum cs (Proxy :: Proxy (IsProduct cs)) is
            pure (Sum (Right out), velem)

-- Can't we make this stuff simpler?
--
--   knot :: Component s t -> (t -> s) -> Component () t
--   Or
--   knot :: (t -> Component t) -> Component t
--
--   data Component t = Component {
--         runComponent :: MomentIO (t, VirtualElement)
--       }
--
--   knot :: (t -> Component t) -> Component t
--   knot rec = Component $ mdo
--       (t, velem) <- runComponent (rec t)
--       pure (t, velem)
--
-- Issue with this one: no types to distinguish components!
--
--   label :: SBehavior Text -> Component ()
--   label stext = Component $ do
--       velem <- ...
--       pure ((), velem)
--
-- Could throw on a phantom type in front of @t@.
--
--   list :: SBehavior [Component t] -> Component [t]
--   list scomponents = Component $ do
--       let subs :: SBehavior (MomentIO [(t, VirtualElement)])
--           subs = fmap (traverse runComponent) scomponents
--       -- commuted :: SBheavior [(t, VirtualElement)]
--       commuted <- scommute subs
--       -- (fmap . fmap) fst commuted :: SBehavior [t]
--       -- (fmap . fmap) snd commuted :: SBehavior [VirtualElement]
--       velem <- ...
--       pure (ts, velem)

-- | Indicates that a subcomponents input should be defined recursively from
--   its output.
data Knot sub where
    Knot
        :: sub
        -> (ComponentOutputT sub -> ComponentInputT sub)
        -> Knot sub

instance IsComponent sub => IsComponent (Knot sub) where
    -- No need for any information it, since output completely determines input.
    type ComponentInputT (Knot sub) = ()
    type ComponentOutputT (Knot sub) = (ComponentInputT sub, ComponentOutputT sub)
    makeComponent (Knot subComponent makeInput) () = mdo
        -- Note the recursive definition. Your makeInput function must only
        -- use the output lazily (and be sure to use a lazy pattern).
        let inputRec :: ComponentInputT sub
            inputRec = makeInput output
        subComponentOut <- makeComponent subComponent inputRec
        let output = fst subComponentOut
        let velem = snd subComponentOut
        return ((inputRec, output), velem)

-- | Switched components are time varying-components, which change form based
--   on their output events. To make them you must give a transition function
--   from the components output to an event giving input. When that event
--   fires, the component is recomputed with that input and shown, and the
--   transition function runs again to produce the next change event.
--
--   This is most interesting when sub ~ ComponentSum (p_1 :*: ... :*: p_n)
--   in which case it will switch between multiple different components.
--   This can express, for example, an n > 2-phase UI in which the first
--   phase is a login screen, and the next phase is selected only when that
--   login component determines it's ok to show it (credential challenger
--   can be abstracted and provided at a higher level).
--
--
--   SEE ALSO Reactive.DOM.Flow which defines similar functionality, but using
--   a Category/Arrow.
data Switched sub where
    Switched
        :: (ComponentOutputT sub -> SEvent (ComponentInputT sub))
        -> (sub)
        -> Switched sub

instance (IsComponent sub) => IsComponent (Switched sub) where
    type ComponentInputT (Switched sub) = ComponentInputT sub
    type ComponentOutputT (Switched sub) = SBehavior (ComponentOutputT sub)
    makeComponent (Switched transition sub) input = mdo

        initial :: (ComponentOutputT sub, VirtualElement Identity)
            <- makeComponent sub input

        let outputBehavior :: SBehavior (ComponentOutputT sub, VirtualElement Identity)
            outputBehavior = initial |> outputEvent

        let outputLagged :: SBehavior (ComponentOutputT sub, VirtualElement Identity)
            outputLagged = lag outputBehavior

        let inputEvent :: SEvent (ComponentInputT sub)
            inputEvent = switch (flip const) ((transition . fst) <$> outputLagged)

        -- This must go after inputEvent, since it forces both parts of it.
        let outputEvent :: SEvent (ComponentOutputT sub, VirtualElement Identity)
            outputEvent = sequenceCommute (const (pure (Const ())))
                                          (fmap Identity . runIdentity)
                                          (makeComponent sub <$> inputEvent)

        let children :: SBehavior (VirtualElement Identity)
            children = snd <$> outputBehavior

        let output :: SBehavior (ComponentOutputT sub)
            output = fst <$> outputBehavior

        velem <- virtualElement (pure "div")
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (always mempty))
                                (pure (pure . node <$> children))

        pure (output, velem)
