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
    , ComponentOutput
    , componentOutput
    , componentOutputEvents
    , component
    , runComponent
    , styleComponent
    , withEvent
    , getEvent
    , ComponentEvents(..)
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

type family NewEventName (name :: Symbol) (events :: [(Symbol, *)]) :: Bool where
    NewEventName name '[] = 'True
    NewEventName name ( '(name, t) ': rest ) = 'False
    NewEventName name ( '(name', t) ': rest ) = NewEventName name rest

type family EventType (name :: Symbol) (events :: [(Symbol, *)]) :: * where
    EventType name ( '(name, t) ': rest ) = t
    EventType name ( '(name', t) ': rest ) = EventType name rest

class HasEvent (name :: Symbol) (events :: [(Symbol, *)]) where
    getEvent
        :: Proxy name
        -> ComponentEventsOutput events
        -> SEvent (EventType name events)

instance {-# OVERLAPS #-} HasEvent name ( '(name, t) ': rest ) where
    getEvent _ (HandleEventOutput _ sevent _) = sevent

instance {-# OVERLAPS #-}
    ( HasEvent name rest
    , EventType name rest ~ EventType name ( '(name', t) ': rest )
    ) => HasEvent name ( '(name', t) ': rest )
  where
    getEvent proxy (HandleEventOutput _ _ rest) = getEvent proxy rest

data ComponentEvents (events :: [(Symbol, *)]) where
    NoEvents :: ComponentEvents '[]
    HandleEvent
        :: ( NewEventName symbol events ~ 'True
           , IsEvent event
           )
        => Proxy symbol
        -> EventName Element.Element event
        -> (Element.Element -> event -> IO t)
        -> ComponentEvents events
        -> ComponentEvents ( '(symbol, t) ': events)

-- You can wire up events whenever the events are showing in the output.
withEvent
    :: ( NewEventName symbol events ~ 'True
       , IsEvent event
       )
    => Proxy symbol
    -> EventName Element.Element event
    -> (Element.Element -> event -> IO r)
    -> ComponentEvents events
    -> ComponentEvents ( '(symbol, r) ': events )
withEvent = HandleEvent

data ComponentEventsOutput events where
    NoEventsOutput :: ComponentEventsOutput '[]
    HandleEventOutput
        :: (
           )
        => Proxy name
        -> SEvent t
        -> ComponentEventsOutput events
        -> ComponentEventsOutput ( '(name, t) ': events )

wireComponentEvents
    :: ComponentEvents events
    -> VirtualElement Identity
    -> MomentIO (ComponentEventsOutput events)
wireComponentEvents term velem = case term of
    NoEvents -> pure NoEventsOutput
    HandleEvent proxy eventName handler rest ->
        HandleEventOutput proxy <$> (virtualEvent velem eventName handler)
                                <*> (wireComponentEvents rest velem)

data ComponentOutput t events = ComponentOutput {
      componentOutput :: t
    , componentOutputEvents :: ComponentEventsOutput events
    }

data Component s t where
    Component
        :: ( IsComponent component
           )
        => component
        -> (s -> ComponentInputT component)
        -> (ComponentOutput (ComponentOutputT component) events -> t)
        -> SBehavior Style
        -> ComponentEvents events
        -> Component s t

instance Profunctor Component where
    dimap l r component = case component of
        Component c f g s e -> Component c (f . l) (r . g) s e

-- | This instance allows us to nest Components.
instance IsComponent (Component s t) where
    type ComponentInputT (Component s t) = s
    type ComponentOutputT (Component s t) = t
    makeComponent = runComponent

component
    :: forall (events :: [(Symbol, *)]) component .
       ( IsComponent component
       )
    => component
    -> ComponentEvents events
    -> Component (ComponentInputT component) 
                 (ComponentOutput (ComponentOutputT component) events)
component c = Component c id id (always mempty)

runComponent :: Component s t -> s -> MomentIO (t, VirtualElement Identity)
runComponent (Component component inp out style events) input = do
    (output, velem) <- makeComponent component (inp input)
    let styledVelem = velemMergeStyle style velem
    events' <- wireComponentEvents events velem
    let packagedOutput = ComponentOutput output events'
    pure (out packagedOutput, styledVelem)

styleComponent
    :: ( Bundleable f Identity g Identity
       , BundlesTo (Sequence f g) (SBehavior) ~ SBehavior
       )
    => Sequence f g Style
    -> Component s t
    -> Component s t
styleComponent sequence (Component c f g s es) = Component c f g s' es
  where
    s' :: SBehavior Style
    s' = (<>) <$> sequence <%> s

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
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
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
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
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
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . node <$> children))

        pure (output, velem)
