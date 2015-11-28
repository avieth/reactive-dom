# reactive-dom

Definitions in this project help the programmer to wire up a reactive DOM user
interface via reactive-banana and ghcjs-dom.

This is **experimental** software, but it's also **working** software, subject
to one restriction: there is an
[issue](https://github.com/ghcjs/shims/pull/25#issuecomment-154876738)
with weak references in GHCJS, but it is resolved in
[this branch](https://github.com/alios/shims/tree/fix-reactive-banana).
Here's a preview of how to use it:

```Haskell
import GHC.TypeLits
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element as Element
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Component
import Reactive.DOM.Component.Label
import Reactive.DOM.Component.TextInput
import Reactive.DOM.Flow
import Examples.Counter

main :: IO ()
main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = mdo

            -- Here we demonstration the construction of a user interface
            -- flow. It's a "click-to-edit" widget. Click on the label, and
            -- it changes to a text box. Change the text box, and it goes back
            -- to a label.
            --
            -- First step: define the components which compose it: a label and
            -- a text input.
            let label = component (ComponentBehavior Label)
                                  -- Take the click event, and refer to it
                                  -- by the type-level symbol "click".
                                  -- The third argument computes the events
                                  -- value in IO from the JavaScript element
                                  -- and event object.
                                  (withEvent (Proxy :: Proxy "click")
                                             Element.click
                                             (const (const (pure ())))
                                             NoEvents
                                  )

            let textInput = component (TextInput)
                                      (withEvent (Proxy :: Proxy "change")
                                                 Element.change
                                                 (const (const (pure ())))
                                                 NoEvents
                                      )

            -- Our components in hand, we must alter their input and output
            -- to fit our flow specification.
            -- The label must take a single JSString, and output a SEvent ().
            let flowLabel = dimap (always)
                                  (getEvent (Proxy :: Proxy "click") . componentOutputevents)
                                  (label)

            -- Getting the text output, which must be SEvent JSString, is a bit
            -- more complicated. We grab the change event, and use <% to sample
            -- the text input box behavior whenever that happens.
            let getOutputText :: ComponentOutput (SBehavior JSString) '[ '("change", ()) ]
                              -> SEvent JSString
                getOutputText output = const <$> componentOutput output
                                             <%  getEvent (Proxy :: Proxy "change") (componentOutputEvents output)

            let flowTextInput = dimap (always)
                                      (getTextOutput)
                                      (textInput)

            -- Now we're ready to state the flow, using arrow notation.
            let myFlow :: Flow JSString Void
                myFlow = proc initialText -> do
                             _ <-        flow flowLabel     <- initialText
                             nextText <- flow flowTextInput <- initialText
                             myFlow <- nextText

            flowElem <- runKleisli (runFlow cteFlow) "Initial text. Click me!")

            _ <- render document body flowElem

            -- We can even render it in multiple places. All instantiations
            -- will always be exactly the same.
            _ <- render docume body flowElem

            return ()

    network <- compile networkDescription
    actuate network

    return ()
```

## Informal overview

### Flow

The ultimate goal of the UI programmer is to define a `Flow s Void`. This is
done via the `Flow` arrow. The name is intended to
be suggestive: a `Flow s t` is a user-input-driven transformation from an `s`
to a `t`. Think about those old Windows software setup wizards. They're simple
examples of flows, which are advanced by button presses, sometimes with
multiple paths (`Flow` is an `ArrowChoice`, so it can express this).
Another example is some user
interface which requires an authentication token `Flow Token Void`. You may
want to compose this with a login `Flow () Token` to obtain `Flow () Void`,
a complete flow.

The function
```Haskell
runFlow :: Flow s Void -> Kleisli MomentIO s (VirtualElement Identity)
```
realizes the flow. That `VirtualElement` can of course be rendered to the DOM
and it will come to life, following it's specification (the flow itself!).

A note on the use of `Void`: picking `Void` for the second
type parameter means that the resulting UI component has to output an
`SEvent Void`. This admits only two possibilities: either the event never fires,
leaving a single-component flow, or the event fires but is connected back to
some earlier part of the same flow, yielding a looping flow with no loose
ends. Check out the [source](Reactive/DOM/Flow.hs) to see how this all falls
into place. It's actually really cool.

### Component

It's great that `Flow` is an arrow, but the category/arrow combinators alone
are useless; we couldn't possibly use them to construct a `Flow s Void`, since
we can't construct an `s -> Void`. But this is expected, because any useful
`Flow` ought to contain enough to data to show something on screen. That's
specified by a `Component`, and injected to a `Flow` via

```Haskell
flow :: Component s (SEvent t) -> Flow s t
```

A `Component s t` is rather complicated, but I think it can be well described
by an instance and a function:

```Haskell
instance Profunctor Component
runComponent :: Component s t -> s -> MomentIO (t, VirtualElement Identity)
```

Like a `Flow`, a `Component` can produce a `VirtualElement`, so there's our
user-facing part. It's a `Profunctor`, so it's a *bit* like an `Arrow` but not
quite, since it lacks an `id`, and there's definitely no way to give `arr`. Its
`Profunctor`ness is helpful, though, for specializing the input and generalizing
the output of a given `Component`; it makes them more recyclable.
Now we see one way in which a `Flow s Void` could arise: the `Component` gives
`never :: SEvent Void`.

Since user interface components **must** be extensible (every software project
will want its own domain of discourse for this) components are actually
specified by a typeclass:

```Haskell
class IsComponent component where
    type ComponentOutputT component :: *
    type ComponentInputT component :: *
    makeComponent
        :: component
        -> ComponentOutputT component
        -> MomentIO (ComponentOutputT component, VirtualElement Identity)
```

Simple components like a label or button implement this class directly, using
the `Reactive.DOM.Node` module to manually construct `VirtualElement`s.

There are, however, some special `IsComponent` instances out there. In
particular, this one:

```Haskell
data Knot subComponent where
    Knot :: subComponent
         -> (ComponentOutputT subComponent -> ComponentInputT subComponent)
         -> Knot subComponent
```

Yes, to make a `Knot subComponent`, you must define the inputs of the
subcomponent using only the outputs of that very same subcomponent. If you can
do this in a sufficiently lazy way, then you're good to go; you've made a
self-referential knot component! But you don't have to worry about tying it
all together, because this is done once, in the `IsComponent` instance for
`Knot subComponent`. So long as `subComponent` has an `IsComponent` instance,
and you've lazily defined its input by its output, the knot will work.
