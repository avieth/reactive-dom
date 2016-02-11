# reactive-dom

Definitions in this project help the programmer to wire up a reactive DOM user
interface, expressed in Haskell via reactive-sequence and ghcjs-dom, and
compiled to JavaScript using ghcjs.

This is **experimental** software, but it's also **working** software!
That's subject to one restriction: there is an
[issue](https://github.com/ghcjs/shims/pull/25#issuecomment-154876738)
with weak references in ghcjs, but it is resolved in
[this branch](https://github.com/alios/shims/tree/fix-reactive-banana).

Here's a preview (full source in [Examples/Counter](./Examples/Counter.hs):

```Haskell
-- A thing with 2 buttons and a labe: + and -, and a label showing the current
-- value. WidgetConstructor t is essentially MomentIO (t, VirtualElement),
-- where VirtualElement is something which can be rendered to the DOM.
--
-- We use the function
--
--     knot :: (r -> WidgetConstructor (t, r)) -> WidgetConstructor t
--
-- The parameter of type r, here a 3-tuple containing a reactive-banana
-- Behavior and two Events, must only be used lazily.
counter :: Int -> WidgetConstructor (Event Int)
counter initial = knot $ \(~(currentValueBehavior, incrEvent, decrEvent)) -> do

    -- Every change to the counter value: either an increment or a
    -- decrement.
    let changes :: Event (Int -> Int)
        changes = unionWith const (const ((+) 1) <$> incrEvent)
                                  (const (flip (-) 1) <$> decrEvent)

    let currentValueEvent :: Event Int
        currentValueEvent = flip ($) <$> currentValueBehavior <@> changes

    let countSequence :: Sequence Int
        countSequence = initial |> currentValueEvent

    -- A label to show the count.
    let display :: WidgetConstructor ()
        display = label (T.pack . show <$> countSequence)

    -- Two buttons: one showing plus and one showing minus.
    -- withEvent extract their click events; the parameter const determines
    -- how we combine that event with the value contained in button:
    --   button :: Sequence Text -> WidgetConstructor ()
    -- so in this case
    --   const :: Event () -> () -> Event ()
    -- i.e. we throw away the useless () that comes from button.
    let bplus :: WidgetConstructor (Event ())
        bplus = withEvent Click const (button (always (T.pack "+")))
    let bminus :: WidgetConstructor (Event ())
        bminus = withEvent Click const (button (always (T.pack "-")))

    -- Roll up the three WidgetConstructors into one WidgetConstructor, via the
    -- applicative variant. We choose a div with no attributes, properties nor
    -- style. The abstraction is fairly shallow when it comes to these things.
    let props = always mempty
    let attrs = always mempty
    let style = always mempty
    let combined :: WidgetConstructor (Event (), Event ())
        combined = runApplicativeWidget (T.pack "div") props attrs style
                   $ (,) <$> applicativeWidget bplus
                         <*  applicativeWidget display
                         <*> applicativeWidget bminus

    -- combined is the constructor we wish to give back, but with a different
    -- value inside (an Event Int rather than (Event (), Event ())). We also
    -- need to come up with those recursive parameters currentValueBehavior,
    -- incrEvent, and decrEvent. To that end, we use
    --
    --   withValue :: WidgetConstructor t
    --             -> (t -> MomentIO r)
    --             -> WidgetConstructor r
    --
    -- which is kind of like a monadic bind but not quite, since the second
    -- parameter does not give a Widget, just an r. Here's how it's used:
    withValue combined $ \(incrEvent, decrEvent) -> do

        -- behavior :: Sequence t -> (Behavior t, Event t).
        (currentValueBehavior, valueChangeEvent) <- behavior countSequence

        pure (valueChangeEvent, (currentValueBehavior, incrEvent, decrEvent))
```

## Informal overview

There are four principal concepts in here.

### VirtualElement

A description of a DOM element, which can be rendered 0 or more times. Its
properties (things like `value` on a text input), attributes, style, and
children are given by `Sequence`s, and are therefore time-varying
(from [reactive-sequence](https://github.com/avieth/reactive-sequence), just
an `Event` with an initial value).

### Widget and WidgetConstructor

```Haskell
newtype Widget t = Widget (t, VirtualElement)
type WidgetConstructor t = MomentIO (Widget t)
```

A functor carrying a `VirtualElement`, and an effectful variant, which is
very useful because we often need `MomentIO` to set up `Event`s and
`Behavior`s. Holding some other value along with a `VirtualElement` allows
us to route data up through composite `Widget`s, as we do in the counter
example above.

### Knot

A *knot* is a pattern for defining `WidgetConstructor`s in which the output of
the constructor is also its input. The definition is very small, and it's
intended to make `RecursiveDo` unnecessary for users of the library.

```Haskell
knot k = mdo
    w <- k r
    let ((t, r), velem) = runWidget w
    pure $ Widget (t, velem)
```

### Flow

Consider those software installation wizards that come up in Windows and OS X,
in which the user interface proceeds in various stages, each advanced by the
press of a 'Next' button. This exhibits the *flow* pattern: a sequence of
`Widget`s, each of which determines when the next one should be shown.

More formally, the datatype `Flow` is defined, and is a `Category`, `Arrow`,
and `ArrowChoice`. To place a `Widget` into a `Flow`, an `Event` to trigger
the next `Flow` must be given:

```Haskell
-- The parameter o allows the Widget's value to be observed outside of the
-- Flow itself.
widgetFlow :: (s -> MomentIO (Widget o, Event t)) -> Flow o s t
```

However, since we have `arr :: (s -> t) -> Flow o s t`, not every `Flow`
determines a `Widget`! So instead of searching for a function like

```Haskell
runFlow :: Flow o s t -> WidgetConstructor (Sequence o)
```

we give instead a more restrictive version

```Haskell
runFlow :: Flow o s Void -> WidgetConstructor (Sequence o)
```

This works because it's just not possible to create a `Flow o s Void` without
giving a `WidgetConstructor`! We call a `Flow o s Void` a *complete flow*.
