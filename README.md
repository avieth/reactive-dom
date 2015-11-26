# reactive-dom

Definitions in this project help the programmer to wire up a reactive DOM user
interface via reactive-banana and ghcjs-dom.

This is *experimental* software, but it's also *working* software, subject
to one restriction: there is an
[issue](https://github.com/ghcjs/shims/pull/25#issuecomment-154876738)
with weak references in GHCJS, but it is resolved in
[this branch](https://github.com/alios/shims/tree/fix-reactive-banana).
Here's a preview of how to use it:

```Haskell
import GHCJS.DOM
import GHCJS.DOM.Document
import Reactive.Banana.Frameworks
import Reactive.DOM.Node
import Reactive.DOM.Component
import Examples.Counter

main :: IO ()
main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = mdo

            -- This is the only interesting part of this snippet: run the
            -- counter component, giving () as input, and then throw its
            -- element into the DOM under the body. V stands for virtual, as
            -- it's not actually a DOM element, and can be instantiated more
            -- than once, as demonstrated. So two counters will appear, and
            -- they will always show the same number, and why wouldn't they?
            -- They are the same counter!
            (_, counterVelem) <- runComponent counter ()
            _ <- render document body counterVelem
            _ <- render document body counterVelem

            return ()

    network <- compile networkDescription
    actuate network

    return ()
```

That's pretty dull. All of the cool Haskell magic is hidden away and all you
see here is reactive-banana and ghcjs-dom boilerplate. If you're curious, then
dive in using the following roadmap.

## Reactive.DOM.Node

In this module we define the `VirtualElement`. It takes `SBehavior`s (see
[reactive-sequence](https://www.github.com/avieth/reactive-sequence)) of
properties, attributes, style, and children, and when its rendered to the
DOM, reacts to changes in these. `SEvent`s can also be pulled from these
`VirtualElement`s, such that actual DOM events from every rendering of the
same `VirtualElement` will all be routed to this one `SEvent`. These are
the elements from which `Component`s are built.

## Reactive.DOM.Component

Working directly with `VirtualElement`s is not fun. It's error-prone, and
quickly devolves into a mess of events and behaviors and recursive-do trip-ups.
The notion of a `Component` is intended to encapsulate this complexity in a few
versatile forms, guiding the programmer's definition of new `Component`s from
old. Let's take a few examples.

### `Reactive.DOM.Component.Label`

This is a very simple `Component`: give any sequence of strings and you can
get a label which always shows the current one. Not surprisingly, it's
described by the `Simple` datatype. Any `Simple` is a `Component`, but it's
the responsibility of the author of the simple component to show how to
go from its input, to its output and a `VirtualElement`. Since these components
are supposed to be *simple*, this shouldn't be a problem, and indeed it isn't
a problem for the label:

```Haskell
-- Give JSStrings, get JSStrings. This means you can control the text by
-- choosing an appropriate sequence, and respond to the text by taking the
-- output.
type Label = Simple (SBehavior JSString) (SBehavior JSString)

-- The implementation of the Label component follows.
label :: Label
label = Simple makeLabel
  where
    makeLabel :: SBehavior JSString -> MomentIO (SBehavior JSString, VirtualElement Identity)
    makeLabel textSequence = do
        -- This is easy enough: make a span which shows the text sequence, but
        -- has no properties, attributes, nor style.
        velem <- virtualElement (pure "span")
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (always M.empty))
                                (pure (pure . pure . text . pure <$> textSequence))
        return (textSequence, velem)
```

### `Examples.Counter`

The counter example, mentioned in the introduction, is our first example of
a complex `Component`. It's composed of three subcomponents, and some routing
logic which describes how those components interact with one-another.


```Haskell
-- What we have in mind is a component with two buttons--increment and
-- decrement--and a label showing the current value. This falls into the
-- Simultaneous type. What we have here makes the following statements:
--
--   - No input is required (just give ()).
--   - The counter is composed of three subcomponents: a Label and two Buttons.
--   - In order to make a counter, you have to describe how to compute inputs
--     for the subcomponents from outputs for those subcomponents. This
--     recursive treatment is essential: the input for the label depends upon
--     the outputs of the buttons, namely their click events.
--
type Counter = Simultaneous () (Label :*: Button :*: Button)

-- With the type given, we have bounds for our implementation.
-- The second parameter of Simultaneous is just the implementations of the
-- subcomponents, in the appropriate order. No problem.
-- The first parameter, however, contains the essence of the counter. It
-- routes outputs of subcomponents to inputs of subcomponents, giving rise
-- to the desired reactive behaviour.
counter :: Counter
counter = Simultaneous (const wireItUp) (label .*. button .*. button)
  where
    -- Note the lazy pattern! That's VERY important. Without it, we'll diverge.
    wireItUp :: (SBehavior JSString :*: SEvent () :*: SEvent ())
             -> (SBehavior JSString :*: SBehavior JSString :*: SBehavior JSString)
    wireItUp ~(Product (_, Product (incr, decr))) = labelBehavior .*. incrLabel .*. decrLabel
      where
        -- The label for the increment button is always "+".
        incrLabel = always "+"
        -- Similarly, the label for the decrement button never changes from "-".
        decrLabel = always "-"
        -- From the increment event (it's the output of the first button in
        -- the product) we can produce an event which increments something by
        -- one.
        incrEvent :: SEvent (Int -> Int)
        incrEvent = (const ((+) 1)) <$> incr
        -- Similarly for the decrement event.
        decrEvent :: SEvent (Int -> Int)
        decrEvent = (const (\x -> x - 1)) <$> decr
        -- Here we merge the increment and decrement events. We appeal to the
        -- semigroup First to disambiguate simultaneous evnts (which probably
        -- won't ever happen, but it's good to be safe). The way we've
        -- written it here, the increment wins over the decrement in case they
        -- happen at the same time.
        changes :: SEvent (Int -> Int)
        changes = getFirst <$> ((First <$> incrEvent) <||> (First <$> decrEvent))
        -- From changes, we can describe the sequence of counter values. We
        -- do so recursively: assume we have the Behavior for this thing,
        -- and use it to define the SBehavior (see reactive-sequence to learn
        -- about the difference). All we say here is that it starts at 0,
        -- and whenever changes gives us an event with (Int -> Int), apply
        -- it to the current value.
        countBehavior :: SBehavior Int
        countBehavior = fixSBehavior' $ \behavior ->
            sEventToSBehavior 0 (($) <$> changes %> behavior)
        -- Finally, the label text is had by dumping the current value to
        -- a JSString.
        labelBehavior :: SBehavior JSString
        labelBehavior = fromString . show <$> countBehavior
```
