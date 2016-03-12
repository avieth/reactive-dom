# reactive-dom

Definitions in this project help the programmer to wire up a reactive DOM user
interface, expressed in Haskell via reactive-banana and ghcjs-dom, and
compiled to JavaScript using ghcjs.

This is **experimental** software, but it's also **working** software!
That's subject to one restriction: there is an
[issue](https://github.com/ghcjs/shims/pull/25#issuecomment-154876738)
with weak references in ghcjs, but it is resolved in
[this branch](https://github.com/alios/shims/tree/fix-reactive-banana).

# Overview

The goal is to come up with a user interface. These
things can be rendered to the DOM when given a document and parent object

```Haskell
type UI t

-- render is in MomentIO because rendering a UI will mutate a
-- reactive-banana event network.
render
    :: (IsDocument document, IsNode parent)
    -> document
    -> parent
    -> UI t
    -> MomentIO (RenderedNode, t)

unrender :: RenderedNode -> MomentIO ()
```

The fact that a `UI` carries a parameter (it is a functor too) means that we
can, among other things, obtain `Event`s from them once they're rendered. We'll
see that this proves useful when defining composite `UI`s.

A `UI` is a specialization of something called a `Widget`.

```Haskell
-- A Widget is a Profunctor, and carries a Symbol. Constraints on that Symbol
-- are used to indicate which W3C tag will be used when it's rendered.
type Widget (tag :: Symbol) s t

-- If there are no constraints on the Widget's tag, we have what's known as
-- an OpenWidget.
type OpenWidget s t = forall tag . Widget tag s t

-- On the other hand, if we know the tag then we have a ClosedWidget
-- (W3CTag tag implies KnownSymbol tag).
data ClosedWidget s t where
    ClosedWidget :: W3CTag tag => Widget tag s t -> ClosedWidget s t

-- A ClosedWidget with trivial input is ready to be rendered.
data UI t = ClosedWidget () t
```

So what's in a `Widget tag s t`? It includes a derivation of a `t` from an
`s` inside a special monad called `ElementBuilder`. But more importantly, it
also carries a definition of the children of some DOM element in terms of the
data carried by those children (with `s` in scope as well, of course). The
latter part, definiting children in terms of children, is the kernel and
atomic unit of a `UI`. Formally, it looks like this:

```Haskell
type Widget tag s t = (s, ViewChildren f) -> ElementBuilder tag (t, Children f)
```

Notice the `f` parameter. This brings us to an important piece of
infrastructure. When working with plain JavaScript, any DOM element has a list
of children. Even if we know the structure of our element, we always have to
work with a list of children. In this library, the children of a `Widget` are
structured by an arbitrary `f :: (* -> *) -> *`. Actaully, it's a bit more
complicated: the container type depends also on the input and output types
`s`, `t`. Some example choices:

```Haskell
-- A single child carrying a value of type t.
type Single t f = f t

-- Precisely two children carrying values of type s, t respectively.
type Double s t f = (f s, f t)

-- An arbitrary list of children, carrying values of homogeneous type.
type NodeList t f = [f t]

-- One or zero children, carrying a value of type t.
type Optional t f = Maybe (f t)
```

To understand why this form is chosen, it may help to look at these types:

```Haskell
-- A rendered DOM element and some data.
type Child t = (t, SomeNode)

-- An unrendered DOM element, which will give data of type t once it is
-- rendered (it's behind an IO-like monad).
type SetChild t
newChild :: UI t -> SetChild t
existingChild :: Child t -> SetChild t
textChild :: T.Text -> SetChild ()

-- Description of what the children should be: the initial SetChild values
-- structured by some f, and the changes to it.
type Children f = (f SetChild, Event [Change f SetChild])

-- Description of what the children actually are, determined by a
-- Children f: the initial Child values, and the changes to it.
type ViewChildren f = (f Child, Event [Change f Child])
```

So if we give, for instance, a `Children (Single (Event ()))`--that's to say,
we give a time-varying `Single (Event ()) SetChild` value--then we obtain
access to a `ViewChildren (Single (Event ())) Child`, and therefore a
time-varying `Event ()`! Of course, we must be careful to use that `Event ()`
lazily, or else the program will diverge. Maybe an example is in order:

```Haskell
-- TODO an example that's short and simple, but not too simple.
```

It may seem strange that we use a notion of changes, rather than just a sequence
of `f SetChild` values, to describe the children (it seems strange to me!). This
choice was made because DOM mutation can be incredibly slow, and we must aim to
do as little of it as possible. Having a `Change f` type allows certain
containers to implement DOM mutations more efficiently.

Once this part of the `Widget` is described, it can't be changed; the children
container `f` is locked away inside the `Widget` constructor. But we can still
augment a `Widget` with style, attributes, properties, take DOM events, and
alter its input/output purely using the `Profunctor` interface. What's more, if
it's an `OpenWidget` (its tag is unconstrained), then we can "horizontally
compose" it:

```Haskell
-- Concatenate the children of the two Widgets.
widgetProduct
    :: OpenWidget s1 t1
    -> OpenWidget s2 t2
    -> OpenWidget (s1, s2) (t1, t2)
```

By demanding `OpenWidget`s, we ensure that neither term has any style,
attributes, properties, or events, and that's very important because the
`widgetProduct` is flat: if one of them had style directives, they would
apply to the whole product, probably leading to a clash.

Any `OpenWidget` may be given a tag, and therefore closed. Any `ClosedWidget`
may be opened by deriving a new `Widget` which contains it as its sole child.

```Haskell
closeWidget :: Tag tag -> OpenWidget s t -> Widget s t
openWidget :: ClosedWidget s t -> OpenWidget s t
```

And so a pattern for UI composition arises: open the `Widget`s, horizontally
compose them, interconnect their inputs and outputs using the `Profunctor`
interface or `Modifier` if a `MonadMoment` is required, possibly tying a
recursive knot (specifying input in terms of output), and then close the
composite by giving it a tag.
