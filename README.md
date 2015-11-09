# reactive-dom

Definitions in this project help the programmer to wire up a reactive DOM user
interface via reactive-banana and ghcjs-dom.

This is experimental software, but it's also working software. Here's a preview
of how it works (full example in [Counter.hs](Examples/Counter.hs)):

```Haskell
-- | Create an element which responds to the given sequence of Ints by
--   displaying the latest one. This component also has internal increment and
--   decrement buttons.
counter :: Sequence Int -> MomentIO (VirtualElement)
counter stateSequence = mdo

    -- The function @element@ is used to create @VirtualElement@s: things which
    -- can be instantiated to DOM elements, and from which we can derive events.

    -- A span to display the current Int.
    display <- element "span"
                       (always M.empty) -- No attributes, ever
                       (always M.empty) -- No style, ever.
                       ([Right initialDisplayText] |> nextDisplayText)

    -- A button to increment.
    incrButton <- element "input"
                          (always $ M.fromList [("type", "button"), ("value", "+")])
                          (always M.empty)
                          (always [])

    -- A button to decrement.
    decrButton <- element "input"
                          (always $ M.fromList [("type", "button"), ("value", "-")])
                          (always M.empty)
                          (always [])

    -- A container for the three elements.
    container <- element "div"
                         (always M.empty)
                         (always M.empty)
                         (always [node decrButton, node display, node incrButton])

    -- Grab some reactive-banana events from the *virtual* elements. The last
    -- parameter is of type
    --   Element -> MouseEvent -> IO t
    -- whith @t@ as in the resulting @Event t@. Here we just choose
    -- @t ~ MouseEvent@ but we could have chosen @()@ since we don't need the
    -- event data.
    incrs <- virtualEvent incrButton Element.click (\_ -> return)
    decrs <- virtualEvent decrButton Element.click (\_ -> return)

    let initialDisplayText = toJSString (show (sequenceFirst stateSequence))

    -- Changes induced by button presses.
    let buttonChanges :: Event (Int -> Int)
        buttonChanges = unionWith const
                                  (fmap (const ((+) 1)) incrs)
                                  (fmap (const (\x -> x - 1)) decrs)

    -- Changes induced by button presses or by the input sequence.
    let changes :: Event (Int -> Int)
        changes = unionWith const
                            (fmap const (sequenceRest stateSequence))
                            buttonChanges

    -- This Behavior is the current value of the counter. It is recursively
    -- defined alongside @computedState@ below.
    stateBehavior :: Behavior Int <- stepper (sequenceFirst stateSequence)
                                             computedState

    -- Apply the changes to the current state to obtain the next state.
    let computedState :: Event Int
        computedState = (flip ($)) <$> stateBehavior <@> changes

    let nextDisplayValue :: Event Int
        nextDisplayValue = unionWith const (sequenceRest stateSequence) computedState

    let nextDisplayText :: Event [VirtualNode]
        nextDisplayText = (pure . Right . toJSString . show) <$> nextDisplayValue

    return container
```
