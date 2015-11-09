{-|
Module      : Examples.Counter
Description : An example in which a reactive counter is defined.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import qualified Data.Map as M
import GHCJS.Types
import GHCJS.DOM.Types (toJSString, MouseEvent)
import GHCJS.DOM
import GHCJS.DOM.Element as Element
import GHCJS.DOM.Node as Node
import GHCJS.DOM.Document as Document
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Debug.Trace

-- | Create an element which responds to the given sequence of Ints by
--   displaying the latest one. This component also has internal increment and
--   decrement buttons.
counter :: Sequence Int -> MomentIO (VirtualElement)
counter stateSequence = mdo

    -- A span to display the current Int.
    display <- element "span"
                       (always M.empty) -- No attributes, ever
                       (always M.empty) -- No style, ever.
                       ([pure (text initialDisplayText)] |> nextDisplayText)

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
                         (always [ pure (node decrButton)
                                 , pure (node display)
                                 , pure (node incrButton)
                                 ]
                         )

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

    let nextDisplayText :: Event [MomentIO VirtualNode]
        nextDisplayText = (pure . pure . text . toJSString . show) <$> nextDisplayValue

    return container

main = runWebGUI $ \webView -> do
    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = mdo

            -- Make the counter VirtualElement. We choose 10 as the initial
            -- count, and throw in 42 whenever the mouse leaves the counter.
            vcounter <- counter (10 |> (const 42 <$> mouseleaves))

            mouseleaves <- virtualEvent vcounter Element.mouseLeave (\_ -> return)

            -- Put the counter into a centred container.
            vcentredCounter <- centred (always (pure (node vcounter)))

            -- Render the centred counter.
            render document body (node vcentredCounter)

            return ()

    network <- compile networkDescription
    actuate network

    return ()
