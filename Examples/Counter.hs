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

counter
    :: ( )
    => Sequence Int
    -> MomentIO (VirtualElement)
counter stateSequence = mdo

    display <- element "span"
                       (M.empty |> never)
                       (M.empty |> never)
                       ([Right initialDisplayText] |> nextDisplayText)

    incrButton <- element "input"
                          (M.fromList [("type", "button"), ("value", "+")] |> never)
                          (M.empty |> never)
                          ([] |> never)

    decrButton <- element "input"
                          (M.fromList [("type", "button"), ("value", "-")] |> never)
                          (M.empty |> never)
                          ([] |> never)

    container <- element "div"
                         (M.empty |> never)
                         (M.empty |> never)
                         ([Left decrButton, Left display, Left incrButton] |> never)

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

main = runWebGUI $ \webView -> do
    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = mdo

            button <- element "input"
                              (M.fromList [("type", "button")] |> never)
                              (M.empty |> never)
                              ([] |> never)

            textInput <- element "input"
                                 (M.empty |> never)
                                 (M.empty |> never)
                                 ([] |> never)

            el <- element "div"
                          (M.empty |> nextAttributes)
                          (M.empty |> nextStyle)
                          ([Left button, Left textInput] |> nextChildren)

            mouseleaves <- virtualEvent el Element.mouseLeave (\_ -> return)
            clicks <- virtualEvent el Element.click (\_ -> return)

            let makeNextStyle :: MouseEvent -> M.Map JSString JSString
                makeNextStyle _ = M.fromList [
                                        ("background-color", "rgba(232,54,98,0.3)")
                                      , ("padding", "2px 2px 2px 2px")
                                      ]

            let makeNextAttributes :: MouseEvent -> M.Map JSString JSString
                makeNextAttributes _ = M.fromList [("foo", "bar")]


            let makeNextChildren :: MouseEvent -> [VirtualNode]
                makeNextChildren _ = [Right "Hello world", Left button, Left button, Left button, Left textInput]

            let nextStyle = makeNextStyle <$> mouseleaves
            let nextAttributes = makeNextAttributes <$> mouseleaves
            let nextChildren = makeNextChildren <$> clicks

            reactimate ((\_ -> trace "Mouseleave" (return ())) <$> mouseleaves)
            reactimate ((\_ -> trace "Click" (return ())) <$> clicks)

            -- It's important to render only after all events are got.
            el' <- centred (always (Left el))
            --rendered <- renderVirtualElement document el'
            --body `appendChild` (Just (renderedVirtualElement rendered))

            counterVirtualElement <- (centred . always . Left) =<< (counter (10 |> ((const 42) <$> clicks)))
            --counterRendered <- renderVirtualElement document counterVirtualElement
            --body `appendChild` (Just (renderedVirtualElement counterRendered))

            top <- horizontally (always [el', counterVirtualElement])
            render document body (Left top)

            --renderedTop <- renderVirtualElement document top
            --body `appendChild` (Just (renderedVirtualElement renderedTop))

            return ()

    network <- compile networkDescription
    actuate network

    return ()
