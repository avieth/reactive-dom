{-|
Module      : Examples.Login
Description : Example of a login component.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import GHCJS.Types
import GHCJS.DOM.Types (toJSString, MouseEvent)
import GHCJS.DOM
import GHCJS.DOM.Event hiding (Event)
import GHCJS.DOM.Element as Element
import GHCJS.DOM.Node as Node
import GHCJS.DOM.Document as Document
import GHCJS.DOM.HTMLInputElement (castToHTMLInputElement, getValue, setValue)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Reactive.DOM.Node
import Debug.Trace

data Login = Login {
      loginVirtualElement :: VirtualElement
    , loginSubmitEvent :: Event (Maybe JSString, Maybe JSString)
    }

login
    :: Sequence (Maybe JSString)
    -> Sequence (Maybe JSString)
    -> MomentIO Login
login usernameSequence passwordSequence = mdo

    usernameInput <- element "input"
                             (always (M.fromList [("placeholder", "Username")]))
                             (always M.empty)
                             (always [])

    passwordInput <- element "input"
                             (always (M.fromList [ ("placeholder", "Password")
                                                 , ("type", "password")
                                                 ]
                                     )
                             )
                             (always M.empty)
                             (always [])

    button <- element "input"
                      (always (M.fromList [ ("type", "submit")
                                          , ("value", "Log in")
                                          ]
                              )
                      )
                      (always M.empty)
                      (always [])

    container <- vertically (always [usernameInput, passwordInput, button])

    form <- element "form"
                    (always M.empty)
                    (always M.empty)
                    (always [node container])

    -- Must preventDefault else the form submit causes the page to reload.
    submit <- virtualEvent form Element.submit (\_ -> preventDefault)

    let getInputValue :: IsEvent event => Element -> event -> IO (Maybe JSString)
        getInputValue el _ = getValue (castToHTMLInputElement el)

    let setInputValue :: Element -> Maybe JSString -> IO ()
        setInputValue el maybeStr = setValue (castToHTMLInputElement el) maybeStr

    usernameInputEvent <- virtualEvent usernameInput Element.input getInputValue
    passwordInputEvent <- virtualEvent passwordInput Element.input getInputValue

    usernameBehavior <- stepper (sequenceFirst usernameSequence) usernameChanges
    passwordBehavior <- stepper (sequenceFirst passwordSequence) passwordChanges

    let usernameChanges = unionWith const (sequenceRest usernameSequence) (usernameInputEvent)
    let passwordChanges = unionWith const (sequenceRest passwordSequence) (passwordInputEvent)

    let usernameAndPasswordBehavior = (,) <$> usernameBehavior <*> passwordBehavior

    virtualReactimate usernameInput usernameChanges setInputValue
    virtualReactimate passwordInput passwordChanges setInputValue

    return $ Login form (usernameAndPasswordBehavior <@ submit)

main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    let networkDescription :: MomentIO ()
        networkDescription = do
            login_ <- login (always Nothing) (always Nothing)
            -- Here we render the same virtual element twice. Observe that
            -- the two will remain tightly in-sync.
            render document body . node . loginVirtualElement $ login_
            render document body . node . loginVirtualElement $ login_
            reactimate (print <$> loginSubmitEvent login_)

    network <- compile networkDescription
    actuate network

    return ()
