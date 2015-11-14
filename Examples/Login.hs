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

data Login m = Login {
      loginVirtualElement :: VirtualElement m
    , loginSubmitEvent :: Event (Maybe JSString, Maybe JSString)
    }

-- | Make a Login component by providing sequences of usernames and passwords.
login
    :: Applicative m
    => Sequence (Maybe JSString) -- ^ Usernames
    -> Sequence (Maybe JSString) -- ^ Passwords
    -> MomentIO (Login m)
login usernameSequence passwordSequence = mdo

    usernameInput <- element (pure "input")
                             (always (pure (M.fromList [("placeholder", "Username")])))
                             (always (pure inputStyle))
                             (always (pure []))

    passwordInput <- element (pure "input")
                             (always (pure (M.fromList [ ("placeholder", "Password")
                                                       , ("type", "password")
                                                       ]
                                           )
                                     )
                             )
                             (always (pure inputStyle))
                             (always (pure []))

    button <- element (pure "input")
                      (always (pure (M.fromList [ ("type", "submit")
                                                , ("value", "☺")
                                                ]
                                    )
                              )
                      )
                      ((pure buttonStyle) |> (fmap pure buttonStyleOnHover))
                      (always (pure []))

    container <- vertically (always (pure [pure usernameInput, pure passwordInput, pure button]))

    form <- element (pure "form")
                    (always (pure M.empty))
                    (always (pure formStyle))
                    (always (pure [pure (node container)]))

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

    buttonMouseenter <- virtualEvent button Element.mouseEnter (\_ _ -> return ())
    buttonMouseleave <- virtualEvent button Element.mouseLeave (\_ _ -> return ())
    let buttonStyleOnHover = unionWith const
                                       ((const buttonHoverStyle) <$> buttonMouseenter)
                                       ((const buttonStyle) <$> buttonMouseleave)

    return $ Login form (usernameAndPasswordBehavior <@ submit)

  where

    inputStyle = M.fromList [
          ("background-color", "rgba(255,255,255,0.4)")
        , ("border", "none")
        , ("margin", "0px 4px 4px 4px")
        , ("text-align", "center")
        ]

    buttonStyle = M.fromList [
          ("background-color", "rgba(0,0,0,0.25)")
        , ("border", "none")
        , ("color", "rgba(255,255,255,0.8)")
        , ("margin", "0px 4px 4px 4px")
        ]

    buttonHoverStyle = M.fromList [
          ("background-color", "rgba(0,0,0,0.25)")
        , ("border", "none")
        , ("color", "rgba(255,255,255,0.8)")
        , ("margin", "0px 4px 4px 4px")
        , ("box-shadow", "0px 0px 4px rgba(255,255,255,0.4) inset")
        ]

    formStyle = M.fromList [
          ("background-color", "rgba(0,0,0,0.2)")
        , ("border-radius", "4px")
        , ("padding", "4px 0px 0px 0px")
        ]

main = runWebGUI $ \webView -> do

    Just document <- webViewGetDomDocument webView
    Just body <- getBody document

    -- Set the body to occupy the entire viewport.
    addStyle (toElement body)
             (M.fromList [ ("position", "absolute")
                         , ("width", "100%")
                         , ("height", "100%")
                         , ("margin", "0")
                         ]
             )

    let networkDescription :: MomentIO ()
        networkDescription = do
            login_ <- login (always Nothing) (always Nothing)
            centredLoginElement <- centred (always (pure (pure (node (loginVirtualElement login_)))))
            background <- element (pure "div")
                                  (always (pure M.empty))
                                  (always (pure (M.fromList [
                                                      ("background-image", "url(\"background.jpeg\")")
                                                    , ("background-position", "center")
                                                    , ("background-size", "cover")
                                                    , ("position", "absolute")
                                                    , ("left", "0px")
                                                    , ("top", "0px")
                                                    , ("width", "100%")
                                                    , ("height", "100%")
                                                    , ("filter", "blur(8px) sepia(60%)")
                                                    ]
                                                )
                                          )
                                  )
                                  (always (pure []))
            ui <- element (pure "div")
                          (always (pure M.empty))
                          (always (pure M.empty))
                          (always (pure [pure (node background), pure (node centredLoginElement)]))

            render document body ui
            reactimate (print <$> loginSubmitEvent login_)

    network <- compile networkDescription
    actuate network

    return ()
