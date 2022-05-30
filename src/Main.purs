module Main where

import Prelude
import Effect (Effect)
import Web.HTML (window)
import Effect.Console (log)
import React.Basic.DOM as D
import Data.Tuple (Tuple(..))
import React.Basic.Hooks as R
import Data.Either (Either(..))
import Effect.Exception (throw)
import Web.HTML.Window (document)
import Data.String.Read (readDefault)
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic.DOM.Events (targetValue)
import React.Basic (element, ReactComponent)
import React.Basic.Events (EventHandler, handler)
import Data.Array (filter, mapWithIndex, updateAt)
import React.Basic.Hooks (useState, reactComponent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Data.Sequences (exampleSequencePair, SequencePair, SequenceType(..), allSequenceTypes)
import Data.Sequences.Validation (ValidationError(..), Errors, validateFirstSequence', validateSecondSequence')
import Data.Array (length)
import Data.String.CodeUnits (toCharArray)
import Data.Array (fromFoldable)

renderOption :: forall a. Show a => a -> R.JSX
renderOption optValue =
  let
    str = show optValue
  in
    D.option
      { value: str
      , children: [ D.text str ]
      }

renderOptions :: Array R.JSX
renderOptions = map renderOption allSequenceTypes

selectField :: String -> (String -> Effect Unit) -> R.JSX
selectField name setValue =
  D.div
    { className: "form-row m-4"
    , children:
        [ D.label
            { className: "col-md-5"
            , children:
                [ D.text name ]
            }
        , D.select
            { className: "col-md-1"
            , children: renderOptions
            , onChange: onChangeFn setValue
            }
        ]
    }

onChangeFn :: (String -> Effect Unit) -> EventHandler
onChangeFn setValue =
  let
    handleValue :: Maybe String -> Effect Unit
    handleValue (Just v) = setValue v

    handleValue Nothing = pure unit
  in
    handler targetValue handleValue

formField :: String -> String -> String -> (String -> Effect Unit) -> Errors -> R.JSX
formField name placeholder value setValue errors =
  let
    validationClass =
      if (length errors) > 0 then
        "is-invalid"
      else
        "is-valid"
  in
    D.div
      { className: "form-row m-4"
      , children:
          [ D.label
              { className: "col-md-5"
              , children: [ D.text name ]
              }
          , D.input
              { className: "col-md-7 form-control " <> validationClass
              , required: true
              , placeholder
              , value
              , onChange: onChangeFn setValue
              }
          ]
            <> renderValidationErrors errors
      }

renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []

renderValidationErrors xs =
  let
    renderError :: ValidationError -> R.JSX
    renderError (ValidationError err) = D.li_ [ D.text err ]

    filterSpecificField :: Errors -> Errors
    filterSpecificField errs =
      filter
        ( \e -> case e of
            ValidationError _ -> true
            _ -> false
        )
        errs

    errors = filterSpecificField xs
  in
    case errors of
      [] -> []
      _ ->
        [ D.div
            { className: "invalid-feedback"
            , children: [ D.ul_ (map renderError errors) ]
            }
        ]

renderState :: SequencePair -> Array R.JSX
renderState sp =
  [ D.div
      { children:
          [ D.text $ show sp ]
      }
  ]

renderHeaders :: String -> Array R.JSX
renderHeaders seq =
  map (\c -> D.th
                { scope: "col"
                , children:
                    [ D.text $ show c ]
                }
      ) $ fromFoldable $ toCharArray seq

renderMatrix :: SequencePair -> Array R.JSX
renderMatrix sp =
  [ D.div
      { children:
          [ D.table
               { className: "table"
               , children:
                   [ D.thead_ [ D.tr_ $ renderHeaders sp.firstSequence.sequence ] ]
               }
          ]
      }
  ]

mkSequenceApp :: Effect (ReactComponent {})
mkSequenceApp =
  reactComponent "SequenceApp" \_ -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple sequences setSequences <- useState exampleSequencePair
    let
      firstSequenceErrors = case validateFirstSequence' sequences of
        Left e -> e
        Right _ -> []

      secondSequenceErrors = case validateSecondSequence' sequences of
        Left e -> e
        Right _ -> []
    pure
      $ D.div
          { className: "container"
          , children:
              [ D.form
                  { children:
                      [ D.h3
                          { className: "m-4"
                          , children:
                              [ D.text "Sequences" ]
                          }
                      , selectField "Sequences Type"
                          ( \t ->
                              let
                                st = (readDefault t :: SequenceType)
                              in
                                setSequences _ { firstSequence { "type" = st }, secondSequence { "type" = st } }
                          )
                      , formField "First Sequence" "First Sequence" sequences.firstSequence.sequence
                          ( \fs ->
                              setSequences _ { firstSequence { sequence = fs } }
                          )
                          firstSequenceErrors
                      , formField "Second Sequence" "Second Sequence" sequences.secondSequence.sequence
                          ( \ss ->
                              setSequences _ { secondSequence { sequence = ss } }
                          )
                          secondSequenceErrors
                      ]
                        <> renderState sequences
                        <> renderMatrix sequences
                  }
              ]
          }

main :: Effect Unit
main = do
  log "Rendering sequence insertion component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create Sequences react component
      sequenceApp <- mkSequenceApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element sequenceApp {}
      -- Render Sequences JSX node in DOM "container" element
      D.render app c
