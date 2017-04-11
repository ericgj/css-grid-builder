module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Grid exposing (Model, Section, AbsoluteUnit(..), GridUnit(..))


main : Program Never Model Msg
main = 
  beginnerProgram 
    { view = view
    , update = update
    , model = init
    }

init : Model
init =
  Grid.empty 
    |> Grid.mapGrid (Grid.setGap (Px 10))

defaultRowUnit = Abs (Px 100)
defaultColUnit = Fr 1

type Msg 
  = AddRow GridUnit
  | AddCol GridUnit

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddRow unit ->
      Grid.addRow unit defaultColUnit model

    AddCol unit ->
      Grid.addCol defaultRowUnit unit model


view : Model -> Html Msg
view model =
  div []
   [ div [ class "grid-controls" ]
     [ viewAddRow 
     , viewAddCol
     ]
   , div [ class "grid-container" ]
     [ viewGrid model
     ]
   ]

viewAddRow : Html Msg
viewAddRow =
  div []
    [ label [] [text "Row"]
    , button [onClick (AddRow <| defaultRowUnit)] [text "+"]
    ]

viewAddCol : Html Msg
viewAddCol =
  div []
    [ label [] [text "Column"]
    , button [onClick (AddCol <| defaultColUnit)] [text "+"]
    ]

viewGrid : Model -> Html Msg
viewGrid model =
  div [ style <| Grid.gridStyles model ]
    <| List.map viewSection (Grid.sections model) 

viewSection : Section -> Html Msg
viewSection section =
  div 
    [ class "grid-section", style <| Grid.sectionToStyles section 
    ] [ ]

