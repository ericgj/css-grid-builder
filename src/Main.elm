module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Grid exposing 
  (Model, GridUnit(..), AbsoluteUnit(..), PositionedSection(..), GridCoord)


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
  | RemoveRow
  | RemoveCol
  | PlaceNewSection GridCoord
  | RemoveSection GridCoord

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddRow unit ->
      Grid.addRow unit model

    AddCol unit ->
      Grid.addCol unit model

    RemoveRow ->
      Grid.removeRow model

    RemoveCol ->
      Grid.removeCol model
 
    PlaceNewSection coord ->
      Grid.addSectionAndPlaceAt section coord model   -- doesn't work, need to initialize section

    RemoveSection coord ->
      Grid.removeSectionFrom coord model

view : Model -> Html Msg
view model =
  div []
   [ div [ class "grid-controls" ]
     [ viewRowControls 
     , viewColControls
     ]
   , div [ class "grid-container" ]
     [ viewGrid model
     ]
   ]

viewRowControls : Html Msg
viewRowControls =
  div []
    [ label [] [text "Row"]
    , button [onClick (AddRow <| defaultRowUnit)] [text "+"]
    , button [onClick RemoveRow] [text "-"]
    ]

viewColControls : Html Msg
viewColControls =
  div []
    [ label [] [text "Column"]
    , button [onClick (AddCol <| defaultColUnit)] [text "+"]
    , button [onClick RemoveCol] [text "-"]
    ]

viewGrid : Model -> Html Msg
viewGrid model =
  let
    pSections = Grid.gridSections model 
  in
    div [ style <| Grid.gridStyles model ]
      <| List.map viewSection pSections 

viewSection : PositionedSection -> Html Msg
viewSection section =
  let
    attrs =
      case section of
        Placeholder c ->
          [ onClick <| PlaceNewSection c ]
        PositionedSection c s _ ->
          []
  in
    div 
      ( [ class "grid-section", style <| Grid.gridSectionStyles section ] ++ attrs 
      )
      [ viewSectionControls section
      , viewSectionBody section
      ]

viewSectionControls : PositionedSection -> Html Msg
viewSectionControls section =
  let
    controls =
      case section of
        Placeholder _ -> 
          []
        PositionedSection c s _ ->
          [ button [onClick <| RemoveSection c ] [text "⨯"]
          ]
          
  in
    div [ class "grid-section-controls" ] controls

viewSectionBody : PositionedSection -> Html Msg
viewSectionBody section =
  div 
    [ class "grid-section-body" 
    ]
    [
    ]

