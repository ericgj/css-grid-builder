module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Grid exposing 
  (Model, GridUnit(..), AbsoluteUnit(..), PositionedSection(..), GridCoord)

type Section
  = Unnamed
  | Named String

emptySection : Section
emptySection =
  Unnamed

main : Program Never (Model Section) Msg
main = 
  beginnerProgram 
    { view = view
    , update = update
    , model = init
    }

init : Model Section
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
  | ExpandRightward Int

update : Msg -> Model Section -> Model Section
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
      Grid.addSectionAndPlaceAt emptySection coord model

    RemoveSection coord ->
      Grid.removeSectionFrom coord model

    ExpandRightward i ->
      Grid.expandSectionRightward i model

view : Model Section -> Html Msg
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

viewGrid : Model Section -> Html Msg
viewGrid model =
  let
    pSections = Grid.gridSections model
  in
    div [ style <| Grid.gridStyles model ]
      <| List.map (viewSection model) pSections 

viewSection : Model Section -> PositionedSection -> Html Msg
viewSection model psection =
  let
    attrs =
      case psection of
        Placeholder c ->
          [ onClick <| PlaceNewSection c ]
        PositionedSection c s _ ->
          []
  in
    div 
      ( [ class "grid-section", style <| Grid.gridSectionStyles psection ] ++ attrs 
      )
      [ viewSectionControls model psection
      , viewSectionBody model psection
      ]

viewSectionControls : Model Section -> PositionedSection -> Html Msg
viewSectionControls model psection =
  let
    canExpand =
      Grid.gridSectionCanExpand psection model

    canExpandControl accessor tagger index =
      if accessor canExpand then 
        Just <| button [onClick (tagger index)] [ text ">"]
      else
        Nothing

    canExpandControls =
      case psection of
        Placeholder _ ->
          []
        PositionedSection c s i ->
          List.filterMap identity
            [ (canExpandControl .rightward ExpandRightward i) ]
      
    baseControls =
      case psection of
        Placeholder _ -> 
          []
        PositionedSection c s i ->
          [ button [onClick <| RemoveSection c ] [text "⨯"]
          ]
          
  in
    div [ class "grid-section-controls" ] (canExpandControls ++ baseControls)

viewSectionBody : Model Section -> PositionedSection -> Html Msg
viewSectionBody model psection =
  let 
    contents =
      case psection of
        Placeholder _ ->
          []
        PositionedSection c s i ->
          [ Grid.getSection i model
              |> Maybe.map viewSectionBodyContents
              |> Maybe.withDefault (text "")
          ]

  in
    div [ class "grid-section-body" ] contents

viewSectionBodyContents : Section -> Html Msg
viewSectionBodyContents section =
  case section of
    Unnamed ->
      text "(unnamed)"
    Named s ->
      text s


