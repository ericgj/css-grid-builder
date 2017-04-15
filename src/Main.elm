module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Grid exposing 
  (Model, Section, Placeholder, SectionElement(..), AbsoluteUnit(..), GridUnit(..))


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
  | AddSection Placeholder
  | RemoveSection Section
  | ExpandRightward Section
  | ExpandLeftward Section

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddRow unit ->
      Grid.addRow unit defaultColUnit model

    AddCol unit ->
      Grid.addCol unit defaultRowUnit model

    RemoveRow ->
      Grid.removeRow model

    RemoveCol ->
      Grid.removeCol model
 
    AddSection placeholder ->
      Grid.addSection placeholder model

    RemoveSection section ->
      Grid.removeSection section model

    ExpandLeftward section ->
      Grid.expandLeftward section model

    ExpandRightward section ->
      Grid.expandRightward section model

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
  div [ style <| Grid.gridStyles model ]
    <| List.map (viewSection model) (Grid.sectionElements model) 

viewSection : Model -> SectionElement -> Html Msg
viewSection model el =
  let
    attrs =
      case el of
        PlaceholderElement placeholder ->
          [ onClick <| AddSection placeholder ]
        SectionElement _ ->
          []
  in
    div 
      ( [ class "grid-section", style <| Grid.gridSectionStyles el ] ++ attrs 
      )
      [ viewSectionControls model el
      , viewSectionBody el
      ]

viewSectionControls : Model -> SectionElement -> Html Msg
viewSectionControls model el =
  let
    canExpand section =
      Grid.canExpand section model

    expandButton msg symb =
      button [onClick <| msg] [text symb]

    spanControls section =
      let
        exp = canExpand section 
      in
        ( if exp.leftward then [ expandButton (ExpandLeftward section) "<" ] else [])   ++
        ( if exp.rightward then [ expandButton (ExpandRightward section) ">" ] else [])

    controls =
      case el of
        PlaceholderElement _ -> 
          []
        SectionElement section ->
          [ button [onClick <| RemoveSection section] [text "⨯"] ]
          ++ (spanControls section)
  in
    div [ class "grid-section-controls" ] controls

viewSectionBody : SectionElement -> Html Msg
viewSectionBody el =
  div 
    [ class "grid-section-body" 
    ]
    [
    ]

