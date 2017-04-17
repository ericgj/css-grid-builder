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
    |> Grid.addRow (MinContent) (MinContent)

defaultRowUnit = Abs (Px 100)
defaultColUnit = Fr 1

type Msg 
  = AddRow GridUnit
  | AddCol GridUnit
  | RemoveRow
  | RemoveCol
  | AddSection Placeholder
  | RemoveSection Section
  | ExpandUpward Section
  | ExpandLeftward Section
  | ExpandDownward Section
  | ExpandRightward Section

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

    ExpandUpward section ->
      Grid.expandSectionUpward section model

    ExpandLeftward section ->
      Grid.expandSectionLeftward section model

    ExpandDownward section ->
      Grid.expandSectionDownward section model

    ExpandRightward section ->
      Grid.expandSectionRightward section model

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
    <| List.map (viewSectionEl model) (Grid.sectionElements model) 

viewSectionEl : Model -> SectionElement -> Html Msg
viewSectionEl model el =
  let
    commonAttrs =
      [ class "grid-section", style <| Grid.gridSectionStyles el ]

    (attrs, body) =
      case el of
        PlaceholderElement placeholder ->
          case (Grid.placeholderInRow 0 placeholder, Grid.placeholderInCol 0 placeholder) of
            (True, True) ->
              ( [ class "grid-section-corner" ]
              , [ ]
              )

            (True, False) ->
              ( [ class "grid-section-colheader"]
              , [ viewColHeader model placeholder ]
              )

            (False, True) ->
              ( [ class "grid-section-rowheader"]
              , [ viewRowHeader model placeholder]
              )

            (False, False) ->
              ( [ class "grid-section-placeholder"
                , onClick <| AddSection placeholder 
                ]
              , [ ]
              )

        SectionElement section ->
            ( [ ]
            , [ viewSectionControls model section
              , viewSectionBody section
              ]
            )
  in
    div 
      ( commonAttrs ++ attrs )
      body

viewColHeader : Model -> Placeholder -> Html Msg
viewColHeader model placeholder =
  Grid.placeholderCol placeholder
    |> (\i -> Grid.colUnit i model |> Maybe.map (\u -> viewGridUnit i u))
    |> Maybe.withDefault (text "")

viewRowHeader : Model -> Placeholder -> Html Msg
viewRowHeader model placeholder =
  Grid.placeholderRow placeholder
    |> (\i -> Grid.rowUnit i model |> Maybe.map (\u -> viewGridUnit i u))
    |> Maybe.withDefault (text "")

viewGridUnit : Int -> GridUnit -> Html Msg
viewGridUnit index unit =
  div [] [ text <| gridUnitToString unit ]

viewSectionControls : Model -> Section -> Html Msg
viewSectionControls model section =
  let
    canExpand =
      Grid.canExpandSection section model
        |> exceptIfSecondRowOrCol

    exceptIfSecondRowOrCol exp =
      { exp 
          | upward = exp.upward && (Grid.sectionInRow 1 section |> not)
          , leftward = exp.leftward && (Grid.sectionInCol 1 section |> not)
      }

    expandButton msg symb =
      button [onClick <| msg] [text symb]

    spanControls =
      let
        exp = canExpand 
      in
        ( if exp.upward then [ expandButton (ExpandUpward section) "⇡" ] else [])   ++
        ( if exp.leftward then [ expandButton (ExpandLeftward section) "⇠" ] else [])   ++
        ( if exp.downward then [ expandButton (ExpandDownward section) "⇣" ] else [])   ++
        ( if exp.rightward then [ expandButton (ExpandRightward section) "⇢" ] else [])

  in
    div 
      [ class "grid-section-controls" ] 
      ( [ button [onClick <| RemoveSection section] [text "⨯"] ] ++ spanControls
      )

viewSectionBody : Section -> Html Msg
viewSectionBody section =
  div 
    [ class "grid-section-body" 
    ]
    [
    ]



-- Temporary

absoluteUnitToString : AbsoluteUnit -> String
absoluteUnitToString unit =
  case unit of
    Px i -> (toString i) ++ "px"
    Rem i -> (toString i) ++ "rem"

gridUnitToString : GridUnit -> String
gridUnitToString unit =
  case unit of
    Abs u -> absoluteUnitToString u
    Fr i -> (toString i) ++ "fr"
    MinContent -> "min-content"


