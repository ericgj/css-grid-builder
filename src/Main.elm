module Main exposing (..)

import Json.Decode as JD

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)

import Grid exposing 
  (Model, Section, Placeholder, GridUnit, SectionElement(..), px, rem, gridUnit, fr, minContent )


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
    |> Grid.mapGrid (Grid.setGap (px 10))
    |> Grid.addRow (minContent) (minContent)

defaultRowUnit = gridUnit <| px 100
defaultColUnit = fr 1

type Msg 
  = AddRow GridUnit
  | AddCol GridUnit
  | RemoveRow
  | RemoveCol
  | SetRowHeight Int GridUnit
  | SetColWidth Int GridUnit
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

    SetRowHeight index unit ->
      Grid.setRowHeight index unit model

    SetColWidth index unit ->
      Grid.setColWidth index unit model
 
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
   , div [ class "grid-css" ]
     [ lazy viewCSS model
     ]
   ]

viewCSS : Model -> Html Msg
viewCSS model =
  let
    noheaders = Grid.shiftRemoveFirstRow model |> Grid.shiftRemoveFirstCol
  in
    div []
      [ pre []
        [ code []
          [ text <| Grid.gridCSS "#grid" noheaders ]
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
    |> (\i -> Grid.colUnit i model |> Maybe.map (\u -> viewUnit SetColWidth i u))
    |> Maybe.withDefault (text "")

viewRowHeader : Model -> Placeholder -> Html Msg
viewRowHeader model placeholder =
  Grid.placeholderRow placeholder
    |> (\i -> Grid.rowUnit i model |> Maybe.map (\u -> viewUnit SetRowHeight i u))
    |> Maybe.withDefault (text "")


viewUnit : (Int -> GridUnit -> Msg) -> Int -> GridUnit -> Html Msg
viewUnit tagger index unit =
  let
    rangeControl v {min,max,step} =
      input 
        [ type_ "number"
        , value (toString v)
        , Html.Attributes.min (toString min)
        , Html.Attributes.max (toString max)
        , Html.Attributes.step (toString step)
        , onInput (parseUnitValue >> tagger index)
        ] [ ]

    parseUnitValue s =
      String.toFloat s
        |> Result.map (\n -> Grid.gridUnitSetValue n unit)
        |> Result.withDefault unit

    unitControl =
      select 
        [ onChange (parseUnit >> tagger index) ]
        [ option [ value "px", selected <| Grid.gridUnitIsPx unit ]
                 [ text "px" ] 
        , option [ value "rem", selected <| Grid.gridUnitIsRem unit ]
                 [ text "rem" ] 
        , option [ value "fr", selected <| Grid.gridUnitIsFr unit ]
                 [ text "fr" ] 
        , option [ value "min-content", selected <| Grid.gridUnitIsMinContent unit ]
                 [ text "min-content" ] 
        ]

    parseUnit sunit =
      let
        defaultUnitValue = Grid.gridUnitRange >> (Maybe.map .min) >> (Maybe.withDefault 1.0)
      in
        case sunit of
          "px" ->  px 1 |> gridUnit |> defaultUnitValue |> px |> gridUnit
          "rem" -> Grid.rem 1 |> gridUnit |> defaultUnitValue |> Grid.rem |> gridUnit
          "fr" -> fr 1 |> defaultUnitValue |> fr
          "min-content" -> minContent
          _ -> fr 1

  in
    div [] 
      [ div [ class "grid-unit-value" ]
        [ Maybe.map2 rangeControl
            ( Grid.gridUnitValue unit )
            ( Grid.gridUnitRange unit )
              |> Maybe.withDefault (text <| Grid.gridUnitToString unit)
        ]
      , div [ class "test-test" ] [ ]
      , div [ class "grid-unit-unit" ]
        [ unitControl ]
      ]
             
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




-- UTILS

onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  Html.Events.on "change" (JD.map tagger Html.Events.targetValue)

