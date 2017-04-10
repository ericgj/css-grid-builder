module Main exposing (..)

import Array exposing (Array)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


test = 
  emptyGrid
    |> addCell (Fr 1) (Px 100)
    |> addCol (Fr 2)
    |> addCol (Rem 20)
    |> setColGap 10

main : Program Never Grid Msg
main = 
  beginnerProgram 
    { view = view
    , update = update
    , model = test
    }
  
type GridUnit
  = Fr Int
  | Px Int
  | Rem Int
  | MinContent

type GridCell =
  GridCell
    { col : GridUnit
    , row : GridUnit
    }
    
type Grid = 
  Grid
    { cells : Array GridCell 
    , colGap : Int
    , rowGap : Int
    }

emptyGrid : Grid
emptyGrid =
  Grid
    { cells = Array.empty 
    , colGap = 0
    , rowGap = 0
    }
    
defaultGridCell : GridCell
defaultGridCell =
  GridCell
    { col = MinContent
    , row = MinContent
    }

withCol : GridUnit -> GridCell -> GridCell
withCol col (GridCell cell) =
  GridCell {cell | col = col}

withRow : GridUnit -> GridCell -> GridCell
withRow row (GridCell cell) =
  GridCell {cell | row = row}


setColGap : Int -> Grid -> Grid
setColGap n (Grid grid) =
  Grid {grid | colGap = n}

setRowGap : Int -> Grid -> Grid
setRowGap n (Grid grid) =
  Grid {grid | rowGap = n}

addCell : GridUnit -> GridUnit -> Grid -> Grid
addCell col row (Grid grid) =
  let newCell =
    defaultGridCell |> withCol col |> withRow row
  in
    Grid { grid | cells = Array.push newCell grid.cells }

addCol : GridUnit -> Grid -> Grid
addCol col grid =
  addCell col MinContent grid
  
addRow : GridUnit -> Grid -> Grid
addRow row grid =
  addCell MinContent row grid
  


gridUnitToString : GridUnit -> String
gridUnitToString unit =
  case unit of
    Fr i -> (toString i) ++ "fr"
    Px i -> (toString i) ++ "px"
    Rem i -> (toString i) ++ "rem"
    MinContent -> "min-content"

gridCellsToStyles : List GridCell -> List (String, String)
gridCellsToStyles cells =
  let
    colString (GridCell {col}) = gridUnitToString col
    rowString (GridCell {row}) = gridUnitToString row
  in
    [ ("grid-template-columns", String.join " " <| List.map colString cells)
    , ("grid-template-rows", String.join " " <| List.map rowString cells)
    ]

gridToStyles : Grid -> List (String, String)
gridToStyles (Grid {cells, colGap, rowGap}) =
  [ ("display", "grid")
  , ("grid-column-gap", (toString colGap) ++ "px")
  , ("grid-row-gap", (toString rowGap) ++ "px")
  ] ++ (gridCellsToStyles <| Array.toList <| cells)


-- ----------------------------------------------------------------

type Msg 
  = DupLeft Int
  | Remove Int
  | Incr Int
  | Decr Int
  | AddCell

update : Msg -> Grid -> Grid
update msg (Grid grid) =
  let 
    insertBefore i x a =
      (Array.slice 0 i a)
        |> Array.push x
        |> flip Array.append (Array.slice i ((Array.length a) + 1) a)
    
    remove i a =
      (Array.slice 0 i a)
        |> flip Array.append (Array.slice (i+1) ((Array.length a) + 1) a)

    decrWidth (GridCell cell) =
      let decrCol col =
        case col of
          Fr w -> Fr <| clamp 1 10000 (w-1)
          Px w ->  Px <| clamp 1 10000 (w-1)
          Rem w -> Rem <| clamp 1 10000 (w-1)
          MinContent -> MinContent
      in
        GridCell {cell | col = decrCol cell.col }

    incrWidth (GridCell cell) =
      let incrCol col =
        case col of
          Fr w -> Fr <| clamp 1 10000 (w+1)
          Px w ->  Px <| clamp 1 10000 (w+1)
          Rem w -> Rem <| clamp 1 10000 (w+1)
          MinContent -> MinContent
      in
        GridCell {cell | col = incrCol cell.col }

  in
    case msg of 
      DupLeft i ->
        Array.get i grid.cells
          |> Maybe.map (\it -> insertBefore i it grid.cells)
          |> Maybe.map (\cells -> Grid { grid | cells = cells })
          |> Maybe.withDefault (Grid grid)
      
      Remove i ->
        Grid { grid | cells = remove i grid.cells }
        
      Decr i ->
        Array.get i grid.cells
          |> Maybe.map decrWidth
          |> Maybe.map (\cell -> Grid { grid | cells = Array.set i cell grid.cells })
          |> Maybe.withDefault (Grid grid)

      Incr i ->
        Array.get i grid.cells
          |> Maybe.map incrWidth
          |> Maybe.map (\cell -> Grid { grid | cells = Array.set i cell grid.cells })
          |> Maybe.withDefault (Grid grid)

      AddCell ->
        Array.get ((Array.length grid.cells) - 1) grid.cells
          |> Maybe.map (\cell -> Grid { grid | cells = Array.push cell grid.cells } )
          |> Maybe.withDefault (Grid grid)

view : Grid -> Html Msg
view (Grid grid as g) =
  div []
    [ div [ class "grid", style <| gridToStyles g ]
        <| Array.toList <| Array.indexedMap viewCell grid.cells
    , viewAddCell
    ]


viewCell : Int -> GridCell -> Html Msg
viewCell i (GridCell {col,row}) =
  div []
    [ div [ class "size" ]
      [ div [] [text <| "↔" ++ (gridUnitToString col) ]
      , div [] [text <| "↕" ++ (gridUnitToString row) ]
      , button [onClick (Decr i)] [text "<"]
      , button [onClick (Incr i)] [text ">"]
      , button [onClick (DupLeft i)] [text "+"]
      , button [onClick (Remove i)] [text "-"]
      ]
    ]


viewAddCell : Html Msg
viewAddCell =
  div [class "grid-add"]
    [ button [onClick AddCell] [ text "+" ] ]
    
 





