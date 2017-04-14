module Grid exposing 
  ( Model, Grid, PositionedSection(..)
  , GridCoord, GridUnit(..), AbsoluteUnit(..)
  , empty, mapGrid, mapSections
  , setGap
  , addRow, addCol, removeRow, removeCol
  , addSection, getSection, addSectionAndPlaceAt, placeSectionAt, removeSectionFrom
  , gridSections
  , gridSectionCanExpand
  , expandSectionRightward
  , gridStyles, gridSectionStyles
 )

import Array exposing (Array)
import Dict exposing (Dict)

type Model section =
  Model
    { grid : Grid
    , sections : Array section
    }

type Grid =
  Grid
    { rows : List GridUnit
    , cols : List GridUnit
    , rowGap : AbsoluteUnit
    , colGap : AbsoluteUnit
    , contents : Dict (Int, Int) (Maybe Int)
    }

type PositionedSection
  = PositionedSection GridCoord GridSpan Int
  | Placeholder GridCoord
    
type alias GridCoord =
  { row : Int
  , col : Int
  }

type alias GridSpan =
  { rowSpan : Int
  , colSpan : Int
  }

type GridDirection
  = Upward
  | Leftward
  | Downward
  | Rightward

type GridUnit
  = Abs AbsoluteUnit
  | Fr Int
  | MinContent
  
type AbsoluteUnit
  = Px Int
  | Rem Int


{-------------------------------------------------------------------------------
  MODEL
-------------------------------------------------------------------------------}

empty : Model section
empty =
  Model 
    { grid = emptyGrid
    , sections = Array.empty
    }

mapGrid : (Grid -> Grid) -> Model section -> Model section
mapGrid fn (Model model) =
  Model { model | grid = fn model.grid }

mapSections : (section -> section) -> Model section -> Model section
mapSections fn (Model model) =
  Model { model | sections = Array.map fn model.sections }

addRow : GridUnit -> Model section -> Model section
addRow unit (Model model) =
  Model { model | grid = addGridRow unit model.grid }  

addCol : GridUnit -> Model section -> Model section
addCol unit (Model model) =
  Model { model | grid = addGridCol unit model.grid }

removeRow : Model section -> Model section
removeRow (Model model) =
  Model { model | grid = removeGridRow model.grid }

removeCol : Model section -> Model section
removeCol (Model model) =
  Model { model | grid = removeGridCol model.grid }

addSectionAndPlaceAt : section -> GridCoord -> Model section -> Model section
addSectionAndPlaceAt section coord model =
  addSection section model
    |> (\(Model newModel) -> ((Array.length newModel.sections) - 1, Model newModel))
    |> (\(i, m) -> placeSectionAt coord i m)

addSection : section -> Model section -> Model section
addSection section (Model model) =
  Model { model | sections = Array.push section model.sections }

getSection : Int -> Model section -> Maybe section
getSection index (Model {sections}) =
  Array.get index sections

placeSectionAt : GridCoord -> Int -> Model section -> Model section
placeSectionAt {row,col} index (Model model) =
  Model { model | grid = placeGridContentsAt row col index model.grid }

removeSectionFrom : GridCoord -> Model section -> Model section
removeSectionFrom {row,col} (Model model) =
  Model { model | grid = removeGridContentsAt row col model.grid }

gridStyles : Model section -> List (String,String)
gridStyles (Model {grid}) =
  gridToStyles grid

gridSections : Model section -> List PositionedSection
gridSections (Model {grid}) =
  positionedSectionsInGrid grid

gridSectionCanExpand : PositionedSection -> Model section 
  -> {upward: Bool, leftward: Bool, downward: Bool, rightward: Bool}
gridSectionCanExpand psection (Model {grid}) =
  positionedSectionCanExpand psection grid 

expandSectionRightward : Int -> Model section -> Model section
expandSectionRightward index (Model model) =
  let
    newGrid =
      positionedSectionsInGrid model.grid
        |> (\psections -> expandPositionedSection Rightward index psections model.grid)
  in
    Model { model | grid = newGrid }
  
gridSectionStyles = positionedSectionStyles

{-------------------------------------------------------------------------------
  GRID
-------------------------------------------------------------------------------}

emptyGrid : Grid
emptyGrid =
  Grid 
    { rows = []
    , cols = []
    , rowGap = Px 0
    , colGap = Px 0
    , contents = Dict.empty
    }

setRowGap : AbsoluteUnit -> Grid -> Grid
setRowGap unit (Grid grid) =
  Grid { grid | rowGap = unit }

setColGap : AbsoluteUnit -> Grid -> Grid
setColGap unit (Grid grid) =
  Grid { grid | colGap = unit }

setGap : AbsoluteUnit -> Grid -> Grid
setGap unit (Grid grid) =
  Grid { grid | rowGap = unit, colGap = unit }

addGridRow : GridUnit -> Grid -> Grid
addGridRow unit (Grid grid) =
  let
    colIndexes =
      List.range 0 <| List.length grid.cols - 1
    newRows = 
      unit :: grid.rows
    newContents = 
      let
        accum colindex contents =
          Dict.insert (List.length newRows - 1, colindex) Nothing contents
      in
        List.foldr accum grid.contents colIndexes
  in
    Grid { grid | rows = newRows, contents = newContents }

addGridCol : GridUnit -> Grid -> Grid
addGridCol unit (Grid grid) =
  let
    rowIndexes =
      List.range 0 <| List.length grid.rows - 1
    newCols = 
      unit :: grid.cols
    newContents = 
      let
        accum rowindex contents =
          Dict.insert (rowindex, List.length newCols - 1) Nothing contents
      in
        List.foldr accum grid.contents rowIndexes
  in
    Grid { grid | cols = newCols, contents = newContents }

removeGridRow : Grid -> Grid
removeGridRow (Grid grid) =
  let
    colIndexes =
      List.range 0 <| List.length grid.cols - 1
    newRows =
      List.tail grid.rows |> Maybe.withDefault []
    newContents =
      let
        accum colindex contents =
          Dict.remove (List.length newRows, colindex) contents
      in
        List.foldr accum grid.contents colIndexes
  in
    Grid { grid | rows = newRows, contents = newContents }

removeGridCol : Grid -> Grid
removeGridCol (Grid grid) =
  let
    rowIndexes =
      List.range 0 <| List.length grid.rows - 1
    newCols = 
      List.tail grid.cols |> Maybe.withDefault []
    newContents = 
      let
        accum rowindex contents =
          Dict.remove (rowindex, List.length newCols) contents
      in
        List.foldr accum grid.contents rowIndexes
  in
    Grid { grid | cols = newCols, contents = newContents }


placeGridContentsAt : Int -> Int -> Int -> Grid -> Grid
placeGridContentsAt row col index (Grid grid) =
  if (List.length grid.rows - 1) < row || (List.length grid.cols - 1) < col then
    Grid grid   -- do nothing if coords are outside current grid
  else
    Grid { grid | contents = Dict.insert (row,col) (Just index) grid.contents }

removeGridContentsAt : Int -> Int -> Grid -> Grid
removeGridContentsAt row col (Grid grid) =
  if (List.length grid.rows - 1) < row || (List.length grid.cols - 1) < col then
    Grid grid   -- do nothing if coords are outside current grid
  else
    Grid { grid | contents = Dict.insert (row,col) Nothing grid.contents }


gridToStyles : Grid -> List (String, String)
gridToStyles (Grid {rows, cols, colGap, rowGap}) =
  [ ("display", "grid")
  , ("grid-row-gap", absoluteUnitToString rowGap)
  , ("grid-column-gap", absoluteUnitToString colGap)
  , ("grid-template-rows", gridRowsToString rows)
  , ("grid-template-columns", gridColsToString cols)
  ]

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

gridRowsToString = List.map gridUnitToString >> List.reverse >> String.join " "

gridColsToString = List.map gridUnitToString >> List.reverse >> String.join " "


-- Derivation of positioned sections

placeholderSectionAt : Int -> Int -> PositionedSection
placeholderSectionAt row col =
  Placeholder { row = row, col = col }

positionedSectionAt : Int -> Int -> Int -> PositionedSection
positionedSectionAt row col index =
  PositionedSection 
    { row = row, col = col } 
    { rowSpan = 1, colSpan = 1 }
    index

mapGridContents : ((Int, Int) -> Maybe Int -> Maybe Int) -> Grid -> Grid
mapGridContents fn (Grid grid) =
  Grid { grid | contents = Dict.map fn grid.contents }

foldGridContents : ((Int, Int) -> Maybe Int -> a -> a) -> a -> Grid -> a
foldGridContents accum initial (Grid {contents}) =
  Dict.foldr accum initial contents

positionedSectionsInGrid : Grid -> List PositionedSection
positionedSectionsInGrid grid =
  let
    expandDims newCoord coord span =
      let
        (oldTop, oldBottom) = (coord.row, coord.row + span.rowSpan - 1)
        (oldLeft, oldRight) = (coord.col, coord.col + span.colSpan - 1)
        newTop = if newCoord.row < oldTop then newCoord.row else oldTop
        newLeft = if newCoord.col < oldLeft then newCoord.col else oldLeft
        newBottom = if newCoord.row > oldBottom then newCoord.row else oldBottom
        newRight = if newCoord.col > oldRight then newCoord.col else oldRight
      in
        ( { row = newTop, col = newLeft }
        , { rowSpan = newBottom - newTop + 1,  colSpan = newRight - newLeft + 1 }
        )

    updatePositionedSection row col i psection =
       case psection of
         Placeholder _ ->
           positionedSectionAt row col i

         PositionedSection coord span _ ->
           let
             (newCoord, newSpan) = expandDims {row = row, col = col} coord span
           in
             PositionedSection newCoord newSpan i

    accum (row,col) mindex (psections, placeholders) =
      let
        newPlaceholder = placeholderSectionAt row col 
      in 
        case mindex of
          Nothing ->
            ( psections, newPlaceholder :: placeholders )
          Just i ->
            ( Dict.get i psections
                |> Maybe.withDefault newPlaceholder
                |> updatePositionedSection row col i
                |> (\p -> Dict.insert i p psections)
            , placeholders
            )
  in
    foldGridContents accum (Dict.empty, []) grid
      |> (\(psections, placeholders) -> Dict.values psections ++ placeholders) 


positionedSectionsToGrid : List PositionedSection -> Grid -> Grid
positionedSectionsToGrid psections (Grid grid) =
  let
    accum psection grid =
      case psection of
        Placeholder {row,col} ->
          Dict.insert (row,col) Nothing grid
        PositionedSection {row,col} {rowSpan,colSpan} i ->
          let
            accumSection (r,c) grid_ =
              Dict.insert (r,c) (Just i) grid_
          in
            listCombine (,) 
              (List.range row (row + rowSpan - 1)) 
              (List.range col (col + colSpan - 1))
                |> List.foldr accumSection grid
  in
    Grid { grid | contents = List.foldr accum Dict.empty psections }


{-------------------------------------------------------------------------------
  POSITIONED SECTION
-------------------------------------------------------------------------------}

positionedSectionCanExpand : PositionedSection -> Grid
  -> {upward: Bool, leftward: Bool, downward: Bool, rightward: Bool}
positionedSectionCanExpand psection (Grid {rows,cols}) =
  case psection of
    Placeholder _ ->
      { upward = False, leftward = False, downward = False, rightward = False }

    PositionedSection {row,col} {rowSpan,colSpan} _ ->
      { upward =
          row - 1 > 0
      , leftward =
          col - 1 > 0
      , downward =
          row + rowSpan < List.length rows
      , rightward =
          col + colSpan < List.length cols
      }

expandPositionedSection : GridDirection -> Int -> List PositionedSection -> Grid -> Grid
expandPositionedSection direction index psections (Grid {rows,cols} as grid) =
  let 
    expand psection =
      case psection of
        Placeholder _ ->
          psection
        PositionedSection coords spans section ->
          if index == section then
            case direction of
              Upward ->
                if coords.row - 1 > 0 then 
                  PositionedSection { coords | row = coords.row - 1 } spans section
                else
                  psection
              Leftward ->
                if coords.col - 1 > 0 then
                   PositionedSection { coords | col = coords.col - 1 } spans section
                else
                  psection
              Downward ->
                if coords.row + spans.rowSpan < List.length rows then
                  PositionedSection coords { spans | rowSpan = spans.rowSpan + 1 } section
                else
                  psection
              Rightward ->
                if coords.col + spans.colSpan < List.length cols then
                  PositionedSection coords { spans | colSpan = spans.colSpan + 1 } section
                else
                  psection
          else
            psection
  in
    List.map expand psections
      |> (\expanded -> positionedSectionsToGrid expanded grid)


positionedSectionStyles : PositionedSection -> List (String, String)
positionedSectionStyles section =
  case section of
    Placeholder coord ->
      placeholderDataToStyles coord
    PositionedSection coord span _ ->
      sectionDataToStyles coord span

sectionDataToStyles : GridCoord -> GridSpan -> List (String, String)
sectionDataToStyles coord span =
  let
    stringDim n sz = 
      if sz == 1 then 
        (toString (n+1)) 
      else 
        ((toString (n+1)) ++ " / " ++ "span " ++ (toString sz))
  in
    [ ("grid-row", stringDim coord.row span.rowSpan)
    , ("grid-column", stringDim coord.col span.colSpan)
    , ("background-color", "#FFF")
    , ("border", "1px solid #CCC")
    ]

placeholderDataToStyles : GridCoord -> List (String, String)
placeholderDataToStyles coord =
  [ ("grid-row", coord.row + 1 |> toString)
  , ("grid-column", coord.col + 1 |> toString)
  , ("background-color", "inherit")
  , ("border", "1px dashed #CCC")
  ]


-- UTILS

listCombine : (a -> b -> c) -> List a -> List b -> List c
listCombine fn a b =
  List.concatMap (\a_ -> List.map (\b_ -> fn a_ b_) b ) a

