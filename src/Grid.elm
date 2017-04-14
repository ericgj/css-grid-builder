module Grid exposing 
  ( Model, Grid, Section, PositionedSection(..)
  , GridCoord, GridUnit(..), AbsoluteUnit(..)
  , empty, mapGrid, mapSections
  , setGap
  , addRow, addCol, removeRow, removeCol
  , addSection, addSectionAndPlaceAt, placeSectionAt, removeSectionFrom
  , gridSections
  , gridStyles, gridSectionStyles
 )

import Array exposing (Array)
import Dict exposing (Dict)

type Model =
  Model
    { grid : Grid
    , sections : Array Section
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

type Section =
  Section
    { name : String }  -- or whatever, besides position in grid

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

empty : Model
empty =
  Model 
    { grid = emptyGrid
    , sections = Array.empty
    }

mapGrid : (Grid -> Grid) -> Model -> Model
mapGrid fn (Model model) =
  Model { model | grid = fn model.grid }

mapSections : (Section -> Section) -> Model -> Model
mapSections fn (Model model) =
  Model { model | sections = Array.map fn model.sections }

addRow : GridUnit -> Model -> Model
addRow unit (Model model) =
  Model { model | grid = addGridRow unit model.grid }  

addCol : GridUnit -> Model -> Model
addCol unit (Model model) =
  Model { model | grid = addGridCol unit model.grid }

removeRow : Model -> Model
removeRow (Model model) =
  Model { model | grid = removeGridRow model.grid }

removeCol : Model -> Model
removeCol (Model model) =
  Model { model | grid = removeGridCol model.grid }

addSectionAndPlaceAt : Section -> GridCoord -> Model -> Model
addSectionAndPlaceAt section coord model =
  addSection section model
    |> (\(Model newModel) -> ((Array.length newModel.sections) - 1, Model newModel))
    |> (\(i, m) -> placeSectionAt coord i m)

addSection : Section -> Model -> Model
addSection section (Model model) =
  Model { model | sections = Array.push section model.sections }

placeSectionAt : GridCoord -> Int -> Model -> Model
placeSectionAt {row,col} index (Model model) =
  Model { model | grid = placeGridContentsAt row col index model.grid }

removeSectionFrom : GridCoord -> Model -> Model
removeSectionFrom {row,col} (Model model) =
  Model { model | grid = removeGridContentsAt row col model.grid }

gridStyles : Model -> List (String,String)
gridStyles (Model {grid}) =
  gridToStyles grid

gridSections : Model -> List PositionedSection
gridSections (Model {grid}) =
  positionedSectionsInGrid grid

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
        , { rowSpan = newBottom - newTop,  colSpan = newRight - newLeft }
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


{-------------------------------------------------------------------------------
  POSITIONED SECTION
-------------------------------------------------------------------------------}

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


