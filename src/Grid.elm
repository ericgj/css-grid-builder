module Grid exposing 
  ( Grid, GridUnit(..), AbsoluteUnit(..)
  , Section, Name(..)
  , Model
  , emptyGrid 
  , setRowGap, setColGap, setGap
  , gridRowSize, gridColSize, gridSize, gridRowDim, gridColDim, gridDims
  , gridToStyles
  , emptySectionAt, incrSectionRowSpan, decrSectionRowSpan, incrSectionColSpan, decrSectionColSpan
  , nameSection, unnameSection
  , sectionToStyles
  , gridStyles
  , cropRight, cropBottom 
  , empty, mapGrid, mapSections, indexedMapSections, sections
  , addRow, addCol, removeRow, removeCol, spanRight
  )


-- GRID

type Grid =
  Grid
    { rows : List GridUnit
    , cols : List GridUnit
    , rowGap : AbsoluteUnit
    , colGap : AbsoluteUnit
    }

type GridUnit
  = Abs AbsoluteUnit
  | Fr Int
  | MinContent
  
type AbsoluteUnit
  = Px Int
  | Rem Int

emptyGrid : Grid
emptyGrid =
  Grid { rows = [ ], cols = [ ], rowGap = Px 0, colGap = Px 0 }

addGridRow : GridUnit -> Grid -> Grid
addGridRow unit (Grid grid) =
  Grid { grid | rows = (unit :: grid.rows) }

addGridCol : GridUnit -> Grid -> Grid
addGridCol unit (Grid grid) =
  Grid { grid | cols = (unit :: grid.cols) }

addGridInitial : GridUnit -> GridUnit -> Grid -> Grid
addGridInitial rowunit colunit grid =
  addGridRow rowunit grid |> addGridCol colunit

removeGridRow : Grid -> Grid
removeGridRow (Grid grid) =
  Grid { grid | rows = List.tail grid.rows |> Maybe.withDefault [] }

removeGridCol : Grid -> Grid
removeGridCol (Grid grid) =
  Grid { grid | cols = List.tail grid.cols |> Maybe.withDefault [] }

setRowGap : AbsoluteUnit -> Grid -> Grid
setRowGap unit (Grid grid) =
  Grid { grid | rowGap = unit }

setColGap : AbsoluteUnit -> Grid -> Grid
setColGap unit (Grid grid) =
  Grid { grid | colGap = unit }

setGap : AbsoluteUnit -> Grid -> Grid
setGap unit (Grid grid) =
  Grid { grid | rowGap = unit, colGap = unit }

gridRowSize : Grid -> Int
gridRowSize (Grid grid) =
  List.length grid.rows

gridColSize : Grid -> Int
gridColSize (Grid grid) =
  List.length grid.cols

gridSize : Grid -> (Int, Int)
gridSize grid =
  ( gridRowSize grid, gridColSize grid )

gridRowDim : Grid -> Int
gridRowDim (Grid grid) =
  List.length grid.rows - 1

gridColDim : Grid -> Int
gridColDim (Grid grid) =
  List.length grid.cols - 1

gridDims : Grid -> (Int, Int)
gridDims grid =
  ( gridRowDim grid, gridColDim grid )

isEmptyGrid : Grid -> Bool
isEmptyGrid grid =
  gridSize grid == (0,0)


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

gridToStyles : Grid -> List (String, String)
gridToStyles (Grid {rows, cols, colGap, rowGap}) =
  [ ("display", "grid")
  , ("grid-row-gap", absoluteUnitToString rowGap)
  , ("grid-column-gap", absoluteUnitToString colGap)
  , ("grid-template-rows", gridRowsToString rows)
  , ("grid-template-columns", gridColsToString cols)
  ]


-- SECTIONS

type alias Sections = List Section

type Section =
  Section
    { row : Int
    , col : Int
    , rowSpan : Int
    , colSpan : Int
    , name : Name
    }

type Name
  = Named String
  | Unnamed

emptySectionAt : Int -> Int -> Section
emptySectionAt row col =
  Section { row = row, col = col, rowSpan = 1, colSpan = 1, name = Unnamed }

incrSectionRowSpan : Section -> Section
incrSectionRowSpan (Section section) =
  Section { section | rowSpan = section.rowSpan + 1 }

decrSectionRowSpan : Section -> Section
decrSectionRowSpan (Section section) =
  Section { section | rowSpan = clampMin 1 <| section.rowSpan - 1 }

incrSectionColSpan : Section -> Section
incrSectionColSpan (Section section) =
  Section { section | colSpan = section.colSpan + 1 }

decrSectionColSpan : Section -> Section
decrSectionColSpan (Section section) =
  Section { section | colSpan = clampMin 1 <| section.colSpan - 1 }

nameSection : String -> Section -> Section
nameSection name (Section section) =
  Section { section | name = Named name }

unnameSection : Section -> Section
unnameSection (Section section) =
  Section { section | name = Unnamed }

swapSectionsPair : (Section, Section) -> (Section, Section)
swapSectionsPair (Section a, Section b) =
  ( Section { a | row = b.row, col = b.col, rowSpan = b.rowSpan, colSpan = b.colSpan }
  , Section { b | row = a.row, col = a.col, rowSpan = a.rowSpan, colSpan = a.colSpan }
  )



cropRightOrNothing : Int -> Section -> Maybe Section
cropRightOrNothing cols (Section section) =
  if section.col >= cols then
    Nothing
  else
    if (section.col + section.colSpan) >= cols then
      Just (Section { section | colSpan = cols - section.col })
    else
      Just (Section section)

cropBottomOrNothing : Int -> Section -> Maybe Section
cropBottomOrNothing rows (Section section) =
  if section.row >= rows then
    Nothing
  else
    if (section.row + section.rowSpan) >= rows then
      Just (Section { section | rowSpan = rows - section.row })
    else
      Just (Section section)


sectionToStyles : Section -> List (String, String)
sectionToStyles (Section section) =
  let
    stringDim n span = 
      if span == 1 then 
        (toString (n+1)) 
      else 
        ((toString (n+1)) ++ " / " ++ "span " ++ (toString span))
  in
    [ ("grid-row", stringDim section.row section.rowSpan)
    , ("grid-column", stringDim section.col section.colSpan)
    ]


-- INTERACTIONS

addRowSections : Grid -> Sections -> Sections
addRowSections grid sections =
  List.range 0 (gridColDim grid)
    |> List.map (emptySectionAt (gridRowDim grid))
    |> (++) sections

addColSections : Grid -> Sections -> Sections
addColSections grid sections =
  List.range 0 (gridRowDim grid)
    |> List.map (\row -> emptySectionAt row (gridColDim grid))
    |> (++) sections


cropRight : Grid -> Sections -> Sections
cropRight grid sections =
  List.filterMap (cropRightOrNothing (gridColDim grid)) sections

cropBottom : Grid -> Sections -> Sections
cropBottom grid sections =
  List.filterMap (cropBottomOrNothing (gridRowDim grid)) sections


{- TODO
swapSections : Int -> Int -> Sections -> Sections
swapSections x y sections =
  sections
-}


type Model =
  Model
    { grid : Grid
    , sections : Sections
    }

empty : Model
empty =
  Model { grid = emptyGrid, sections = [] }

mapGrid : (Grid -> Grid) -> Model -> Model
mapGrid fn (Model {grid, sections}) =
  Model { grid = fn grid, sections = sections }

mapSections : (Section -> Section) -> Model -> Model
mapSections fn (Model {grid, sections}) =
  Model { grid = grid, sections = List.map fn sections }

indexedMapSections : (Int -> Section -> Section) -> Model -> Model
indexedMapSections fn (Model {grid, sections}) =
  Model { grid = grid, sections = List.indexedMap fn sections }

sections : Model -> List Section
sections (Model {sections}) =
  sections

addRow : GridUnit -> GridUnit -> Model -> Model
addRow unit colunit (Model {grid, sections}) =
  let
    newGrid = 
      if isEmptyGrid grid then
        addGridInitial unit colunit grid
      else
        addGridRow unit grid
  in
    Model 
      { grid = newGrid  
      , sections = addRowSections newGrid sections
    }

addCol : GridUnit -> GridUnit -> Model -> Model
addCol rowunit unit (Model {grid, sections}) =
  let
    newGrid = 
      if isEmptyGrid grid then
        addGridInitial rowunit unit grid
      else
        addGridCol unit grid
  in
    Model 
      { grid = newGrid  
      , sections = addColSections newGrid sections
      }

removeRow : Model -> Model
removeRow (Model {grid, sections}) =
  let
    newSections = cropBottom grid sections
  in
    Model
      { grid = removeGridRow grid
      , sections = newSections
      }

removeCol : Model -> Model
removeCol (Model {grid, sections}) =
  let
    newSections = cropRight grid sections
  in
    Model
      { grid = removeGridCol grid
      , sections = newSections
      }

spanRight : Int -> Model -> Model
spanRight n model =
  let
    span i (Section section) =
      let 
        (row, col) = (section.row, section.col + section.colSpan)
      in
        if n == i then
          Section { section | colSpan = section.colSpan + 1 }
        else
          Section section
  in
    indexedMapSections span model


gridStyles : Model -> List (String, String)
gridStyles (Model {grid}) =
  gridToStyles grid



-- UTILS

clampMin : Int -> Int -> Int
clampMin min n =
  if n < min then
    min
  else
    n

