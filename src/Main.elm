module Main exposing (..)

import Array exposing (Array)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- GRID -----------------------------------------------------------------------

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

gridSize : Grid -> Int
gridSize (Grid grid) =
  Array.length grid.cells

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

addCells : Int -> GridUnit -> GridUnit -> Grid -> Grid
addCells n col row grid =
  List.range 0 n 
    |> List.foldr (\i g -> addCell col row g) grid

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


-- SECTIONS ----------------------------------------------------------------

type alias Sections = Array Section

type Section =
  Section
    { span : SectionSpan }

type SectionSpan
  = NoSpan
  | Span Int

mapSectionSpan : (SectionSpan -> SectionSpan) -> Section -> Section
mapSectionSpan fn (Section {span}) =
  Section { span = fn span }

incrSpan : SectionSpan -> SectionSpan
incrSpan span =
  case span of
    NoSpan -> Span 2
    Span n -> Span (n + 1)

decrSpan : SectionSpan -> SectionSpan
decrSpan span =
  case span of
    NoSpan -> NoSpan
    Span n ->
      case n of
        2 -> NoSpan
        _ -> Span (n-1)

noSpan : SectionSpan
noSpan = NoSpan

emptySection : Section
emptySection =
  Section { span = noSpan }

mergeSectionRight : Int -> Sections -> Sections
mergeSectionRight = mergeSectionHelper 1

mergeSectionLeft : Int -> Sections -> Sections
mergeSectionLeft = mergeSectionHelper (-1)

mergeSectionHelper : Int -> Int -> Sections -> Sections
mergeSectionHelper offset n sections =
  let 
    setSpan sections_ =
      Array.get n sections_
      |> Maybe.map (mapSectionSpan incrSpan)
      |> Maybe.map (\newSection -> Array.set n newSection sections_)
      |> Maybe.withDefault sections_

    removeSection sections_ =
      arrayRemove (n + offset) sections_

  in
    setSpan sections |> removeSection


gridColumnSpanStyle : SectionSpan -> List (String, String)
gridColumnSpanStyle span =
  case span of
    NoSpan -> []
    Span n -> [ ("grid-column-span", toString n) ]


-- ARRAY UTILS ----------------------------------------------------------------

clampMin : Int -> Int -> Int
clampMin min n =
  if n < min then
    min
  else
    n

arrayInsertBefore : Int -> a -> Array a -> Array a
arrayInsertBefore i a ary =
  (Array.slice 0 i ary)
    |> Array.push a
    |> flip Array.append (Array.slice i ((Array.length ary) + 1) ary)

arrayRemove : Int -> Array a -> Array a
arrayRemove i ary =
  (Array.slice 0 i ary)
    |> flip Array.append (Array.slice (i+1) ((Array.length ary) + 1) ary)

arrayRemoveSlice : Int -> Int -> Array a -> Array a
arrayRemoveSlice i j ary =
  Array.append 
    (Array.slice j ((Array.length ary) + 1) ary)
    (Array.slice 0 i ary)

arrayRemoveFromRight : Int -> Array a -> Array a
arrayRemoveFromRight i ary =
  arrayRemoveSlice
    (clampMin 0 <| (Array.length ary) - i)
    ((Array.length ary) + 1)
    ary


-- ----------------------------------------------------------------

testGrid = 
  emptyGrid
    |> addCell (Fr 1) (Px 100)
    |> addCol (Fr 2)
    |> addCol (Rem 20)
    |> setColGap 10

test =
  Model { grid = testGrid, sections = Array.fromList [emptySection, emptySection, emptySection ] }


main : Program Never Model Msg
main = 
  beginnerProgram 
    { view = view
    , update = update
    , model = emptyModel |> mapGrid (setColGap 10)
    }

emptyModel : Model
emptyModel =
  Model { grid = emptyGrid, sections = Array.empty }

type Model =
  Model
    { grid : Grid
    , sections : Sections
    }

mapGrid : (Grid -> Grid) -> Model -> Model
mapGrid fn (Model model) =
  Model { model | grid = fn model.grid }

setGrid : Grid -> Model -> Model
setGrid grid (Model model) =
  Model { model | grid = grid }

addSection : Model -> Model
addSection (Model model) =
  Model { model | sections = Array.push emptySection model.sections }

removeSection : Model -> Model
removeSection (Model model) =
  Model { model | sections = Array.slice 0 -1 model.sections }

addRowSections : Model -> Model
addRowSections (Model model) =
  let 
    cols (Grid grid) = 
      Array.length grid.cells
    newSections n =
      Array.fromList <| List.map (always emptySection) (List.range 0 n)
  in
    Model { model | sections = Array.append (cols model.grid |> newSections) model.sections }

removeRowSections : Model -> Model
removeRowSections (Model model) =
  let 
    cols (Grid grid) = Array.length grid.cells
  in
    Model { model | sections = arrayRemoveFromRight (cols model.grid) model.sections }


type Msg 
  = SetGridSize Int
  | DupLeft Int
  | Remove Int
  | Incr Int
  | Decr Int
  | AddRow
  | MergeLeft Int
  | MergeRight Int

update : Msg -> Model -> Model
update msg (Model {grid, sections}) =
  updateHelper msg grid sections

updateHelper : Msg -> Grid -> Sections -> Model
updateHelper msg (Grid grid) sections =
  let 
    decrWidth (GridCell cell) =
      let decrCol col =
        case col of
          Fr w -> Fr <| clampMin 1 (w-1)
          Px w ->  Px <| clampMin 1 (w-1)
          Rem w -> Rem <| clampMin 1 (w-1)
          MinContent -> MinContent
      in
        GridCell {cell | col = decrCol cell.col }

    incrWidth (GridCell cell) =
      let incrCol col =
        case col of
          Fr w -> Fr <| clampMin 1 (w+1)
          Px w ->  Px <| clampMin 1 (w+1)
          Rem w -> Rem <| clampMin 1 (w+1)
          MinContent -> MinContent
      in
        GridCell {cell | col = incrCol cell.col }

  in
    case msg of 

      SetGridSize n ->
        let 
          currentSize = Array.length grid.cells
        in
          if currentSize == n then
            Model { grid = Grid grid, sections = sections }
          else 
            if currentSize > n then 
              Array.slice 0 n grid.cells
                |> (\cells -> Grid {grid | cells = cells})
                |> (\g -> Model {grid = g, sections = sections})
            else
              addCells (n - currentSize) (Fr 1) (MinContent) (Grid grid)
                |> (\g -> Model {grid = g, sections = sections})


      DupLeft i ->
        Array.get i grid.cells
          |> Maybe.map (\it -> arrayInsertBefore i it grid.cells)
          |> Maybe.map (\cells -> Grid { grid | cells = cells })
          |> Maybe.withDefault (Grid grid)
          |> (\g -> Model {grid = g, sections = sections})
          |> addSection
      
      Remove i ->
        Grid { grid | cells = arrayRemove i grid.cells }
          |> (\g -> Model {grid = g, sections = sections})
          |> removeSection
        
      Decr i ->
        Array.get i grid.cells
          |> Maybe.map decrWidth
          |> Maybe.map (\cell -> Grid { grid | cells = Array.set i cell grid.cells })
          |> Maybe.withDefault (Grid grid)
          |> (\g -> Model { grid = g, sections = sections })

      Incr i ->
        Array.get i grid.cells
          |> Maybe.map incrWidth
          |> Maybe.map (\cell -> Grid { grid | cells = Array.set i cell grid.cells })
          |> Maybe.withDefault (Grid grid)
          |> (\g -> Model { grid = g, sections = sections })

      AddRow ->
        Model { grid = Grid grid, sections = sections } 
          |> addRowSections

      MergeLeft i ->
        mergeSectionLeft i sections
          |> (\newSections -> Model { grid = Grid grid, sections = newSections } )

      MergeRight i ->
        mergeSectionRight i sections
          |> (\newSections -> Model { grid = Grid grid, sections = newSections } )


view : Model -> Html Msg
view (Model {grid, sections}) =
  div [] 
    [ viewGridSpec grid
    , div [ class "grid", style <| gridToStyles grid ]
        <| Array.toList <| Array.indexedMap viewSection sections
    , viewAddRow
    ]

viewGridSpec : Grid -> Html Msg
viewGridSpec grid =
  div []
    [ input 
      [ type_ "number"
      , value (toString <| gridSize grid)
      , onInput (String.toInt >> Result.withDefault (gridSize grid) >> SetGridSize)
      ] []
    ]

viewSection : Int -> Section -> Html Msg
viewSection i (Section {span}) =
  div [ style <| gridColumnSpanStyle span ]
    [ div [ class "controls" ]
      [ button [onClick (MergeLeft i)] [text "<"]
      , button [onClick (MergeRight i)] [text ">"]
      ]
    ]

viewAddRow : Html Msg
viewAddRow =
  div [ ]
    [ button [onClick AddRow] [text "+"]
    ]



