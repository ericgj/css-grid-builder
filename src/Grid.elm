module Grid exposing 
  ( Model, Section, Placeholder, SectionElement(..), AbsoluteUnit(..), GridUnit(..)
  , empty, mapGrid
  , addRow, addCol, removeRow, removeCol
  , addSection, removeSection, place, sectionElements, gridStyles, gridSectionStyles
  , canExpand, expandLeftward, expandRightward
  , setRowGap, setColGap, setGap
  )

type Model =
  Model
    { grid : Grid
    , sections : List Section
    }

type Grid =
  Grid
    { rows : List GridUnit
    , cols : List GridUnit
    , rowGap : AbsoluteUnit
    , colGap : AbsoluteUnit
    }

type Section =
  Section
    { row : Int
    , col : Int
    , rowSpan : Int
    , colSpan : Int
    }

type Placeholder =
  Placeholder
    { row : Int
    , col : Int
    }

type SectionElement
  = SectionElement Section
  | PlaceholderElement Placeholder

type alias Rect = 
  { top : Int
  , left : Int
  , bottom : Int
  , right : Int
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
empty : Model
empty =
  Model
    { grid = emptyGrid
    , sections = []
    }

mapGrid : (Grid -> Grid) -> Model -> Model
mapGrid fn (Model model) =
  Model { model | grid = fn model.grid }

addRow : GridUnit -> GridUnit -> Model -> Model
addRow unit colunit (Model model) =
  let
    preAddCol (Grid grid) =
      if List.isEmpty grid.cols then
        addGridCol colunit (Grid grid)
      else
        Grid grid
  in
    Model { model | grid = addGridRow unit <| preAddCol model.grid }  

addCol : GridUnit -> GridUnit -> Model -> Model
addCol unit rowunit (Model model) =
  let
    preAddRow (Grid grid) =
      if List.isEmpty grid.rows then
        addGridRow rowunit (Grid grid)
      else
        Grid grid
  in
    Model { model | grid = addGridCol unit <| preAddRow model.grid }

removeRow : Model -> Model
removeRow (Model model) =
  Model { model | grid = removeGridRow model.grid }

removeCol : Model -> Model
removeCol (Model model) =
  Model { model | grid = removeGridCol model.grid }

addSection : Placeholder -> Model -> Model
addSection (Placeholder {row,col}) model =
  place (sectionAt (row,col)) model

removeSection : Section -> Model -> Model
removeSection section (Model model) =
  Model { model | sections = removeSectionFrom section model.sections }

place : Section -> Model -> Model
place section (Model model) =
  Model { model | sections = placeSectionIn model.grid section model.sections }

canExpand : Section -> Model
  -> { upward : Bool, leftward : Bool, downward : Bool, rightward : Bool }
canExpand section (Model {grid,sections}) =
  canExpandSection grid section sections

expand : GridDirection -> Section -> Model -> Model
expand direction section (Model model) =
  let
    model_ = removeSection section (Model model)
  in
    sectionExpand direction model.grid section 
      |> Maybe.map (\s -> place s model_)
      |> Maybe.withDefault model_

expandRightward = expand Rightward
expandLeftward = expand Leftward

sectionElements : Model -> List SectionElement
sectionElements (Model {grid,sections}) =
  gridSectionElements grid sections


gridStyles : Model -> List (String,String)
gridStyles (Model {grid}) =
  gridToStyles grid

gridSectionStyles = sectionElementToStyles


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
  Grid { grid | rows = unit :: grid.rows }

addGridCol : GridUnit -> Grid -> Grid
addGridCol unit (Grid grid) =
  Grid { grid | cols = unit :: grid.cols }

removeGridRow : Grid -> Grid
removeGridRow (Grid grid) =
  Grid { grid | rows = List.tail grid.rows |> Maybe.withDefault [] }

removeGridCol : Grid -> Grid
removeGridCol (Grid grid) =
  Grid { grid | cols = List.tail grid.cols |> Maybe.withDefault [] }


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



{-------------------------------------------------------------------------------
  SECTION
-------------------------------------------------------------------------------}

sectionAt : (Int, Int) -> Section
sectionAt (row, col) =
  rectSection {top = row, left = col, bottom = row, right = col}

canPlaceSectionIn : Grid -> Section -> List Section -> Bool
canPlaceSectionIn grid section sections =
  (List.all (not << sectionIntersects section) sections) &&
  (sectionInsideGrid section grid)

placeSectionIn : Grid -> Section -> List Section -> List Section
placeSectionIn grid section sections =
  if canPlaceSectionIn grid section sections then
    section :: sections
  else
    sections

canExpandSection : Grid -> Section -> List Section
  -> { upward : Bool, leftward : Bool, downward : Bool, rightward : Bool }
canExpandSection grid section sections =
  let
    sections_ = removeSectionFrom section sections
    canPlace s = canPlaceSectionIn grid s sections_
    maybeCanPlace = Maybe.map canPlace >> Maybe.withDefault False
  in    
    { upward = sectionExpand Upward grid section |> maybeCanPlace
    , leftward = sectionExpand Leftward grid section |> maybeCanPlace
    , downward = sectionExpand Downward grid section |> maybeCanPlace
    , rightward = sectionExpand Rightward grid section |> maybeCanPlace
    }

removeSectionFrom : Section -> List Section -> List Section
removeSectionFrom (Section {row,col}) sections =
  listRemoveWhere (\(Section s) -> s.row == row && s.col == col) sections

gridSectionElements : Grid -> List Section -> List SectionElement
gridSectionElements grid sections =
  let
    accum (row,col) elements =
      let r = {top = row, left = col, bottom = row, right = col}
      in
        listFind (sectionRect >> rectInside r) sections
          |> Maybe.map sectionElement
          |> Maybe.withDefault (placeholderElementAt (row,col)) 
          |> (\el -> el :: elements)
  in
    gridCoords grid
      |> List.foldr accum []

gridCoords : Grid -> List (Int, Int)
gridCoords (Grid {rows,cols}) =
  listCombine (,) 
    (List.range 0 <| List.length rows - 1)
    (List.range 0 <| List.length cols - 1)


sectionRect : Section -> Rect
sectionRect (Section {row,col,rowSpan,colSpan}) =
  { top = row
  , left = col
  , bottom = row + rowSpan - 1
  , right = col + colSpan - 1 
  }

rectSection : Rect -> Section
rectSection {top,left,bottom,right} =
  Section { row = top, col = left, rowSpan = bottom - top + 1, colSpan = right - left + 1 }

gridRect : Grid -> Rect
gridRect (Grid {rows,cols}) =
  { top = 0
  , left = 0
  , bottom = List.length rows - 1
  , right = List.length cols - 1 
  }

rectIntersects : Rect -> Rect -> Bool
rectIntersects a b =
  (a.right >= b.left && a.left <= b.right) && 
  (a.bottom >= b.top && a.top <= b.bottom)

rectInside : Rect -> Rect -> Bool
rectInside a b =
  (a.left >= b.left && a.right <= b.right) && 
  (a.top >= b.top && a.bottom <= b.bottom)

rectIntersection : Rect -> Rect -> Maybe Rect
rectIntersection a b =
  let
    min x y = if x < y then x else y
    max x y = if x > y then x else y
  in 
    if rectIntersects a b then
      Just
        { top = max a.top b.top
        , left = max a.left b.left
        , bottom = min a.bottom b.bottom
        , right = min a.right b.right
        }
    else
      Nothing

sectionIntersects : Section -> Section -> Bool
sectionIntersects a b =
  rectIntersects (sectionRect a) (sectionRect b)

sectionInsideGrid : Section -> Grid -> Bool
sectionInsideGrid section grid = 
  rectInside (sectionRect section) (gridRect grid)

sectionIntersection : Section -> Section -> Maybe Section
sectionIntersection a b =
  rectIntersection (sectionRect a) (sectionRect b)
    |> Maybe.map rectSection

sectionExpand : GridDirection -> Grid -> Section -> Maybe Section
sectionExpand direction grid (Section section) =
  let
    newSection =
      case direction of
        Rightward ->
          Section { section | colSpan = section.colSpan + 1 }
        Downward ->
          Section { section | rowSpan = section.rowSpan + 1 }
        Leftward ->
          Section { section | col = section.col - 1, colSpan = section.colSpan + 1 }
        Upward ->
          Section { section | row = section.row - 1, rowSpan = section.rowSpan + 1 }
  in
    if rectInside (sectionRect newSection) (gridRect grid) then
      Just newSection
    else
      Nothing

sectionShift : GridDirection -> Grid -> Section -> Maybe Section
sectionShift direction grid (Section section) =
  let
    newSection =
      case direction of
        Rightward ->
          Section { section | col = section.col + 1 }
        Downward ->
          Section { section | row = section.row + 1 }
        Leftward ->
          Section { section | col = section.col - 1 }
        Upward ->
          Section { section | row = section.row - 1 }
  in
    if rectInside (sectionRect newSection) (gridRect grid) then
      Just newSection
    else
      Nothing


{-------------------------------------------------------------------------------
  SECTION ELEMENT
-------------------------------------------------------------------------------}

sectionElement : Section -> SectionElement
sectionElement section =
  SectionElement section

placeholderElementAt : (Int, Int) -> SectionElement
placeholderElementAt (row,col) =
  PlaceholderElement (Placeholder { row = row, col = col })

sectionElementToStyles : SectionElement -> List (String, String)
sectionElementToStyles section =
  case section of
    PlaceholderElement placeholder ->
      placeholderToStyles placeholder
    SectionElement section ->
      sectionToStyles section

sectionToStyles : Section -> List (String, String)
sectionToStyles (Section {row,col,rowSpan,colSpan}) =
  let
    stringDim n sz = 
      if sz == 1 then 
        (toString (n+1)) 
      else 
        ((toString (n+1)) ++ " / " ++ "span " ++ (toString sz))
  in
    [ ("grid-row", stringDim row rowSpan)
    , ("grid-column", stringDim col colSpan)
    , ("background-color", "#FFF")
    , ("border", "1px solid #CCC")
    ]

placeholderToStyles : Placeholder -> List (String, String)
placeholderToStyles (Placeholder {row,col}) =
  [ ("grid-row", row + 1 |> toString)
  , ("grid-column", col + 1 |> toString)
  , ("background-color", "inherit")
  , ("border", "1px dashed #CCC")
  ]



-- UTILS

listFind : (a -> Bool) -> List a -> Maybe a
listFind predicate list =
  case list of
    [] -> 
      Nothing
    first :: rest ->
      if predicate first then
        Just first
      else
        listFind predicate rest

listRemoveWhere : (a -> Bool) -> List a -> List a
listRemoveWhere predicate list =
  let
    accum a newlist =
      if predicate a then
        newlist
      else
        a :: newlist
  in
    List.foldr accum [] list


listCombine : (a -> b -> c) -> List a -> List b -> List c
listCombine fn a b =
  List.concatMap (\a_ -> List.map (\b_ -> fn a_ b_) b ) a

