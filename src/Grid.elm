module Grid exposing 
  ( Model, Section, Placeholder, SectionElement(..), StandardUnit(..), GridUnit(..)
  , empty, mapGrid
  , addRow, addCol, removeRow, removeCol, shiftRemoveFirstRow, shiftRemoveFirstCol
  , setRowHeight, setColWidth
  , gridStyles, gridSectionStyles, gridCSS
  , px, rem, fr, minContent, gridUnit
  , gridUnitIsPx, gridUnitIsRem, gridUnitIsFr, gridUnitIsMinContent
  , gridUnitToString, gridUnitValue, gridUnitSetValue, gridUnitRange
  , addSection, removeSection, nameSection, unnameSection, placeSection
  , sectionInRow, sectionInCol, sectionName
  , canExpandSection, expandSectionUpward, expandSectionLeftward
  , expandSectionDownward, expandSectionRightward
  , placeholderRow, placeholderCol, placeholderInRow, placeholderInCol
  , sectionElements
  , rowUnit, colUnit, setRowGap, setColGap, setGap
  )

import Regex exposing (regex)

type Model =
  Model
    { grid : Grid
    , sections : List Section
    }

type Grid =
  Grid
    { rows : List GridUnit
    , cols : List GridUnit
    , rowGap : StandardUnit
    , colGap : StandardUnit
    }

type Section =
  Section
    { row : Int
    , col : Int
    , rowSpan : Int
    , colSpan : Int
    , content : SectionContent
    }

type SectionContent =
  SectionContent
    { name : Maybe String
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
  = StdUnit StandardUnit
  | Fr Float
  | MinContent
  
type StandardUnit
  = Px Float
  | Rem Float


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

shiftRemoveFirstRow : Model -> Model
shiftRemoveFirstRow (Model model) =
  let
    shiftUpIfPossible section =
      sectionShift Upward model.grid section
        |> Maybe.withDefault section
  in
    Model 
      { model 
          | grid = removeFirstGridRow model.grid
          , sections = List.map shiftUpIfPossible model.sections
      }

shiftRemoveFirstCol : Model -> Model
shiftRemoveFirstCol (Model model) =
  let
    shiftLeftIfPossible section =
      sectionShift Leftward model.grid section
        |> Maybe.withDefault section
  in
    Model 
      { model 
          | grid = removeFirstGridCol model.grid
          , sections = List.map shiftLeftIfPossible model.sections
      }

setRowHeight : Int -> GridUnit -> Model -> Model
setRowHeight index unit model =
  mapGrid (setRowUnit index unit) model

setColWidth : Int -> GridUnit -> Model -> Model
setColWidth index unit model =
  mapGrid (setColUnit index unit) model

addSection : Placeholder -> Model -> Model
addSection (Placeholder {row,col}) model =
  placeSection (sectionAt (row,col)) model

nameSection : String -> Section -> Model -> Model
nameSection name section model =
  updateSection (sectionSetName name section) model 

unnameSection : Section -> Model -> Model
unnameSection section model =
  updateSection (sectionClearName section) model

removeSection : Section -> Model -> Model
removeSection section (Model model) =
  Model { model | sections = sectionRemoveFrom section model.sections }

updateSection : Section -> Model -> Model
updateSection section (Model model) =
  Model { model | sections = sectionUpdateIn section model.sections }

placeSection : Section -> Model -> Model
placeSection section (Model model) =
  Model { model | sections = sectionPlaceIn model.grid section model.sections }

canExpandSection : Section -> Model
  -> { upward : Bool, leftward : Bool, downward : Bool, rightward : Bool }
canExpandSection section (Model {grid,sections}) =
  sectionCanExpand grid section sections

expandSection : GridDirection -> Section -> Model -> Model
expandSection direction section (Model model) =
  let
    model_ = removeSection section (Model model)
  in
    sectionExpand direction model.grid section 
      |> Maybe.map (\s -> placeSection s model_)
      |> Maybe.withDefault model_

expandSectionUpward = expandSection Upward
expandSectionLeftward = expandSection Leftward
expandSectionDownward = expandSection Downward
expandSectionRightward = expandSection Rightward


rowUnit : Int -> Model -> Maybe GridUnit
rowUnit index (Model {grid}) =
  gridRowUnit index grid

colUnit : Int -> Model -> Maybe GridUnit
colUnit index (Model {grid}) =
  gridColUnit index grid

sectionElements : Model -> List SectionElement
sectionElements (Model {grid,sections}) =
  gridSectionElements grid sections


gridStyles : Model -> List (String,String)
gridStyles (Model {grid}) =
  gridToStyles grid

gridSectionStyles = sectionElementToStyles


gridCSS : String -> Model -> String
gridCSS selector (Model {grid,sections}) =
  let
    sectionClass i s =
      sectionName s
        |> Maybe.map slugify
        |> Maybe.withDefault ("section-" ++ (toString i))
      
    toCSS i s =
      sectionElement s
        |> sectionElementToCSS ("#" ++ sectionClass i s)

    sectionCSS =
      sections |> List.reverse |> List.indexedMap toCSS
  in
    String.join "\n\n"
      ( (gridToCSS selector grid) :: sectionCSS )


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

gridRowUnit : Int -> Grid -> Maybe GridUnit
gridRowUnit index (Grid {rows}) =
  listGetAt ((List.length rows) - index - 1) rows

gridColUnit : Int -> Grid -> Maybe GridUnit
gridColUnit index (Grid {cols}) =
  listGetAt ((List.length cols) - index - 1) cols

setRowGap : StandardUnit -> Grid -> Grid
setRowGap unit (Grid grid) =
  Grid { grid | rowGap = unit }

setColGap : StandardUnit -> Grid -> Grid
setColGap unit (Grid grid) =
  Grid { grid | colGap = unit }

setGap : StandardUnit -> Grid -> Grid
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

removeFirstGridRow : Grid -> Grid
removeFirstGridRow (Grid grid) =
  Grid { grid | rows = List.take ((List.length grid.rows) - 1) grid.rows }

removeFirstGridCol : Grid -> Grid
removeFirstGridCol (Grid grid) =
  Grid { grid | cols = List.take ((List.length grid.cols) - 1) grid.cols }

setRowUnit : Int -> GridUnit -> Grid -> Grid
setRowUnit index unit (Grid grid) =
  let
    index_ = (List.length grid.rows - index - 1)
  in
    Grid { grid | rows = listSetAt index_ unit grid.rows |> Maybe.withDefault grid.rows }

setColUnit : Int -> GridUnit -> Grid -> Grid
setColUnit index unit (Grid grid) =
  let
    index_ = (List.length grid.cols - index - 1)
  in
    Grid { grid | cols = listSetAt index_ unit grid.cols |> Maybe.withDefault grid.cols }

gridToStyles : Grid -> List (String, String)
gridToStyles (Grid {rows, cols, colGap, rowGap}) =
  [ ("display", "grid")
  , ("grid-row-gap", standardUnitToString rowGap)
  , ("grid-column-gap", standardUnitToString colGap)
  ] ++
  ( List.reverse rows 
      |> gridUnitsToString 
      |> Maybe.map (\rs -> [("grid-template-rows",rs)]) 
      |> Maybe.withDefault []
  ) ++
  ( List.reverse cols
      |> gridUnitsToString 
      |> Maybe.map (\cs -> [("grid-template-columns",cs)]) 
      |> Maybe.withDefault []
  )

gridUnitsToString : List GridUnit -> Maybe String
gridUnitsToString units =
  case units of
    [] ->
      Nothing
    _ ->
      List.map gridUnitToString units |> String.join " " |> Just

gridToCSS : String -> Grid -> String
gridToCSS selector grid =
  gridToStyles grid |> stylesToCSS selector


{-------------------------------------------------------------------------------
  GRID UNIT
-------------------------------------------------------------------------------}

px : Float -> StandardUnit
px i = Px i

rem : Float -> StandardUnit
rem i = Rem i

fr : Float -> GridUnit
fr i = Fr i

minContent : GridUnit
minContent = MinContent

gridUnit : StandardUnit -> GridUnit
gridUnit u = StdUnit u

gridUnitIsPx : GridUnit -> Bool
gridUnitIsPx unit =
  case unit of
    StdUnit u -> standardUnitIsPx u
    _ -> False

gridUnitIsRem : GridUnit -> Bool
gridUnitIsRem unit =
  case unit of
    StdUnit u -> standardUnitIsRem u
    _ -> False

gridUnitIsFr : GridUnit -> Bool
gridUnitIsFr unit =
  case unit of
    Fr i -> True
    _ -> False

gridUnitIsMinContent : GridUnit -> Bool
gridUnitIsMinContent unit =
  case unit of
    MinContent -> True
    _ -> False

standardUnitIsPx : StandardUnit -> Bool
standardUnitIsPx unit =
  case unit of
    Px _ -> True
    _ -> False

standardUnitIsRem : StandardUnit -> Bool
standardUnitIsRem unit =
  case unit of
    Rem _ -> True
    _ -> False



gridUnitValue : GridUnit -> Maybe Float
gridUnitValue unit =
  case unit of
    StdUnit u -> standardUnitValue u
    Fr i -> Just i
    MinContent -> Nothing

standardUnitValue : StandardUnit -> Maybe Float
standardUnitValue unit =
  case unit of
    Px i -> Just i
    Rem i -> Just i

gridUnitSetValue : Float -> GridUnit -> GridUnit
gridUnitSetValue value unit =
  case unit of
    StdUnit u -> StdUnit <| standardUnitSetValue value u
    Fr i -> Fr value
    MinContent -> MinContent

standardUnitSetValue : Float -> StandardUnit -> StandardUnit
standardUnitSetValue value unit =
  case unit of
    Px i -> Px value
    Rem i -> Rem value


gridUnitRange : GridUnit -> Maybe {min: Float, max: Float, step: Float}
gridUnitRange unit =
  case unit of
    StdUnit u -> standardUnitRange u
    Fr _ -> Just { min = 1, max = 16, step = 1 }
    MinContent -> Nothing

standardUnitRange : StandardUnit -> Maybe {min: Float, max: Float, step: Float}
standardUnitRange unit =
  case unit of
    Px _ -> Just { min = 10, max = 1000, step = 1 }
    Rem _ -> Just { min = 1, max = 10, step = 0.1 }


gridUnitToString : GridUnit -> String
gridUnitToString unit =
  case unit of
    StdUnit u -> standardUnitToString u
    Fr i -> (toString i) ++ "fr"
    MinContent -> "min-content"

standardUnitToString : StandardUnit -> String
standardUnitToString unit =
  case unit of
    Px i -> (toString i) ++ "px"
    Rem i -> (toString i) ++ "rem"



{-------------------------------------------------------------------------------
  SECTION
-------------------------------------------------------------------------------}

sectionAt : (Int, Int) -> Section
sectionAt (row, col) =
  rectSection {top = row, left = col, bottom = row, right = col}

sectionSetName : String -> Section -> Section
sectionSetName name (Section section) =
  Section { section | content = sectionContentSetName name section.content }

sectionClearName : Section -> Section
sectionClearName (Section section) =
  Section { section | content = sectionContentClearName section.content }

sectionInRow : Int -> Section -> Bool
sectionInRow r (Section {row,rowSpan}) =
  r >= row && r < (row + rowSpan)

sectionInCol : Int -> Section -> Bool
sectionInCol c (Section {col,colSpan}) =
  c >= col && c < (col + colSpan)

sectionName : Section -> Maybe String
sectionName (Section {content}) =
  sectionContentName content

sectionCanBePlacedIn : Grid -> Section -> List Section -> Bool
sectionCanBePlacedIn grid section sections =
  (List.all (not << sectionIntersects section) sections) &&
  (sectionInsideGrid section grid)

sectionPlaceIn : Grid -> Section -> List Section -> List Section
sectionPlaceIn grid section sections =
  if sectionCanBePlacedIn grid section sections then
    section :: sections
  else
    sections

sectionCanExpand : Grid -> Section -> List Section
  -> { upward : Bool, leftward : Bool, downward : Bool, rightward : Bool }
sectionCanExpand grid section sections =
  let
    sections_ = sectionRemoveFrom section sections
    canPlace s = sectionCanBePlacedIn grid s sections_
    maybeCanPlace = Maybe.map canPlace >> Maybe.withDefault False
  in    
    { upward = sectionExpand Upward grid section |> maybeCanPlace
    , leftward = sectionExpand Leftward grid section |> maybeCanPlace
    , downward = sectionExpand Downward grid section |> maybeCanPlace
    , rightward = sectionExpand Rightward grid section |> maybeCanPlace
    }

sectionRemoveFrom : Section -> List Section -> List Section
sectionRemoveFrom (Section {row,col}) sections =
  listRemoveWhere (\(Section s) -> s.row == row && s.col == col) sections

sectionUpdateIn : Section -> List Section -> List Section
sectionUpdateIn (Section section) sections =
  listUpdateWhere 
    (\(Section s) -> s.row == section.row && s.col == section.col) 
    (Section section) sections 

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
  Section 
    { row = top
    , col = left
    , rowSpan = bottom - top + 1
    , colSpan = right - left + 1 
    , content = emptySectionContent
    }

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
  SECTION CONTENT
-------------------------------------------------------------------------------}

emptySectionContent : SectionContent
emptySectionContent =
  SectionContent { name = Nothing }

sectionContentSetName : String -> SectionContent -> SectionContent
sectionContentSetName name (SectionContent content) =
  SectionContent { content | name = Just name }

sectionContentClearName : SectionContent -> SectionContent
sectionContentClearName (SectionContent content) =
  SectionContent { content | name = Nothing }

sectionContentName : SectionContent -> Maybe String
sectionContentName (SectionContent {name}) =
  name

{-------------------------------------------------------------------------------
  PLACEHOLDER
-------------------------------------------------------------------------------}

placeholderRowCol : Placeholder -> (Int,Int)
placeholderRowCol (Placeholder {row,col}) =
  (row, col)

placeholderRow = placeholderRowCol >> Tuple.first
placeholderCol = placeholderRowCol >> Tuple.second

placeholderInRow : Int -> Placeholder -> Bool
placeholderInRow r (Placeholder {row}) =
  r == row

placeholderInCol : Int -> Placeholder -> Bool
placeholderInCol c (Placeholder {col}) =
  c == col


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
    ]

placeholderToStyles : Placeholder -> List (String, String)
placeholderToStyles (Placeholder {row,col}) =
  [ ("grid-row", row + 1 |> toString)
  , ("grid-column", col + 1 |> toString)
  ]


sectionElementToCSS : String -> SectionElement -> String
sectionElementToCSS selector section =
  sectionElementToStyles section |> stylesToCSS selector


-- UTILS

listGetAt : Int -> List a -> Maybe a
listGetAt index list =
  if index < 0 then
    Nothing
  else
    List.head <| List.drop index list

listSetAt : Int -> a -> List a -> Maybe (List a)
listSetAt index a list =
  if index < 0 then
    Nothing
  else
    let
      (head, tail) = (List.take index list, List.drop index list |> List.tail)
    in
      Maybe.map (\t -> a :: t |> List.append head) tail

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

listUpdateWhere : (a -> Bool) -> a -> List a -> List a
listUpdateWhere predicate replace list =
  let
    accum a newlist =
      if predicate a then
        replace :: newlist
      else
        a :: newlist
  in
    List.foldr accum [] list


listCombine : (a -> b -> c) -> List a -> List b -> List c
listCombine fn a b =
  List.concatMap (\a_ -> List.map (\b_ -> fn a_ b_) b ) a



stylesToCSS : String -> List (String, String) -> String
stylesToCSS selector styles =
  let
    styleLines = 
      styles
        |> List.map (\(k,v) -> "  " ++ k ++ ": " ++ v ++ ";")
  in
    String.join "\n"
        [ selector ++ " {"
        , String.join "\n" styleLines
        , "}"
        ]

slugify : String -> String
slugify s =
  String.toLower s
    |> Regex.replace Regex.All (regex "[^A-Za-z\\-]") (always "-")

