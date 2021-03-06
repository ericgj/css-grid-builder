# Grid-Elm

An experimental elm frontend for designing css-grid layouts 

_14 Apr 2017_

Ok, a simpler approach. Rectangle intersection is not really hard to do, and is
cheap. The keys here are:

  - Calling context manages sections

  - Placeholders are derived looking at every coordinate of the grid, does it
    fall within any of the sections. It sounds expensive (n^4), but of course 
    at the grid dimensions we are talking about it makes no difference.



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


    sectionRect : Section -> Rect
    sectionRect (Section {row,col,rowSpan,colSpan}) =
      { top : row
      , left : col
      , bottom : row + rowSpan - 1
      , right : col + colSpan - 1 
      )

    rectSection : Rect -> Section
    rectSection {top,left,bottom,right} =
      Section { row = top, col = left, rowSpan = bottom - top, colSpan = right - left }

    gridRect : Grid -> Rect
    gridRect (Grid {rows,cols}) =
      ( top : 0
      , left : 0
      , bottom : List.length rows - 1
      , right : List.length cols - 1 
      )

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
            { top : max a.top b.top
            , left : max a.left b.left
            , bottom : min a.bottom b.bottom
            , right : min a.right b.right
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

    canPlaceSection : Grid -> Section -> List Section -> Bool
    canPlaceSection grid section sections =
      (List.all (not << sectionIntersects section) sections) &&
      (sectionInsideGrid section grid)

    placeSection : Grid -> Section -> List Section -> List Section
    placeSection grid section sections =
      if canPlaceSection grid section sections then
        section :: sections
      else
        sections

    gridCoords : Grid -> List (Int, Int)
    gridCoords (Grid {rows,cols}) =
      listCombine (,) 
        (List.range 0 <| List.length rows - 1)
        (List.range 0 <| List.length cols - 1)

    gridSectionElements : Grid -> List Section -> List SectionElements
    gridSectionElements grid sections =
      let
        accum (row,col) elements =
          let r = {top = row, left = col, bottom = row, right = col}
          in
            if List.any (sectionRect >> rectInside r) sections then
              elements
            else
              Placeholder { row = row, col = col } :: elements
      in
        gridCoords grid
          |> List.foldr accum []


    

_12 Apr 2017_

## Spanning: the paint by numbers approach

Another approach is not to store sections themselves, but store _grid_ state,
which then sections can be derived from.

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

    -- Note: derived below
    type PositionedSection
      = PositionedSection GridCoord GridSpan Section
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
    

    placeholderSectionAt : Int -> Int -> PositionedSection
    placeholderSectionAt row col =
      Placeholder { row = row, col = col }

    positionedSectionAt : Int -> Int -> Section -> PositionedSection
    positionedSectionAt row col section =
      PositionedSection 
        { row = row, col = col } 
        { rowSpan = 1, colSpan = 1 }
        section

    foldGridContents : ((Int, Int) -> Maybe Int -> a -> a) -> a -> Grid -> a
    foldGridContents accum initial (Grid {contents}) =
      Dict.foldr accum initial contents

    gridSections = Model -> List PositionedSection
    gridSections (Model {grid,sections}) =
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
               Array.get i sections
                 |> Maybe.map (positionedSectionAt row col)
                 |> Maybe.withDefault (placeholderSectionAt row col)

             PositionedSection coord span section ->
               let
                 (newCoord, newSpan) = expandDims {row = row, col = col} coord span
               in
                 PositionedSection newCoord newSpan section

        accum (row,col) mindex (psections, placeholders) =
          let
            newPlaceholder = placeholderSectionAt row col 
          in 
            case mindex of
              Nothing ->
                ( psections, newPlaceholder :: placeholders )
              Just i ->
                Dict.get i psections
                  |> Maybe.withDefault newPlaceholder
                  |> updatePositionedSection row col i
                  |> (\p -> Dict.insert i p psections)
            
        
      in
        foldGridContents accum (Dict.empty, []) grid
          |> (\(psections, placeholders) -> Dict.values psections ++ placeholders) 
          


_11 Apr 2017_

## Spanning

OK, so CSS-Grid 'underlaps' sections by default. Spanned sections (defined 
later in css?) appear underneath sections pinned to or spanning the same 
grid location.

Now, it gets quite hairy to deal with intersections in two dimensions. You
sometimes get shapes that cannot be represented by a single css element. Like:

  -------------------------------------------------------------------
  |                                             |                   |
  |                                             |                   |
  |   B                                         |    A              |
  |                                             |                   |
  |                                             |                   |
  |                                             |                   |
  |                                             |                   |
  -----------------------------------------------                   |
                           |                                        |
                           |                                        |
                           |                                        |
                           |                                        |
                           |                                        |
                           |                                        |
                           |                                        |
                           ------------------------------------------

**B** spans into **A**. Before that, **A** is 
`grid-area : 1 / 2 / span 2 / span 2`.  What the heck is it afterwards?

This is my proposal for how to deal with this. We have a concept of 'unnamed'
sections. These are 'placeholders' which other, _named_ sections can span into.
And the crucial part is: they themselves are single-span. Thus they are easy
to remove in case of intersection. 

In fact, 'placeholder' is a better name. This should also be built into the
type.


_10 Apr 2017_

Not really sure what the UI should look like, but given the odd way css-grid
works, the easiest thing seems to be (a) set up the specs of your columns and
rows first, then (b) "render sections" into that layout.

"Render sections" might might mean 

  - specifying n sections evenly into the layout and then messing with the 
`grid-column-span` and `grid-row-span`'s, or 

  - specifying named sections first, and then matching them up to grid cells

Or some combination of these, or something else.

****

CSS-Grid lets you do all kinds of nonsensical things, so the first thing is 
really to make some decisions about how to constrain the users' choices, which
helps shape what the UI looks like.

In terms of _website layout_, I am following along this [walkthrough by 
Eric Meyer](https://alistapart.com/article/practical-grid). So he starts with
the _rows_ (which seems a bit counterintuitive to me, but whatever), and then
does the _columns_. Presumably you should be able to them in either order.

I like the instant feedback. When you specify a new row, it drops in the
new row div, and likewise with the columns. The tricky part is what to do when
you have rows already and you add columns, or vice-versa. As you add the divs,
do you pin them to column and row? And then go through and fill in divs for
col/row combinations that aren't covered by other divs?  And what about multi-
span rows and cols? There is probably an algorithm and data structure for this
kind of shit...

    type alias Grid =
      { rows : List GridUnit
      , cols : List GridUnit
      }

    type alias GridSections =
      Dict (Int, Int) Section

    type Section = Unnamed | Named String

    fillGridSections : Grid -> GridSections -> GridSections
    fillGridSections grid gs =
      let accum (row,col) gs_ =
        Dict.get (row,col) gs_
          |> Maybe.map (always gs_)
          |> Maybe.withDefault (Dict.set (row,col) Unnamed gs_)
      in
        List.map2 (,) 
          (List.range 0 (List.length grid.rows)) 
          (List.range 0 (List.length grid.cols))
            |> List.foldr accum gs 

All fine and dandy, but that structure makes it hard to impossible to derive
the spans for each of the sections.


    type alias Grid =
      { rows : List GridUnit
      , cols : List GridUnit
      }

    type alias Section =
      { row : Int
      , col : Int
      , rowSpan : Int
      , colSpan : Int
      }

    hasSectionAt : Int -> Int -> List Section -> Bool
    hasSectionAt row col sections =
      let 
        intersecting section =
          List.member row (List.range section.row (section.row + section.rowSpan + 1)) and 
          List.member col (List.range section.col (section.col + section.colSpan + 1)) 
      in
        List.any intersecting sections

    addMissingSections : Grid -> List Section -> List Section
    addMissingSections grid sections =
      let 
        gridCoords =
          List.map2 (,)
              (List.range 0 (List.length grid.rows)) 
              (List.range 0 (List.length grid.cols))

        missingSectionAt (row,col) =
          not (hasSectionAt row col sections)
      
        addSection (row,col) sections_ =
          sections_ ++ [ { row = row, col = col, rowSpan = 1, colSpan = 1 } ]
     in
       List.map missingSectionAt gridCoords
         |> List.foldr addSection sections


Although it occurs to me: when adding a row, there is no danger of overlap with
existing sections; and likewise when adding a column. Adding a row means pinning
a section to each column in that row. Adding a column means pinning a section
to each row in that column. 

Question: what about removing a row/column? Most of the time I doubt you want
wrapping spanned sections, you want to crop them. Or remove them altogether if
they are pinned beyond the cropped region.

    cropRightOrRemove : Int -> Section -> Maybe Section
    cropRightOrRemove cols section =
      if section.cols >= cols then
        Nothing
      else
        if (section.cols + section.colSpan) >= cols then
          Just { section | colSpan = cols - section.cols }
        else
          Just section


The other tricky part is spanning, AKA 'merging'. I am not sure what CSS-Grid
does with overlapping sections. I kind of think I want to try it before getting
into the mess of inter-section cropping or whatever.


