# Grid-Elm

An experimental elm frontend for designing css-grid layouts 


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


