namespace FSharp.Extensions

/// A tuple of two ints, used for referencing a 2D discrete grid.
type coordinates = int * int

///<summary>Contains operations for working with values of type <see cref="T:FSharp.Extensions.coordinates" />.</summary>
module Coordinates =
    
    /// Get the metropolitan distance between two given coordinates
    let getDistanceBetweenCoordinates ((x1,y1): coordinates) ((x2,y2): coordinates) : coordinates =
        (x2-x1,y2-y1)

    /// Get new coordinates after shifting given coordinates by a given shift.
    /// Starting coordinates and shift parameters are interchangable thanks to the communitivity property of addition!
    let getAfterShift ((i, j): int * int) ((x, y): coordinates) : coordinates =
        (x+i, y+j)
    
    /// Get a sequence of coordinates after applying shifts to given coordinates.
    let getAfterShifts (start: coordinates) (shifts: (int * int) seq) : coordinates seq =
        Seq.map (getAfterShift start) shifts
    
    /// Get up to 8 unique coordinates that are a given metropolitan distance away from given coordinates
    let getAfterShiftInAllDirections (start: coordinates) ((i, j) : int * int) : coordinates seq =
        [
            (i,j);
            (i,-j);
            (-i,j);
            (-i,-j)
            (j,i);
            (j,-i);
            (-j,i);
            (-j,-i)
        ]
        |> Seq.distinct
        |> getAfterShifts start
    
    /// Get 4 coordinates that are orthogonal (share a side with) some given coordinates
    let getOrthogonalAdjacentCoordinates (start: coordinates) : coordinates seq =
        [
            (0,1);
            (1,0);
            (0,-1);
            (-1,0)
        ]
        |> getAfterShifts start

    /// Get 4 coordinates at diagonals to some given coordinates
    let getDiagonallyAdjacentCoordinates (start: coordinates) : coordinates seq =
        [
            (1,1);
            (1,-1);
            (-1,-1);
            (-1,1)
        ]
        |> getAfterShifts start

    /// Get 8 coordinates up, down, left, right and at diaganols to some given coordinates
    let getAdjacentCoordinates (start: coordinates) : coordinates seq =
        getDiagonallyAdjacentCoordinates start 
        |> Seq.append (getOrthogonalAdjacentCoordinates start)        