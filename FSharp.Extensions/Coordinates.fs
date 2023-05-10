namespace FSharp.Extensions

open System.Numerics

/// A tuple of two INumbers, used for referencing a 2D discrete grid.
type coordinates<'Size when 'Size :> INumber<'Size>> = (struct ('Size * 'Size))

///<summary>Contains operations for working with values of type <see cref="T:FSharp.Extensions.coordinates" />.</summary>
module Coordinates =
    
    /// Get the shift between two given coordinates
    let getShiftBetweenCoordinates ((x1,y1): coordinates<'Size>) ((x2,y2): coordinates<'Size>) : coordinates<'Size> =
        (x2-x1,y2-y1)

    /// Get the metropolitan distance between two given coordinates
    let getDistanceBetweenCoordinates ((x1,y1): coordinates<'Size>) ((x2,y2): coordinates<'Size>) : 'Size =
        getShiftBetweenCoordinates (x1,y1) (x2, y2)
        |> (fun struct (x,y) -> 'Size.Abs(x) + 'Size.Abs(y))

    /// Get new coordinates after shifting given coordinates by a given shift.
    /// Starting coordinates and shift parameters are interchangable thanks to the communitivity property of addition!
    let getAfterShift ((i, j): struct ('Size * 'Size)) ((x, y): coordinates<'Size>) : coordinates<'Size> =
        struct (x+i, y+j)
    
    /// Get a sequence of coordinates after applying shifts to given coordinates.
    let getAfterShifts (start: coordinates<'Size>) (shifts: (struct ('Size * 'Size)) seq) : coordinates<'Size>seq =
        Seq.map (getAfterShift start) shifts
    
    /// Get up to 8 unique coordinates that are a given metropolitan distance away from given coordinates
    let getAfterShiftInAllDirections (start: coordinates<'Size>) ((i, j) : struct ('Size * 'Size)) : coordinates<'Size>seq =
        [
            struct (i,j);
            struct (i,-j);
            struct (-i,j);
            struct (-i,-j)
            struct (j,i);
            struct (j,-i);
            struct (-j,i);
            struct (-j,-i)
        ]
        |> Seq.distinct
        |> getAfterShifts start
    
    /// Get 4 coordinates that are orthogonal (share a side with) some given coordinates
    let getOrthogonalAdjacentCoordinates (start: coordinates<'Size>) : coordinates<'Size>seq =
        [
            struct ('Size.Zero,'Size.One);
            struct ('Size.One,'Size.Zero);
            struct ('Size.Zero,-'Size.One);
            struct (-'Size.One,'Size.Zero)
        ]
        |> getAfterShifts start

    /// Get 4 coordinates at diagonals to some given coordinates
    let getDiagonallyAdjacentCoordinates (start: coordinates<'Size>) : coordinates<'Size>seq =
        [
            struct ('Size.One,'Size.One);
            struct ('Size.One,-'Size.One);
            struct (-'Size.One,-'Size.One);
            struct (-'Size.One,'Size.One)
        ]
        |> getAfterShifts start

    /// Get 8 coordinates up, down, left, right and at diaganols to some given coordinates
    let getAdjacentCoordinates (start: coordinates<'Size>) : coordinates<'Size>seq =
        getDiagonallyAdjacentCoordinates start 
        |> Seq.append (getOrthogonalAdjacentCoordinates start)
        
    /// Get all coordinates after repeating a shift, up to and including when a stopping function criteria is met.
    let rec afterRepeatedShift (stopAt: coordinates<'Size> -> bool) (shift: struct ('Size * 'Size)) (start: coordinates<'Size>) : coordinates<'Size>list =
        let newCoordinates = getAfterShift shift start
        if stopAt newCoordinates then
            [newCoordinates]
        else
            newCoordinates :: afterRepeatedShift stopAt shift newCoordinates
            