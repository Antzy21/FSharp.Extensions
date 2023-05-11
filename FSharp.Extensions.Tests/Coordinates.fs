namespace Coordinates

open Xunit
open FSharp.Extensions
    
module getShiftBetweenCoordinates =
    
    [<Fact>]
    let ``(2,2) & (1,3) => (-1,1)`` () =
        let result = Coordinates.getShiftBetweenCoordinates (2,2) (1,3)
        Assert.Equal(struct (-1,1), result)

module getDistanceBetweenCoordinates =
    
    [<Fact>]
    let ``(2,2) & (1,3) => 2`` () =
        let result = Coordinates.getDistanceBetweenCoordinates (2,2) (1,3)
        Assert.Equal(2, result)

module getAfterShift =
    
    [<Fact>]
    let ``(2,2) + (1,3) => (3,5)`` () =
        let result = Coordinates.getAfterShift (2,2) (1,3)
        Assert.Equal(struct (3,5), result)

module afterRepeatedShift = 

    [<Fact>]
    let ``Stopper is inclusive`` () =
        let stopper coords = coords = struct (3,3)
        let result = Coordinates.afterRepeatedShift stopper (1,1) (0,0)
        Assert.Equal<coordinates<int> list>([struct (1,1); struct (2,2); struct (3,3)], result)

module getAfterShiftInAllDirections =
    
    [<Fact>]
    let ``Gets correct set of coordinates`` () =
        let result =
            Coordinates.getAfterShiftInAllDirections (0,0) (1,1)
            |> Seq.toList
        Assert.Equal<coordinates<int> list>([struct (1,1); struct (1,-1); struct (-1,1); struct (-1,-1)], result)

