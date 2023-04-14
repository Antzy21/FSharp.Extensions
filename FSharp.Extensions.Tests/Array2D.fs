namespace Array2D

open Xunit
open FSharp.Extensions
    
module ContainsCoordinates =
    
    [<Fact>]
    let ``(1,1) coords are in array`` () =
        let ary = array2D [[1;2];[3;4]]
        Array2D.containsCoordinates (1,1) ary
        |> Assert.True
        
    [<Fact>]
    let ``(0,0) coords are in array`` () =
        let ary = array2D [[1;2];[3;4]]
        Array2D.containsCoordinates (0,0) ary
        |> Assert.True
        
    [<Fact>]
    let ``(2,1) coords are not in array`` () =
        let ary = array2D [[1;2];[3;4]]
        Array2D.containsCoordinates (2,1) ary
        |> Assert.False
    
    [<Fact>]
    let ``(1,2) coords are not in array`` () =
        let ary = array2D [[1;2];[3;4]]
        Array2D.containsCoordinates (1,2) ary
        |> Assert.False

module Fold =

    let folder (coords: coordinates<int>) (s: string) (i: int) : string =
        s + $"{i} "

    [<Fact>]
    let ``foldij`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldij folder "" ary
        Assert.Equal("1 2 3 4 ", result)

    [<Fact>]
    let ``foldibackj`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldibackj folder "" ary
        Assert.Equal("2 1 4 3 ", result)
        
    [<Fact>]
    let ``foldijback`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldijback folder "" ary
        Assert.Equal("3 4 1 2 ", result)
        
    [<Fact>]
    let ``foldibackjback`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldibackjback folder "" ary
        Assert.Equal("4 3 2 1 ", result)
        
    [<Fact>]
    let ``foldji`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldji folder "" ary
        Assert.Equal("1 3 2 4 ", result)

    [<Fact>]
    let ``foldjbacki`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldjbacki folder "" ary
        Assert.Equal("3 1 4 2 ", result)
    
    [<Fact>]
    let ``foldjiback`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldjiback folder "" ary
        Assert.Equal("2 4 1 3 ", result)
    
    [<Fact>]
    let ``foldjbackiback`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldjbackiback folder "" ary
        Assert.Equal("4 2 3 1 ", result)

module AtCoordinates =
    
    [<Fact>]
    let ``Value at (0,0) is 1`` () =
        let ary = array2D [[1;2];[3;4]]
        Assert.Equal(1, ary.[0,0])
    
    [<Fact>]
    let ``Value at (0,1) is 2`` () =
        let ary = array2D [[1;2];[3;4]]
        Assert.Equal(2, ary.[0,1])