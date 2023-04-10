namespace Array2D

open Xunit
open FSharp.Extensions

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
    let ``foldji`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldji folder "" ary
        Assert.Equal("1 3 2 4 ", result)

    [<Fact>]
    let ``foldjbacki`` () =
        let ary = array2D [[1;2];[3;4]]
        let result = Array2D.foldjbacki folder "" ary
        Assert.Equal("3 1 4 2 ", result)
