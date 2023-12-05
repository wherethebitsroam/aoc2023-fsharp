module Tests

open Xunit
open Aoc2023.Day05

[<Fact>]
let ``Day5`` () =
    let m1 = { Src = 10; Dst = 0; Length = 6 }
    let m2 = { Src = 13; Dst = 0; Length = 6 }
    let overlap = Mapping.overlap m1 m2
    Assert.Equal(Some(13,15), overlap)
    
