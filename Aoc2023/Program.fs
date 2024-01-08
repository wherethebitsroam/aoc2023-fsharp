// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open Aoc2023

// open System.Text
// for i = 1 to 25 do
//     let path = $"Day%02d{i}.fs"
//     let content = $"module Aoc2023.Day%02d{i}
//
// let part1 (s: string) =
//     printfn \"Aoc2023.Day%02d{i}.part1\"
//
// let part2 (s: string) =
//     printfn \"Aoc2023.Day%02d{i}.part2\"
//
// "
//     File.WriteAllText(path, content, Encoding.UTF8)
//     printfn $"<Compile Include=\"%s{path}\" />"
    
let data = File.ReadAllText "../../day20.txt"
// data |> Day20.part1 |> printfn "%A"
data |> Day20.part2 |> printfn "%A"
