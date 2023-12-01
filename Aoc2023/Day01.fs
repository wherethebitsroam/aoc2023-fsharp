module Aoc2023.Day01

open System

let parseLine (l: string) =
    let digits = l |> Seq.filter Char.IsDigit
    let num = [Seq.head digits; Seq.last digits]
    num |> String.Concat |> Int32.Parse

let part1 (s: string) =
    s.Trim().Split '\n'
    |> Seq.map parseLine
    |> Seq.sum
    
let replaceWords (s: string) =
    s.Replace("one", "o1e")
        .Replace("two", "t2o")
        .Replace("three", "t3e")
        .Replace("four", "4")
        .Replace("five", "5e")
        .Replace("six", "6")
        .Replace("seven", "7n")
        .Replace("eight", "e8t")
        .Replace("nine", "n9e")
    
let part2 (s: string) =
    s.Trim().Split '\n'
    |> Seq.map replaceWords
    |> Seq.map parseLine
    |> Seq.sum
    

