module Aoc2023.Day03

open System

type Span = {Start: int; End: int}

type Parsed = {
    RowLength: int
    Numbers: Span list
    Symbols: Set<int>
    Stars: int list
}

let adjacent (rowLength: int) (s: Span) =
    seq {
       for i in (s.Start-1) .. s.End do
           yield i
           yield i - rowLength
           yield i+rowLength
    }
    |> Set.ofSeq

let parse (s: string) =
    let rowLength = s.IndexOf '\n' + 1
    
    let mutable numbers = List.empty
    let mutable symbols = Set.empty
    let mutable stars = List.empty
    
    let mutable pos = 0
    
    while pos < s.Length do
        match s[pos] with
        | d when Char.IsDigit d ->
            let start = pos
            pos <- pos + 1
            while Char.IsDigit s[pos] do
                pos <- pos + 1
            numbers <- { Start = start; End = pos } :: numbers
        | '.' | '\n' ->
            pos <- pos + 1
        | x ->
            if x = '*' then
                stars <- pos :: stars
            symbols <- Set.add pos symbols
            pos <- pos + 1
            
    { RowLength = rowLength; Numbers = numbers; Symbols = symbols; Stars = stars }
    
let numberToInt (s: string) (span: Span)  =
    s.Substring(span.Start, span.End - span.Start) |> Int32.Parse
                
let part1 (s: string) =
    let x = parse (s.Trim())
    x.Numbers
    |> List.filter (fun n -> adjacent x.RowLength n |> Set.intersect x.Symbols |> Set.isEmpty |> not)
    |> List.map (numberToInt s)
    |> List.sum

let part2 (s: string) =
    let x = parse (s.Trim())
    // for each star:
    // - get the numbers adjacent to the star
    // - filter to only results with 2 numbers
    // - map the numbers to ints and multiply
    // - sum
    x.Stars
    |> List.map (fun star -> x.Numbers |> List.filter (fun num -> num |> adjacent x.RowLength |> Set.contains star))
    |> List.filter (fun l -> List.length l = 2)
    |> List.map (fun l -> l |> List.map (numberToInt s) |> List.reduce (fun acc elem -> acc * elem) )
    |> List.sum
