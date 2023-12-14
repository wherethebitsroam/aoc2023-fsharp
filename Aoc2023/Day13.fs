module Aoc2023.Day13

open FParsec

module Parser =
    let row = many1 (anyOf ".#") .>> newline
    let parser = many1 (many1 row .>> spaces)

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let test = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"

let diff (r1: char list) (r2: char list) =
    List.zip r1 r2 |> List.sumBy (fun (c1,c2) -> if c1 = c2 then 0 else 1)
    
let findReflectionIndex (expectedDiff: int) (rows: char list list): int option =
    let rows = rows |> Array.ofList
    let len = Array.length rows
    seq {1..(len-1)}
    |> Seq.tryFind (fun i ->
        let width = min i (len-i)
        let diffs = seq { 0..(width-1) } |> Seq.sumBy (fun j -> diff rows[i-j-1] rows[i+j])
        diffs = expectedDiff
        )
    
let findReflection (expectedDiff: int) (pattern: char list list) =
    // first try horizontal
    match pattern |> findReflectionIndex expectedDiff with
    | Some x -> x * 100
    | None ->
        // try vertical
        match pattern |>  List.transpose |> findReflectionIndex expectedDiff with
        | Some x -> x
        | None -> failwith "wtf"

let part1 (s: string) =
    s
    |> Parser.parse
    |> List.map (findReflection 0)
    |> List.sum
    
let part2 (s: string) =
    s
    |> Parser.parse
    |> List.map (findReflection 1)
    |> List.sum

