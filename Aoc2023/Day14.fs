module Aoc2023.Day14

open FParsec

module Parser =
    let row = many1 (anyOf ".#O") .>> newline
    let parser = many1 row .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result |> List.map Array.ofList |> Array.ofList
        | Failure (error,_,_) -> failwith error

let test = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"

let weight (rocks: char array array) =
    let len = Array.length rocks[0]
    rocks
    |> Array.map (fun row ->
        row |> Array.indexed |> Array.sumBy (fun (i, c) ->
                match c with
                | 'O' -> len - i
                | _ -> 0
            )
        )
    |> Array.sum

let rollRow (row: char array) =
    // index of where the next rock will roll to
    let mutable roll = 0
    row
    |> Array.indexed
    |> Array.iter (fun (i,c) ->
        match c with
        | '#' -> roll <- i + 1
        | 'O' ->
            row[i] <- '.'
            row[roll] <- 'O'
            roll <- roll + 1
        | _ -> ()
        )

let rotateClockwise (rocks: char array array) = rocks |> Array.rev |> Array.transpose
let rotateAntiClockwise (rocks: char array array) = rocks |> Array.transpose |> Array.rev
    
let roll (rocks: char array array) =
    rocks |> Array.iter rollRow
    rocks
    
let part1 (s: string) =
    s |> Parser.parse |> rotateAntiClockwise |> roll |> weight
    
let cycle (rocks: char array array) =
    rocks
    |> roll
    |> rotateClockwise
    |> roll
    |> rotateClockwise
    |> roll
    |> rotateClockwise
    |> roll
    |> rotateClockwise
    
let mutable step: Map<string, int> = Map.empty

let mkKey (rocks: char array array) = $"%A{rocks}"
    
let findCycle (max: int) (rocks: char array array) =
    let mutable rocks = rocks
    step <- Map.add (mkKey rocks) 0 step
    seq { 1..(max-1) }
    |> Seq.pick (fun i ->
        rocks <- rocks |> cycle
        let key = mkKey rocks
        match Map.tryFind key step with
        | Some x ->
            Some (rocks,x,i)
        | None ->
            step <- Map.add key i step
            None
        )

let runCycles (count: int) (rocks: char array array) =
    let mutable rocks = rocks
    for _ in 0..(count-1) do
        rocks <- rocks |> cycle
    rocks
        

let part2 (s: string) =
    // start with N facing left, W facing down
    let rocks = s |> Parser.parse |> rotateAntiClockwise
    let rocks, startCycle, endCycle = rocks |> findCycle 1000000
    let remain = (1000000000 - startCycle) % (endCycle - startCycle)
    let rocks = rocks |> runCycles remain
    rocks |> weight

