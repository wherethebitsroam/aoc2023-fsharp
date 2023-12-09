module Aoc2023.Day09

open FParsec

module Parser =
    let intList = sepBy pint64 (pstring " ")
    let parser = sepBy intList (skipChar '\n') .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let rec next (series: int64 list) =
    let diffs =
        series
        |> List.pairwise
        |> List.map (fun (a,b) -> b - a)
        
    if diffs |> List.pairwise |> List.forall (fun (a,b) -> a = b) then
        (series |> List.last) + (diffs |> List.head)
    else
        (series |> List.last) + next diffs

let test = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

let part1 (s: string) =
    s
    |> Parser.parse
    |> List.filter (fun l -> l.Length > 0)
    |> List.map next
    |> List.sum

let part2 (s: string) =
    s
    |> Parser.parse
    |> List.filter (fun l -> l.Length > 0)
    |> List.map List.rev
    |> List.map next
    |> List.sum

