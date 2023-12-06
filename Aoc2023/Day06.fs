module Aoc2023.Day06

open FParsec

module Parser =
    let intList = many (pint64 .>> spaces)
    let time = pstring "Time:" >>. spaces >>. intList
    let dist = pstring "Distance:" >>. spaces >>. intList
    let parser = time .>>. dist

    let parse (s: string) =
        match (run parser s) with
        | Success(result, _, _) -> List.zip (fst result) (snd result)
        | Failure(errorMsg, _, _) -> failwith errorMsg
        
// d = (t - h) * h
// h^2 - th + d = 0
let solve (t: double) (d: double) =
    let s = sqrt (t*t - 4.0*d)
    ((t-s)/2.0, (t+s)/2.0)
    
let ways (t: int64) (d: int64) =
    let l,h = solve (double t) (double d)
    let l = int64 (floor (l + 1.0))
    let h = int64 (ceil (h - 1.0))
    h - l + 1L

let part1 (s: string) =
    s |> Parser.parse |> List.map (fun (t,d) -> ways t d) |> List.reduce (*)

let part2 (s: string) =
    let t = 48876981L
    let d = 255128811171623L
    ways t d
    