module Aoc2023.Day11

open FParsec

module Parser =
    let row = many1 (anyOf ".#") .>> spaces
    let parser = many1 row

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
type Point = { X: int64; Y: int64 }

module Point =
    let dist (p1: Point) (p2: Point) =
        {X = abs (p1.X - p2.X); Y = abs (p1.Y - p2.Y)}
        
let points (map: char list list) =
    let rowToPoints y cs = cs |> List.indexed |> List.choose (fun (x,c) -> if c = '#' then Some {X = x; Y = y} else None)
    map |> List.indexed |> List.fold (fun ps (y,cs) -> (cs |> rowToPoints y) @ ps) List.empty
    
let between (a: int64) (b: int64) (v: int64) =
    if a > b then
        b < v && v < a
    else
        a < v && v < b

let empty (f: Point -> int64) (points: Point list) =
    let m = points |> List.map f
    let s = m |> Set.ofList
    
    seq { (List.min m)..(List.max m) }
    |> Seq.filter (fun x -> Set.contains x s |> not)
    |> List.ofSeq

let dist (extra: int64) (emptyX: int64 list) (emptyY: int64 list) (p1: Point) (p2: Point) =
    let d = Point.dist p1 p2
    let xExtra = emptyX |> List.filter (between p1.X p2.X) |> List.length |> int64
    let yExtra = emptyY |> List.filter (between p1.Y p2.Y) |> List.length |> int64
    
    d.X + d.Y + ((extra - 1L) * (xExtra + yExtra))
    
let dists (extra: int64) (points: Point list) =
    let calcDist = dist extra (empty (_.X) points) (empty (_.Y) points)
    
    let rec loop (ps: Point list) (acc: int64 list) =
        match ps with
        | p1::rest -> loop rest (acc @ (rest |> List.map (calcDist p1)))
        | [] -> acc
    
    loop points List.empty

let test = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"

let solve (extra: int64) (s: string) = s |> Parser.parse |> points |> dists extra |> List.sum

let part1 (s: string) = s |> solve 2

let part2 (s: string) = s |> solve 1000000

