module Aoc2023.Day16

open FParsec

let parseWith (parser: Parser<'a,_>) (s: string) =
    match run parser s with
    | Success (result,_,_) -> result
    | Failure (error,_,_) -> failwith error

module Parser =
    let line = many1 (anyOf ".|-\/") .>> spaces
    let parser = many1 line
    
type Dir = Up | Down | Left | Right
type Point = {X: int; Y: int}

module Point =
    let next (dir: Dir) (p: Point) =
        match dir with
        | Up -> { X = p.X; Y = p.Y-1 }
        | Down -> { X = p.X; Y = p.Y+1 }
        | Left -> { X = p.X-1; Y = p.Y }
        | Right -> { X = p.X+1; Y = p.Y }
        
let test = @".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"

let getChar (map: char array array) (p: Point) =
    map |> Array.tryItem p.Y |> Option.bind (Array.tryItem p.X)
    

let rec beam (map: char array array) (point: Point) (dir: Dir) (been: Map<Point,Set<Dir>>)=
    let dirs = been |> Map.tryFind point |> Option.defaultValue Set.empty
    if dirs |> Set.contains dir then
        been
    else
        match getChar map point with
        | None ->
            // we're off the map
            been
        | Some x ->
            // never been here in this direction!
            let been = been |> Map.add point (dirs |> Set.add dir)
            match x with
            | '.' -> been |> beam map (Point.next dir point) dir
            | '\\' ->
                let dir =
                    match dir with
                    | Up -> Left
                    | Down -> Right
                    | Right -> Down
                    | Left -> Up
                been |> beam map (Point.next dir point) dir
            | '/' ->
                let dir =
                    match dir with
                    | Up -> Right
                    | Down -> Left
                    | Right -> Up
                    | Left -> Down
                been |> beam map (Point.next dir point) dir
            | '-' ->
                match dir with
                | Left | Right -> been |> beam map (Point.next dir point) dir
                | Up | Down ->
                    been
                    |> beam map (Point.next Right point) Right
                    |> beam map (Point.next Left point) Left
            | '|' ->
                match dir with
                | Up | Down -> been |> beam map (Point.next dir point) dir
                | Left | Right ->
                    been
                    |> beam map (Point.next Up point) Up
                    |> beam map (Point.next Down point) Down
            | _ -> failwith "wtf"
            
let printMap (map: char array array) (been: Map<Point,Set<Dir>>) =
    map
    |> Array.indexed
    |> Array.map (fun (y,row) ->
        row
        |> Array.indexed
        |> Array.map (fun (x,_) -> if Map.containsKey {X = x; Y=y} been then "#" else "." )
        |> String.concat "")
    |> Array.iter (printfn "%s")

let part1 (s: string) =
    let map =
        s
        |> parseWith Parser.parser
        |> List.map Array.ofList
        |> Array.ofList
        
    Map.empty
    |> beam map {X = 0; Y = 0} Right
    |> Map.count
    
        

let part2 (s: string) =
    let map =
        s
        |> parseWith Parser.parser
        |> List.map Array.ofList
        |> Array.ofList
        
    let lastY = (Array.length map) - 1
    let lastX = (Array.length map[0]) - 1
    
    let right = seq { 0..lastY } |> Seq.map (fun y -> ({X = 0; Y = y}, Right))
    let left = seq { 0..lastY } |> Seq.map (fun y -> ({X = lastX; Y = y}, Left))
    let down = seq { 0..lastX } |> Seq.map (fun x -> ({X = x; Y = 0}, Down))
    let up = seq { 0..lastX } |> Seq.map (fun x -> ({X = x; Y = lastY}, Up))
    
    Seq.collect id [right; left; down; up]
    |> Seq.map (fun (p,dir) -> Map.empty |> beam map p dir |> Map.count)
    |> Seq.max

