module Aoc2023.Day10

open FParsec

module Parser =
    let pipes = many1 (anyOf "|-LJ7F.S") .>> spaces
    let parser = many1 pipes

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
       
type Point = { X: int; Y: int }
type Dir = Left | Right | Up | Down

let findPath (map: char array array) =
    let width = map[0].Length
    let inBounds (p: Point) =
        p.X >= 0 && p.X < width && p.Y >= 0 && p.Y < map.Length
        
    let move (p: Point) (dir: Dir) =
        match dir with
        | Up -> {X = p.X; Y = p.Y - 1}
        | Down -> {X = p.X; Y = p.Y + 1}
        | Left -> {X = p.X - 1; Y = p.Y}
        | Right -> {X = p.X + 1; Y = p.Y}
    
    let nextDir (p: Point) (dir: Dir) =
        match (map[p.Y][p.X], dir) with
        | '|', Up  -> Some Up
        | '|', Down -> Some Down
        | '-', Left -> Some Left
        | '-', Right -> Some Right
        | 'L', Down -> Some Right
        | 'L', Left -> Some Up
        | 'J', Down -> Some Left
        | 'J', Right -> Some Up
        | '7', Up -> Some Left
        | '7', Right -> Some Down
        | 'F', Up -> Some Right
        | 'F', Left -> Some Down
        | _ -> None
    
    let rec follow (p: Point) (path: Point list) (dir: Dir) =
        let nextPoint = move p dir
        let path = path @ [nextPoint]
        if inBounds nextPoint then
            if map[nextPoint.Y][nextPoint.X] = 'S' then
                Some path
            else
                match nextDir nextPoint dir with
                | Some d -> follow nextPoint path d
                | None -> None
        else
            None
        
    let sy = map |> Array.findIndex (fun row -> row |> Array.exists (fun c -> c = 'S'))
    let sx = map[sy] |> Array.findIndex (fun c -> c = 'S')
    let s = {X = sx; Y = sy}
   
    // we follow the path twice :(
    [Left; Right; Up; Down]
    |> List.map (follow s List.empty)
    
let test = "-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"

let part1 (s: string) =
    let map =
        s
        |> Parser.parse
        |> List.map Array.ofList
        |> Array.ofList
    let path = findPath map |> List.choose id |> List.head
    path.Length / 2
    
type State = In | Out

let inside (map: char array array) (path: Point list) =
    let set = path |> Set.ofList
    let mutable inside = 0
    
    let onPath (x: int) (y: int) = set |> Set.contains {X = x; Y = y}
    
    for y, row in Array.indexed map do
        let mutable state = Out
        
        let toggle () =
            match state with
            | In -> state <- Out
            | Out -> state <- In
        
        let mutable lastTurn = '.'
        for x, c in Array.indexed row do
            if onPath x y then
                match c, lastTurn with
                | '|', _ -> toggle()
                | '7', 'F' -> lastTurn <- '.'
                | '7', 'L' ->
                    toggle()
                    lastTurn <- '.'
                | 'J', 'L' -> lastTurn <- '.'
                | 'J', 'F' ->
                    toggle()
                    lastTurn <- '.'
                | 'F', _ -> lastTurn <- 'F'
                | 'L', _ -> lastTurn <- 'L'
                | _ -> ()
            else
                if state = In then
                    inside <- inside + 1
                    
    inside
                
let test2 = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"

let part2 (s: string) =
    let map =
        s
        |> Parser.parse
        |> List.map Array.ofList
        |> Array.ofList
    let path = findPath map |> List.choose id |> List.head
    inside map path

