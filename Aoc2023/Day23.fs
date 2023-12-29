module Aoc2023.Day23

open FParsec

module Parser =
    let parseLine = many1 (anyOf "#.^>v<") .>> spaces
    let parser = many1 parseLine .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result |> List.map Array.ofList |> Array.ofList
        | Failure (error,_,_) -> failwith error
        
let test = "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
"

type Dir = Up | Down | Left | Right
type Point = {x: int; y: int}

module Point =
    let next (d: Dir) (p: Point) =
        match d with
        | Up -> { x = p.x; y = p.y-1 }
        | Down -> { x = p.x; y = p.y+1 }
        | Left -> { x = p.x-1; y = p.y }
        | Right -> { x = p.x+1; y = p.y }
    
    let manhatten (p1: Point) (p2: Point) =
        abs (p1.x - p2.x) + abs (p1.y - p2.y)

let findPath (map: char array array) (y: int) =
    let x =
        map[y]
        |> Array.indexed
        |> Array.pick (fun (x,c) -> if c = '.' then Some x else None)
    {x=x;y=y}
    
let start map = findPath map 0
let stop map = findPath map (Array.length map - 1)

let inMap (map: char array array) (p: Point) =
    p.x >= 0 && p.x < (Array.length map[0]) && p.y >= 0 && p.y < (Array.length map)

let neighbors (map: char array array) (p: Point) =
    [Left;Right;Up;Down]
    |> List.map (fun d -> (d,Point.next d p))
    |> List.filter (fun (_,p) -> inMap map p)
    |> List.filter (fun (d,p) ->
        match map[p.y][p.x], d with
        | '.', _ -> true
        | '>', Right -> true
        | '^', Up -> true
        | '<', Left -> true
        | 'v', Down -> true
        | _ -> false
        )
    |> List.map snd
        
let neighbors2 (map: char array array) (p: Point) =
    [Left;Right;Up;Down]
    |> List.map (fun d -> Point.next d p)
    |> List.filter (inMap map)
    |> List.filter (fun p -> map[p.y][p.x] <> '#')

let hike (map: char array array) (neighbors: char array array -> Point -> Point list) (start: Point) (stop: Point) =
    let rec hike' (seen: Set<Point>) (dist: int) (p: Point): int option =
        let seen = Set.add p seen
        if p = stop then
            Some dist
        else
            let options = 
                neighbors map p
                |> List.filter (fun p -> Set.contains p seen |> not)
                |> List.choose (fun p -> hike' seen (dist+1) p)
                
            match options with
            | [] -> None
            | os -> os |> List.max |> Some
            
    hike' Set.empty 0 start

let part1 (s: string) =
    let map = s |> Parser.parse
    hike map neighbors (start map) (stop map)
    
type Section = {set: Set<Point>; ends: Point list}

module Section =
    let add (p: Point) (s: Section) = {set = Set.add p s.set; ends = s.ends}
    let contains (p: Point) (s: Section) = Set.contains p s.set
    let start (p: Point) = {set = Set.empty; ends = [p]} |> add p
    let stop (p: Point) (s: Section) = {set = Set.add p s.set; ends = p::s.ends}

let sections (map: char array array) (start: Point) =
    let rec walk (sections: Section list) (current: Section) (p: Point) =
        let current = Section.add p current
        let options = neighbors2 map p |> List.filter (fun n -> Section.contains n current |> not)
        match options with
        | [] -> (Section.stop p current)::sections
        | [n] -> walk sections current n
        | os ->
            let sections = (Section.stop p current)::sections
            os
            |> List.fold (fun ss n ->
                if ss |> List.exists (Section.contains n) |> not then
                    walk ss (Section.start p) n
                else
                    ss
                ) sections
            
    walk [] (Section.start start) start
    
let hike2 (m: Map<Point, (Point * int) list>) (start: Point) (stop: Point) =
    let rec walk (seen: Set<Point>) (p: Point) (dist: int) =
        if p = stop then
            Some dist
        else
            let seen = Set.add p seen
            let options = m[p] |> List.filter (fun s -> Set.contains (fst s) seen |> not)
                
            match options with
            | [] -> None
            | os ->
                match List.choose (fun (n,d) -> walk seen n (dist+d)) os with
                | [] -> None
                | os -> os |> List.max |> Some
            
    walk Set.empty start 0
    
let addSectionToMap (s: Section) (m: Map<Point, (Point * int) list>) =
    let update (p: Point) (x: (Point * int) list option) =
        let add = (p, Set.count s.set - 1)
        match x with
        | None -> Some [add]
        | Some ps -> Some (add::ps)
    match s.ends with
    | [p1;p2] -> m |> Map.change p1 (update p2) |> Map.change p2 (update p1)
    | _ -> failwithf "bad section: %A" s
    
let part2 (s: string) =
    let map = s |> Parser.parse
    let sections = sections map (start map)
    
    // map of section connections
    let connections = sections |> List.fold (fun m s -> addSectionToMap s m) Map.empty
        
    hike2 connections (start map) (stop map)

