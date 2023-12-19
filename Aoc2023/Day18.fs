module Aoc2023.Day18

open FParsec

type Turn = CW | ACW
type Dir = Up | Down | Left | Right

module Dir =
    let turn (d1: Dir) (d2: Dir) =
        match (d1,d2) with
        | Up, Right | Right, Down | Down, Left | Left, Up -> CW
        | Up, Left | Left, Down | Down, Right | Right, Up -> ACW
        | _ -> failwithf "bad turn %A -> %A" d1 d2
type Dig = {Dir: Dir; Dist: int; Color: char list }

module Dig =
    let fromParse (d: char) (dist: int) (c: char list) =
        let dir = 
            match d with
            | 'D' -> Down
            | 'U' -> Up
            | 'L' -> Left
            | 'R' -> Right
            | x -> failwithf $"invalid dir: %c{x}"
        { Dir = dir; Dist = dist; Color = c }

module Parser =
    let dir = anyOf "LRUD" .>> spaces
    let meters = pint32 .>> spaces
    let color = skipString "(#" >>. many1 hex .>> skipString ")" .>> spaces
    let row = pipe3 dir meters color Dig.fromParse
    let parser = many1 row .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let test = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"

type Point = {X: int; Y: int}

module Point =
    let add (m: int) (d: Dir) (p: Point) =
        match d with
        | Up -> {X = p.X; Y = p.Y - m}
        | Down -> {X = p.X; Y = p.Y + m}
        | Left -> {X = p.X - m; Y = p.Y}
        | Right -> {X = p.X + m; Y = p.Y}
        
    let manhatten (p1: Point) (p2: Point) =
        abs (p1.X - p2.X) + abs (p1.Y - p2.Y)

// horizontal S-bend
// horizontal U-bend
// vertical
type TrenchKind = S | U | V
type Trench = {start: Point; stop: Point; dir: Dir; color: char list; startTurn: Turn; stopTurn: Turn; }

module Trench =
    let kind (t: Trench) =
        if t.start.X = t.stop.X then V
        elif t.startTurn = t.stopTurn then U
        else S

let dig (start: Point) (plan: Dig list) =
    let mutable current = start
    let mutable trenches = List.empty
    let first = plan |> List.head
    let last = plan |> List.last
    let mutable dir = last.Dir
    
    for p,next in plan |> List.pairwise do
        let stop = Point.add p.Dist p.Dir current
        // can probably sort so start is always the lower value
        trenches <- {
            start = current
            stop = stop
            dir = p.Dir
            color = p.Color
            startTurn = Dir.turn dir p.Dir
            stopTurn = Dir.turn p.Dir next.Dir 
        }::trenches
        current <- stop
        dir <- p.Dir
        
    let stop = Point.add last.Dist last.Dir current
    if stop <> start then
        failwithf "start != stop: %A %A" start stop
    
    // Add the last trench
    trenches @ [{
        start = current
        stop = stop
        color = last.Color
        dir = last.Dir
        startTurn = Dir.turn dir last.Dir
        stopTurn = Dir.turn last.Dir first.Dir 
    }]
    
let includes (a: int) (b: int) (x: int) = (a <= x && x <= b) || (b <= x && x <= a)
let between (a: int) (b: int) (x: int) = (a < x && x < b) || (b < x && x < a)

let dump<'a> (x: 'a) =
    printfn "%A" x
    x
    
let fill (ts: Trench list) =
    // 3 possibilities:
    //       #      #
    // ####  ####   #
    // #  #     #   #
    let row (y: int) =
        ts
        |> List.filter (fun t ->  (t.start.Y = y && t.stop.Y = y) || between t.start.Y t.stop.Y y)
        |> List.map (fun t -> (min t.start.X t.stop.X, max t.start.X t.stop.X, Trench.kind t))
        |> List.sortBy (fun (x,_,_) -> x)
        |> List.fold (fun (inside,lastStop,count) (start, stop, kind) ->
            let count = if inside then count + start - lastStop - 1 else count
            match kind with
            | V | S -> (inside |> not, stop, count)
            | U -> (inside, stop, count)
            ) (false, 0, 0)
        |> fun (_,_,count) -> count |> int64
            
    let minY = ts |> List.map (fun t -> min t.start.Y t.stop.Y) |> List.min        
    let maxY = ts |> List.map (fun t -> max t.start.Y t.stop.Y) |> List.max
    
    let mutable total = 0L
    
    for y in minY..maxY do
        total <- total + row y
        
    let edges = ts |> List.sumBy (fun t -> Point.manhatten t.start t.stop |> int64)
    total + edges
    
let part1 (s: string) =
    s |> Parser.parse |> dig {X=0;Y=0} |> fill
    
let implode (xs:char list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()
    
let useHex (ds: Dig list) =
    ds
    |> List.map (fun d ->
        let dist, dir = d.Color |> List.splitAt 5
        let dist = System.Convert.ToInt32(implode dist, 16)
        let dir =
            match dir with
            | ['0'] -> Right
            | ['1'] -> Down
            | ['2'] -> Left
            | ['3'] -> Up
            | _ -> failwithf "bad dir: %A" dir
        {Dir = dir; Dist = dist; Color = d.Color }
        )

let part2 (s: string) =
     s |> Parser.parse |> useHex |> dig {X=0;Y=0} |> fill

