module Aoc2023.Day17

open Microsoft.FSharp.Core

module Parser =
    let parse (s: string) =
        s.Trim().Split('\n')
        |> Array.map (fun x -> x |> Seq.map (fun c -> int c - int '0') |> Array.ofSeq )
    
let test = "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"

type Dir = Up | Down | Left | Right

module Dir =
    let next (d: Dir) (len: int) =
        let baseDirs =
            match d with
            | Right | Left -> [Up; Down]
            | Up | Down -> [Left; Right]
        
        if len < 3 then
            d::baseDirs
        else
            baseDirs
    
    let next2 (d: Dir) (len: int) =
        let baseDirs =
            match d with
            | Right | Left -> [Up; Down]
            | Up | Down -> [Left; Right]
            
        if len < 4 then
            [d]
        elif len < 10 then
            d::baseDirs
        else
            baseDirs

type Point = {X: int; Y: int}

module Point =
    let next (dir: Dir) (p: Point) =
        match dir with
        | Up -> { X = p.X; Y = p.Y-1 }
        | Down -> { X = p.X; Y = p.Y+1 }
        | Left -> { X = p.X-1; Y = p.Y }
        | Right -> { X = p.X+1; Y = p.Y }
        
    let manhatten (p1: Point) (p2: Point) =
        abs (p1.X - p2.X) + abs (p1.Y - p2.Y)
    
let inMap (map: int array array) (p: Point) =
    p.X >= 0 && p.X < (Array.length map[0]) && p.Y >= 0 && p.Y < (Array.length map)
    
    
type Opt = {P: Point; D: Dir; Len: int; Cost: int; Path: Point list; MinCost: int}

module Opt =
    let create (p: Point) (d: Dir) =
        {P = p; D = d; Len = 0; Cost = 0; Path = []; MinCost = 10000000 }
    
let hike (map: int array array) (start: Point) (stop: Point) (dirFn: Dir -> int -> Dir list) =
    let mutable seen: Map<Point * Dir * int, int> = Map.empty
    
    let nextOpts (opt: Opt) =
        dirFn opt.D opt.Len
        |> List.map (fun d -> (Point.next d opt.P, d) )
        |> List.filter (fun (p,_) -> inMap map p)
        |> List.choose (fun (p,d) ->
            let cost = opt.Cost + map[p.Y][p.X]
            let len = if d = opt.D then opt.Len+1 else 1
            
            let keep =
                match seen |> Map.tryFind (opt.P,d,len) with
                | None -> true
                | Some x -> cost < x
                
            if keep then
                seen <- seen |> Map.add (opt.P,d,len) cost
                Some {
                    P = p
                    D = d
                    Len = len
                    Cost = cost
                    Path = opt.Path @ [p]
                    MinCost = cost + Point.manhatten p stop
                }
            else
                None)
        
    let rec run (options: Set<Opt>) =
        let sorted =
            options
            |> Seq.sortBy (fun x -> x.MinCost)
            |> List.ofSeq
            
        match sorted with
        | [] -> None
        | next::rest ->
            if next.P = stop then
                Some next
            else
                let opts = nextOpts next
                run (rest @ opts |> Set.ofList)
            
    run ([Opt.create start Right] |> Set.ofList)

let part1 (s: string) =
    let map = s |> Parser.parse
    let pEnd = {X = (Array.length map[0]) - 1; Y = (Array.length map) - 1}
    hike map {X=0;Y=0} pEnd Dir.next

let part2 (s: string) =
    let map = s |> Parser.parse
    let pEnd = {X = (Array.length map[0]) - 1; Y = (Array.length map) - 1}
    hike map {X=0;Y=0} pEnd Dir.next2

