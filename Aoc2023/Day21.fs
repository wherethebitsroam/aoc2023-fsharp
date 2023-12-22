module Aoc2023.Day21

open FParsec

module Parser =
    let parseLine = many1 (anyOf ".#S") .>> spaces
    let parser = many1 parseLine

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error

let test = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"

type Point = {X: int; Y: int}

module Point =
    let neighbors (p: Point) =
        [
            {X=p.X; Y=p.Y+1}
            {X=p.X; Y=p.Y-1}
            {X=p.X+1; Y=p.Y}
            {X=p.X-1; Y=p.Y}
        ]

type Garden = {Dim: int; Plots: Set<Point>; Start: Point}

module Garden =
    let fromMap (map: char list list) =
        let plots, start =
            map
            |> List.indexed
            |> List.fold (fun (set, start) (y,row) ->
                row
                |> List.indexed
                |> List.fold (fun (set, start) (x,c) ->
                    match c with
                    | '.' -> (Set.add {X=x;Y=y} set, start)
                    | 'S' -> (Set.add {X=x;Y=y} set, Some {X=x;Y=y})
                    | _ -> (set, start)
                    ) (set, start)
                ) (Set.empty, None)
        
        match start with
        | Some start ->
            let dim = List.length map
            {Plots = plots; Start = start; Dim = dim}
        | None -> failwith "no start found"
    
    let isPlot (p: Point) (g: Garden) =
        let x = p.X % g.Dim
        let x = if x < 0 then x+g.Dim else x
        let y = p.Y % g.Dim
        let y = if y < 0 then y+g.Dim else y
        Set.contains {X=x;Y=y} g.Plots
    
    let centeredPoints (g: Garden) =
        let half = g.Dim/2
        seq {
            for y in (g.Start.Y-half)..(g.Start.Y+half) do
                for x in (g.Start.X-half)..(g.Start.X+half) do
                    yield {X=x;Y=y}
        }
        
    let offsetPoints (offset: Point) (g: Garden) =
        centeredPoints g
        |> Seq.map (fun p -> {X = p.X + offset.X*g.Dim; Y = p.Y + offset.Y*g.Dim })
    
let rec steps (reached: Set<Point>) (stepsRemaining: int) (g: Garden) =
    if stepsRemaining > 0 then
        let next =
            reached
            |> Set.fold (fun set p ->
                p
                |> Point.neighbors
                |> List.filter (fun p -> Garden.isPlot p g)
                |> List.fold (fun set p -> Set.add p set) set
                ) Set.empty
        
        steps next (stepsRemaining - 1) g
    else
        reached
        
let print (seen: Set<Point>) (g: Garden) =
    let minX = seen |> List.ofSeq |> List.map (_.X) |> List.min
    let maxX = seen |> List.ofSeq |> List.map (_.X) |> List.max
    let minY = seen |> List.ofSeq |> List.map (_.Y) |> List.min
    let maxY = seen |> List.ofSeq |> List.map (_.Y) |> List.max
    
    for y in minY..maxY do
        if y%g.Dim = 0 then
            printfn ""
        for x in minX..maxX do
            if x%g.Dim = 0 then
                printf " "
            let p = {X=x;Y=y}
            if p = g.Start then
                printf "S"
            if Set.contains p seen then
                printf "O"
            elif Garden.isPlot p g then
                printf "."
            else
                printf "#"
        printfn ""
        
let printGardens (x: int) (seen: Set<Point>) (g: Garden) =
    let range = x+1
    for y in -range..range do
        for x in -range..range do
            let count = Garden.offsetPoints {X=x;Y=y} g |> Seq.sumBy (fun p -> if Set.contains p seen then 1 else 0)
            printf "%5d " count
        printfn ""

let part1 (s: string) =
    let garden = s |> Parser.parse |> Garden.fromMap
    steps (Set.add garden.Start Set.empty) 64 garden |> Set.count
    
let part2 (s: string) =
    let g = s |> Parser.parse |> Garden.fromMap
    
    // 26501365 % 131 = 65
    // let seen = steps (Set.add g.Start Set.empty) (65+g.Dim*x) g
    // printGardens x seen g
    //
    // x=2:
    // 0     0     0     0     0     0     0
    // 0     0   996  5874   976     0     0
    // 0   996  6830  7791  6831   976     0
    // 0  5866  7791  7787  7791  5878     0
    // 0   986  6823  7791  6834   986     0
    // 0     0   986  5870   986     0     0
    // 0     0     0     0     0     0     0
    // x=3:
    // 0     0     0     0     0     0     0     0     0
    // 0     0     0   996  5874   976     0     0     0
    // 0     0   996  6830  7791  6831   976     0     0
    // 0   996  6830  7791  7787  7791  6831   976     0
    // 0  5866  7791  7787  7791  7787  7791  5878     0
    // 0   986  6823  7791  7787  7791  6834   986     0
    // 0     0   986  6823  7791  6834   986     0     0
    // 0     0     0   986  5870   986     0     0     0
    // 0     0     0     0     0     0     0     0     0
    
    let x = 26501365 / 131 |> int64
    let x1 = x - 1L
    let total =
        (5874L+5878L+5870L+5866L) // points
        + x*(996L+976L+986L+986L) // outside edges
        + x1*(6830L+6823L+6834L+6831L) // inside edges
        + x*x*7791L // evens
        + x1*x1*7787L // odds
    
    total

