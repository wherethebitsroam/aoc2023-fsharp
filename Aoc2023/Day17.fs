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
    let turn (d: Dir) =
        match d with
        | Right | Left -> [Up; Down]
        | Up | Down -> [Left; Right]

type Point = {X: int; Y: int}

module Point =
    let next (dir: Dir) (p: Point) (dist: int)=
        match dir with
        | Up -> { X = p.X; Y = p.Y-dist }
        | Down -> { X = p.X; Y = p.Y+dist }
        | Left -> { X = p.X-dist; Y = p.Y }
        | Right -> { X = p.X+dist; Y = p.Y }
        
    let manhatten (p1: Point) (p2: Point) =
        abs (p1.X - p2.X) + abs (p1.Y - p2.Y)
    
let inMap (map: int array array) (p: Point) =
    p.X >= 0 && p.X < (Array.length map[0]) && p.Y >= 0 && p.Y < (Array.length map)
    
    
module AStar =

    type Config<'a> =
        {
          /// <summary>
          /// A method that, given a source, will return its neighbours.
          /// </summary>
          neighbours: 'a -> seq<'a>
          /// <summary>
          /// Given two nodes that are next to each other, return the g cost between them.
          /// The g cost is the cost of moving from one to the other directly.
          /// </summary>
          gCost: 'a -> 'a -> float
          /// <summary>
          /// Given two nodes, return the f cost between them. This is a heuristic score used from a given node to the goal.
          /// Line-of-sight distance is an example of how this might be defined.
          /// </summary>
          fCost: 'a -> 'a -> float
          /// <summary>
          /// The maximum number of tiles to check - used to limit overly long searches when accuracy is not paramount
          /// </summary>
          maxIterations: int option }

    let search<'a when 'a: comparison> (starts: 'a list) (goal: 'a) (isGoal: 'a -> bool) config : seq<'a> option =

        let rec reconstructPath cameFrom current =
            seq {
                yield current

                match Map.tryFind current cameFrom with
                | None -> ()
                | Some next -> yield! reconstructPath cameFrom next
            }

        let rec crawler closedSet (openSet, gScores, fScores, cameFrom) =
            match config.maxIterations with
            | Some n when n = Set.count closedSet -> None
            | _ ->
                match List.sortBy (fun n -> Map.find n fScores) openSet with
                | current :: _ when isGoal current -> Some <| reconstructPath cameFrom current
                | current :: rest ->
                    let gScore = Map.find current gScores
                    
                    let next =
                        config.neighbours current
                        |> Seq.filter (fun n -> closedSet |> Set.contains n |> not)
                        |> Seq.fold
                            (fun (openSet, gScores, fScores, cameFrom) neighbour ->
                                let tentativeGScore = gScore + config.gCost current neighbour

                                if List.contains neighbour openSet
                                   && tentativeGScore >= Map.find neighbour gScores then
                                    (openSet, gScores, fScores, cameFrom)
                                else
                                    let newOpenSet =
                                        if List.contains neighbour openSet then
                                            openSet
                                        else
                                            neighbour :: openSet

                                    let newGScores =
                                        Map.add neighbour tentativeGScore gScores

                                    let newFScores =
                                        Map.add neighbour (tentativeGScore + config.fCost neighbour goal) fScores

                                    let newCameFrom = Map.add neighbour current cameFrom
                                    newOpenSet, newGScores, newFScores, newCameFrom)
                            (rest, gScores, fScores, cameFrom)

                    crawler (Set.add current closedSet) next
                | _ -> None

        let gScores = starts |> List.map (fun start -> (start, 0.)) |> Map.ofList

        let fScores = starts |> List.map (fun start -> (start, config.fCost start goal)) |> Map.ofList

        crawler Set.empty (starts, gScores, fScores, Map.empty)

// P in the point in the map
// D is the direction we entered the point
type Q = {P: Point; D: Dir}

let neighbours (map: int array array) (lo: int) (hi: int) (q: Q) =
    // assume we need to turn and give all neighbors in those directions
    Dir.turn q.D
    |> Seq.ofList
    |> Seq.collect (fun d -> seq {lo..hi} |> Seq.map (Point.next d q.P) |> Seq.map (fun p -> {P = p; D = d}))
    |> Seq.filter (fun q -> inMap map q.P)
    
let gCost (map: int array array) (q1: Q) (q2: Q) =
    let mutable total = 0
    if q1.P.Y = q2.P.Y then
        // horizontal
        let minX = min q1.P.X q2.P.X
        let maxX = max q1.P.X q2.P.X
        for x in minX..maxX do
            total <- total + map[q1.P.Y][x]
    else
        // vertical
        let minY = min q1.P.Y q2.P.Y
        let maxY = max q1.P.Y q2.P.Y
        for y in minY..maxY do
            total <- total + map[y][q1.P.X]
            
    total - map[q1.P.Y][q1.P.X] |> float
    
let fCost (q1: Q) (q2: Q) =
    Point.manhatten q1.P q2.P |> float
    
let solve (lo: int) (hi: int) (map: int array array) =
    let pEnd = {X = (Array.length map[0]) - 1; Y = (Array.length map) - 1}
    let result =
        AStar.search [{P = {X=0;Y=0}; D = Down}; {P = {X=0;Y=0}; D = Right}] {P = pEnd; D = Down} (fun q -> q.P = pEnd) {
            neighbours = neighbours map lo hi
            gCost = gCost map
            fCost = fCost
            maxIterations = Some 100000
        }
        
    match result with
    | None -> None
    | Some s ->
        Seq.pairwise s |> Seq.sumBy (fun (q1,q2) -> gCost map q2 q1) |> Some

let part1 (s: string) =
    s |> Parser.parse |> solve 1 3
    
let part2 (s: string) =
    s |> Parser.parse |> solve 4 10
