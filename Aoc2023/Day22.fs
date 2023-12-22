module Aoc2023.Day22

open FParsec

type Point = {x:int; y: int; z: int}
type Brick = Point * Point

module Point =
    let create (x: int) (y: int) (z: int) = {x=x;y=y;z=z}
    
module Parser =
    let parseXYZ = pipe3 (pint32 .>> skipChar ',') (pint32 .>> skipChar ',') pint32 Point.create
    let parseLine = parseXYZ .>> skipChar '~' .>>. parseXYZ .>> spaces
    let parser = many1 parseLine

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let test = "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
"

module Brick =
    let pointsForZ ((p1,p2): Brick) (z: int) =
        seq {
            for x in (min p1.x p2.x)..(max p1.x p2.x) do
                for y in (min p1.y p2.y)..(max p1.y p2.y) do
                    yield {x=x;y=y;z=z}
        }
        
    let points ((p1,p2): Brick) =
        seq {
            for x in (min p1.x p2.x)..(max p1.x p2.x) do
                for y in (min p1.y p2.y)..(max p1.y p2.y) do
                    for z in (min p1.z p2.z)..(max p1.z p2.z) do
                        yield {x=x;y=y;z=z}
        }

let settleBrick (brick: Brick) (occupied: Map<Point,Brick>) =
    let rec settle (z: int) =
        if z > 1 then
            if Brick.pointsForZ brick (z-1) |> Seq.forall (fun p -> Map.containsKey p occupied |> not) then
                settle (z-1)
            else
                z
        else
            z
            
    let p1, p2 = brick
    let minZ = min p1.z p2.z
    let z = settle minZ
    let diff = minZ - z
    let newBrick = ({x=p1.x;y=p1.y;z=p1.z - diff}, {x=p2.x;y=p2.y;z=p2.z - diff})
    
    // remove the old brick points
    let occupied = brick |> Brick.points |> Seq.fold (fun occ p -> Map.remove p occ) occupied
    // add the new brick points
    let occupied = newBrick |> Brick.points |> Seq.fold (fun occ p -> Map.add p newBrick occ) occupied
    
    (newBrick, occupied)
    
let settleBricks (bricks: Brick list) =
    let occupied = bricks |> Seq.collect (fun b -> b |> Brick.points |> Seq.map (fun p -> (p,b))) |> Map.ofSeq
    bricks
    |> List.sortBy (fun (p1,p2) -> min p1.z p2.z)
    |> List.fold (fun (settled, occupied)  brick ->
        let brick, occ = settleBrick brick occupied
        (settled @ [brick], occ)
        ) (List.empty, occupied)
    
// map of brick -> bricks supporting brick
let supportedBy (bricks: Brick list) (occupied: Map<Point,Brick>)=
    bricks
    |> List.map (fun (p1,p2) ->
        let minZ = min p1.z p2.z
        let supporting =
            Brick.pointsForZ (p1,p2) (minZ - 1)
            |> Seq.choose (fun p -> Map.tryFind p occupied)
            |> Set.ofSeq
        ((p1,p2), supporting)
        )
    |> Map.ofList

let part1 (s: string) =
    let bricks, occupied =
        s
        |> Parser.parse
        |> settleBricks
    
    let singleSupport =
        supportedBy bricks occupied
        |> Map.filter (fun b s -> Set.count s = 1)
        |> Map.values
        |> Set.ofSeq
        
    List.length bricks - Set.count singleSupport
    
let destroy (supportedBy: Map<Brick,Set<Brick>>) (supporting: Map<Brick,Set<Brick>>) (b: Brick) =
    let rec fall (fallen: Set<Brick>) (b: Brick) =
        let fallen = Set.add b fallen
        Map.tryFind b supporting
        |> Option.defaultValue Set.empty
        // filter to the unsupported bricks
        |> Set.filter (fun candidate -> Set.isSubset supportedBy[candidate] fallen)
        // remove one at a time
        |> Set.fold fall fallen
    
    // count how many fall and remove one for the the removed brick
    (fall Set.empty b |> Set.count) - 1

let part2 (s: string) =
    let bricks, occupied =
        s
        |> Parser.parse
        |> settleBricks
        
    let supportedBy = supportedBy bricks occupied
    
    let addSupporting (b: Brick) (m: Map<Brick,Set<Brick>>) (supportedBy: Brick) =
        m
        |> Map.change supportedBy (fun existing ->
            match existing with
            | None -> Some ([b] |> Set.ofList)
            | Some bs -> Some (Set.add b bs))
    
    // map of brick -> bricks supported by brick
    let supporting =
        supportedBy
        |> Map.fold (fun m b sbs -> sbs |> Set.fold (addSupporting b) m) Map.empty
        
    bricks
    |> List.sumBy (destroy supportedBy supporting)

