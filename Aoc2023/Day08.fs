module Aoc2023.Day08

open FParsec

module Parser =
    let directions = many (anyOf "LR")
    let isNodeIdChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    let nodeId = manySatisfy isNodeIdChar
    let node = pipe3 (nodeId .>> skipString " = (") (nodeId .>> skipString ", ") (nodeId .>> skipString ")" .>> spaces) (fun n l r -> (n,l,r))
    let parser = directions .>> spaces .>>. many node

    let parse (s: string) =
        match (run parser s) with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

let test = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

let test2 = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

let test3 = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

let steps (map: Map<string, string * string>) (dirs: char list) =
    let dirs = dirs |> Array.ofList
    
    let rec steps' (stepNo: int) (node: string) =
        let dir = dirs[stepNo%dirs.Length]
        let next =
            match dir with
            | 'L' -> fst map[node]
            | 'R' -> snd map[node]
            | _ -> failwith "wtf"
        let stepNo = stepNo + 1
        if next = "ZZZ" then stepNo else steps' stepNo next
        
    steps' 0 "AAA"
        
let part1 (s: string) =
    let dirs, nodes = s |> Parser.parse
    let map = nodes |> List.map (fun (n,l,r) -> (n, (l,r))) |> Map.ofList
    steps map dirs
    
let findLoop (map: Map<string, string * string>) (dirs: char list) (start: string) =
    let dirs = dirs |> Array.ofList
    
    let rec matches (stepNo: int) (path: char list) =
        match path with
        | [] -> true
        | p::rest ->
            let dir = dirs[stepNo%dirs.Length]
            match (p,dir) with
            | 'L','R' | 'R','L' -> false
            | _ -> matches (stepNo + 1) rest
    
    let rec loop (stepNo: int) (node: string) (seen: Map<string,int * char list>) (path: char list) =
        let isLoop (seenPath: char list) =
            let ldiff = path.Length - seenPath.Length
            if ldiff >= dirs.Length then
                let path = List.take ldiff path |> List.rev
                matches stepNo path
            else
                // diff must be longer that dirs to be a loop
                false
        
        match Map.tryFind node seen with
        | Some (step, seenPath) when isLoop seenPath ->
            let zs =
                seen
                |> Map.filter (fun n (st,_) -> n.EndsWith('Z') && st >= step)
                |> Map.values
                |> Seq.map fst
                |> List.ofSeq
            // (repeat interval, z positions)
            (stepNo - step, zs)
        | _ ->
            let dir = dirs[stepNo%dirs.Length]
            let options = map[node]
            let next =
                match dir with
                | 'L' -> fst options
                | 'R' -> snd options
                | _ -> failwith "wtf"
            let p = if fst options = snd options then '*' else dir
            loop (stepNo + 1) next (Map.add node (stepNo, path) seen) (p::path)
        
    loop 0 start Map.empty List.empty

let rec gcd a b =
    match (a,b) with
    | x,y when x = y -> x
    | x,y when x > y -> gcd (x-y) y
    | x,y -> gcd x (y-x)

let lcm a b = a*b/(gcd a b)

let part2 (s: string) =
    let dirs, nodes = s |> Parser.parse
    let map = nodes |> List.map (fun (n,l,r) -> (n, (l,r))) |> Map.ofList
    
    map
    |> Map.keys
    |> Seq.filter (fun n -> n.EndsWith('A'))
    |> Seq.map (findLoop map dirs)
    |> Seq.map (fun (x,_) -> int64 x)
    |> Seq.fold lcm 1
        
    

