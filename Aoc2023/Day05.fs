module Aoc2023.Day05

open FParsec

type Mapping = {Src: int64; Dst: int64; Length: int64}

module Mapping =
    let empty = { Src = 0L; Dst = 0L; Length = 0L }
    let matchesSrc (x: int64) (m: Mapping) = x >= m.Src && x < m.Src + m.Length
    let matchesDst (x: int64) (m: Mapping) = x >= m.Dst && x < m.Dst + m.Length
    let mapForward (x: int64) (m: Mapping) = x - m.Src + m.Dst
    let mapBack (x: int64) (m: Mapping) = x + m.Src - m.Dst
        
    let merge (ms1: Mapping list) (ms2: Mapping list) =
        let map ((start, stop): int64 * int64) =
            let m1 = ms1 |> List.filter (matchesDst start) |> List.tryHead |> Option.defaultValue empty
            let m2 = ms2 |> List.filter (matchesSrc start) |> List.tryHead |> Option.defaultValue empty
            { Src = mapBack start m1; Length = stop-start; Dst = mapForward start m2 }
            
        let ms1Limits = ms1 |> List.fold (fun ls m -> ls @ [m.Dst; m.Dst + m.Length]) List.empty
        let ms2Limits = ms2 |> List.fold (fun ls m -> ls @ [m.Src; m.Src + m.Length]) List.empty
        ms1Limits @ ms2Limits
        |> List.sort
        |> List.pairwise
        |> List.filter (fun (a,b) -> a <> b)
        |> List.map map
        
    let apply (mappings: Mapping list) (x: int64) =
        match List.filter (matchesSrc x) mappings with
        | [] -> x
        | [m] -> mapForward x m
        | ms -> failwithf $"multiple matches: %A{ms}"

module Parser =
    let intList = sepBy pint64 (pstring " ")
    let word = manySatisfy (fun c -> c <> '-' && c <> ' ')
    let mapLine =  (word .>> pstring "-to-") .>>. (word .>> pstring " map:" .>> spaces)
    let mapping = pipe3 (pint64 .>> skipChar ' ') (pint64 .>> skipChar ' ') pint64 (fun d s l -> {Src = s; Dst = d; Length = l })
    let map = mapLine >>. many (mapping .>> spaces)
    let seeds = pstring "seeds: " >>. intList .>> spaces
    let parser = seeds .>>. (many (map .>> spaces))

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let part1 (s: string) =
    let seeds, mappings = s |> Parser.parse
    let map = mappings |> List.reduce Mapping.merge
    seeds |> List.map (Mapping.apply map) |> List.min
    
let rec inSeeds (x: int64) (seeds: int64 list) =
    match seeds with
    | s::l::rest ->
        if x >= s && x < s + l then true else inSeeds x rest
    | [] -> false
    | _ -> failwith "wtf"

let part2 (s: string) =
    let seeds, mappings = s |> Parser.parse
    let map = mappings |> List.reduce Mapping.merge
    
    map
    |> List.filter (fun m -> inSeeds m.Src seeds)
    |> List.map (fun m -> m.Dst)
    |> List.min