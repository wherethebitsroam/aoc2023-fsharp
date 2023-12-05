module Aoc2023.Day05

open System.Collections.Generic
open FParsec

type Mapping = {Src: int64; Dst: int64; Length: int64}

module Mapping =
    let srcEnd (m: Mapping) = m.Src + m.Length - 1L
    
    let matchesSrc (x: int64) (m: Mapping) = x >= m.Src && x < m.Src + m.Length
    let matchesDst (x: int64) (m: Mapping) = x >= m.Dst && x < m.Dst + m.Length
    let mapSrc (x: int64) (m: Mapping) = x - m.Src + m.Dst
    
    let diff (m1: Mapping option) (m2: Mapping option) =
        match (m1,m2) with
        | Some m1, Some m2 -> m1.Dst - m1.Src + m2.Dst - m2.Src
        | None, Some m | Some m, None -> m.Dst - m.Src
        | _ -> failwith "wtf"
        
    let getSource (m1: Mapping option) (m2: Mapping option) (start: int64) =
        match (m1,m2) with
        | Some m, _ -> start - m.Dst + m.Src
        | None, Some m -> start
        | _ -> failwith "wtf"
    
    let merge (ms1: Mapping list) (ms2: Mapping list) =
        let ms1Limits = ms1 |> List.fold (fun ls m -> ls @ [m.Dst; m.Dst + m.Length]) List.empty
        let ms2Limits = ms2 |> List.fold (fun ls m -> ls @ [m.Src; m.Src + m.Length]) List.empty
        ms1Limits @ ms2Limits
        |> List.sort
        |> List.pairwise
        |> List.filter (fun (a,b) -> a <> b)
        |> List.map (fun (start,stop) -> (start, stop-start, ms1 |> List.filter (matchesDst start) |> List.tryHead, ms2 |> List.filter (matchesSrc start) |> List.tryHead))
        |> List.map (fun (start,len,m1,m2) ->
            let src = getSource m1 m2 start
            { Src = src; Length = len; Dst = src + (diff m1 m2)})
    

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
        
let applyOne (x: int64) (mappings: Mapping list) =
    match List.filter (Mapping.matchesSrc x) mappings with
    | [] -> x
    | [m] -> Mapping.mapSrc x m
    | ms -> failwithf $"multiple matches: %A{ms}"
        
let applyAll (mappings: Mapping list list) (x: int64) =
    mappings |> List.fold applyOne x
        
let part1 (s: string) =
    let seeds, mappings = s |> Parser.parse
    seeds |> List.map (applyAll mappings) |> List.min
    
let rec inSeeds (x: int64) (seeds: int64 list) =
    match seeds with
    | s::l::rest ->
        if x >= s && x < s + l then true else inSeeds x rest
    | [] -> false
    | _ -> failwith "wtf"

let part2 (s: string) =
    let seeds, mappings = s |> Parser.parse
    let map =
        mappings
        |> List.fold Mapping.merge List.empty
    
    map
    |> List.filter (fun m -> inSeeds m.Src seeds)
    |> List.map (fun m -> m.Dst)
    |> List.min

