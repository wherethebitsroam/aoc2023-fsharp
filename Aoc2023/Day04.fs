module Aoc2023.Day04

open System

type Card = {Num: int; Winning: Set<int>; Yours: Set<int>}

module Card =
    let parse (s: string) =
        let x = s.Split(':', StringSplitOptions.TrimEntries)
        let c = x[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        let y = x[1].Split('|', StringSplitOptions.TrimEntries)
        
        let parseSet (s: string) =
            s.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse |> Set.ofSeq
    
        { Num = Int32.Parse c[1]; Winning =  parseSet y[0]; Yours = parseSet y[1] }
    
    let winners (c: Card) =
        let intersect = Set.intersect c.Winning c.Yours
        intersect.Count
        
    let score (c: Card) =
        (1 <<< winners c) >>> 1
        
let part1 (s: string) =
    s.Trim().Split '\n'
    |> Array.map Card.parse
    |> Array.map Card.score
    |> Array.sum

let folder (m: Map<int,int>) (c: Card) =
    let count = m[c.Num]
    let winners = Card.winners c
   
    let update (existing: Option<int>) =
        let value = existing |> Option.defaultValue 1
        Some (value + count)
    
    let mutable map = m
    for num in (c.Num + 1)..(c.Num+winners) do
        map <- Map.change num update map
    map

let part2 (s: string) =
    let cards = s.Trim().Split '\n' |> Array.map Card.parse
    let initial = cards |> Array.map (fun c -> (c.Num, 1)) |> Map.ofArray
    cards |> Array.fold folder initial |> Map.values |> Seq.sum
    

