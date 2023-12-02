module Aoc2023.Day02

open System

type Color = Red | Green | Blue
type Game = {Number: int; Sets: Map<Color,int> array}

let parseGame (s: string) =
    let x = s.Split(" ")
    x[1] |> Int32.Parse
    
let parseColorCount (s: string) =
    let x = s.Split(" ")
    let count = x[0] |> Int32.Parse
    let color =
        match x[1] with
        | "red" -> Red
        | "blue" -> Blue
        | "green" -> Green
        | _ -> failwith "Bad color"
        
    (color, count)
   
let parseSet (s: string) = s.Split(", ") |> Array.map parseColorCount |> Map.ofArray
    
let parseSets (s: string) = s.Split("; ") |> Array.map parseSet
    
let parseRow (s: string) =
    let x = s.Split(": ")
    let game = x[0] |> parseGame
    let sets = x[1] |> parseSets
    { Number = game;  Sets = sets }

let getCount (c: Color) (m: Map<Color, int>) =
    m |> Map.tryFind c |> Option.defaultValue 0

let setPossible (m: Map<Color, int>) =
    getCount Red m <= 12 && getCount Blue m <= 14 && getCount Green m <= 13
    
let gamePossible (g: Game) =
    g.Sets |> Array.forall setPossible

let part1 (s: string) =
    s.Trim().Split '\n'
    |> Seq.map parseRow
    |> Seq.filter gamePossible
    |> Seq.map (fun g -> g.Number)
    |> Seq.sum
    
let maxColor (c: Color) (ms: Map<Color, int> array) =
    ms |> Array.map (getCount c) |> Array.max
    
let powerSet (g: Game) =
    maxColor Red g.Sets * maxColor Blue g.Sets * maxColor Green g.Sets

let part2 (s: string) =
    s.Trim().Split '\n'
    |> Seq.map parseRow
    |> Seq.map powerSet
    |> Seq.sum

