module Aoc2023.Day04

open FParsec

type Card = {Num: int; Winning: Set<int>; Yours: Set<int>}

module Card =
    let create (num: int) (winning: int list) (yours: int list) =
        { Num = num; Winning =  winning |> Set.ofList; Yours = yours |> Set.ofList }
        
    let winners (c: Card) = Set.intersect c.Winning c.Yours |> Set.count
        
    let score (c: Card) = (1 <<< winners c) >>> 1
        
let intList = many (pint32 .>> spaces)
let cardNum = pstring "Card" >>. spaces >>. pint32 .>> pstring ":" .>> spaces
let cardParser = pipe3 cardNum (intList .>> pstring "|" .>> spaces) intList Card.create
let cardsParser = many cardParser

let parse (s: string) =
    match (run cardsParser s) with
    | Success(cards, _, _) -> cards
    | Failure(errorMsg, _, _) -> failwith errorMsg
        
let part1 (s: string) =
    s |> parse |> List.map Card.score |> List.sum

let folder (m: Map<int,int>) (c: Card) =
    let cardCount = m[c.Num]
    let update = Option.map ((+) cardCount)
    let updateMap (map: Map<int,int>) (num: int) = Map.change num update map
    seq { (c.Num + 1)..(c.Num + Card.winners c) } |> Seq.fold updateMap m

let part2 (s: string) =
    let cards = s |> parse
    let initial = cards |> List.map (fun c -> (c.Num, 1)) |> Map.ofList
    cards |> List.fold folder initial |> Map.values |> Seq.sum