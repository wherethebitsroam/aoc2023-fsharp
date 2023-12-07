module Aoc2023.Day07

open FParsec

module Parser =
    let parseCards = many (anyOf "23456789TJQKA")
    let parseLine = parseCards .>> spaces .>>. pint32 .>> spaces
    let parser = many parseLine

    let parse (s: string) =
        match (run parser s) with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg
        
let countsToRank (counts: int list) =
    match counts with
    | 5::_ -> 7 // Five of a kind
    | 4::_ -> 6 // Four of a kind
    | 3::2::_ -> 5 // Full house
    | 3::_ -> 4 // Three of a kind
    | 2::2::_ -> 3 // Two pair
    | 2::_ -> 2 // One pair
    | _ -> 1 // High card
        
let rank (hand: char list) =
    hand |> List.countBy id |> List.map snd |> List.sortDescending |> countsToRank

let rank2 (hand: char list) =
    let jokers = hand |> List.filter (fun c -> c = 'J') |> List.length
    let counts = hand |> List.filter (fun c -> c <> 'J') |> List.countBy id |> List.map snd |> List.sortDescending
    
    // add jokers to the first value
    let counts =
        match counts with
        | h::rest -> h + jokers :: rest
        | [] -> [5] // must be 5 jokers
    
    counts |> countsToRank

let cardValue (card: char) =
    match card with
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "wtf"
    
let cardValue2 (card: char) =
    match card with
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 1 // lowest individual
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "wtf"
    
let handValue (f: char -> int) (hand: char list) =
    hand |> List.rev |> List.indexed |> List.map (fun (i,c) -> (f c) <<< (i*4)) |> List.sum
    
let test = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

let part1 (s: string) =
    s
    |> Parser.parse
    |> List.sortBy (fun h -> (rank (fst h)), (handValue cardValue (fst h)))
    |> List.indexed
    |> List.map (fun (i, (_, bid)) -> bid * (i + 1))
    |> List.sum

let part2 (s: string) =
    s
    |> Parser.parse
    |> List.sortBy (fun h -> (rank2 (fst h)), (handValue cardValue2 (fst h)))
    |> List.indexed
    |> List.map (fun (i, (_, bid)) -> bid * (i + 1))
    |> List.sum

