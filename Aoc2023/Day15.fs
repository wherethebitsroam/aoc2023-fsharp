module Aoc2023.Day15

open FParsec

let parseWith (parser: Parser<'a,_>) (s: string) =
    match run parser s with
    | Success (result,_,_) -> result
    | Failure (error,_,_) -> failwith error

type Op = Remove | Add of int

module Parser =
    let parser = sepBy1 (many1 (noneOf ",\n")) (skipChar ',') .>> spaces
        
    let mkOp (c: char) (v: int option) =
        match c with
        | '-' -> Op.Remove
        | '=' ->
            match v with
            | Some v -> Op.Add v
            | None -> failwith "= without number"
        | x -> failwithf $"invalid op: %c{x}"
        
    let inst = pipe3 (many1 (noneOf "=-")) (anyOf "-=") (opt pint32) (fun a b c -> (a,mkOp b c))
    let parser2 = sepBy1 inst (skipChar ',') .>> spaces

let test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
"

let hash (s: char list) = s |> List.fold (fun acc c -> ((acc + (int c))*17)%256) 0

let part1 (s: string) = s |> parseWith Parser.parser |> List.map hash |> List.sum
    
type Lens = {Label: char list; Value: int}

let processInst (boxes: Lens list array) ((label,op): char list * Op) =
    let box = hash label
    match op with
    | Remove ->
        boxes[box] <- boxes[box] |> List.filter (fun l -> l.Label <> label)
    | Add v ->
        let lens = {Label = label; Value = v }
        match boxes[box] |> List.tryFindIndex (fun l -> l.Label = label) with
        | Some i ->
            // update the box
            boxes[box] <- boxes[box] |> List.updateAt i lens
        | None ->
            // add the box
            boxes[box] <- boxes[box] @ [lens]
    boxes
    
let boxValue (box: Lens list) = box |> List.indexed |> List.sumBy (fun (i,l) -> (i+1)*l.Value)
        
let part2 (s: string) =
    s
    |> parseWith Parser.parser2
    |> List.fold processInst (Array.create 256 [])
    |> Array.indexed
    |> Array.sumBy (fun (b, box) -> (b+1)*(boxValue box))
