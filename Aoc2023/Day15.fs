module Aoc2023.Day15

open FParsec

module Parser =
    let parser = sepBy1 (many1 (noneOf ",\n")) (skipChar ',') .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
    let inst = pipe3 (many1 (noneOf "=-")) (anyOf "-=") (opt pint32) (fun a b c -> (a,b,c))
    let parser2 = sepBy1 inst (skipChar ',') .>> spaces

    let parse2 (s: string) =
        match run parser2 s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        

let test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
"

let hash (s: char list) =
    s |> List.fold (fun acc c -> ((acc + (int c))*17)%256) 0

let part1 (s: string) =
    s
    |> Parser.parse
    |> List.map hash
    |> List.sum
    
type Lens = {Label: char list; Value: int}

let processInst (boxes: Lens list array) ((label,op,v): char list * char * int option) =
    let box = hash label
    match op with
    | '-' ->
        boxes[box] <- boxes[box] |> List.filter (fun l -> l.Label <> label)
    | '=' ->
        match v with
        | Some v ->
            let lens = {Label = label; Value = v }
            match boxes[box] |> List.tryFindIndex (fun l -> l.Label = label) with
            | Some i ->
                // update the box
                boxes[box] <- boxes[box] |> List.updateAt i lens
            | None ->
                // add the box
                boxes[box] <- boxes[box] @ [lens]
        | None -> failwith "wtf"
    | _ -> failwith "wtf"
    boxes
        
let part2 (s: string) =
    let boxes = Array.create 256 []
    let boxes =
        s
        |> Parser.parse2
        |> List.fold processInst boxes
    
    boxes
    |> Array.indexed
    |> Array.fold (fun acc (b, box) ->
        let boxValue =
            box
            |> List.indexed
            |> List.fold (fun acc (slot, lens) -> acc + (slot+1)*lens.Value ) 0
        acc + (b+1)*boxValue
        ) 0

