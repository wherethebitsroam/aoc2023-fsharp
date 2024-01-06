module Aoc2023.Day25

open FParsec

module Parser =
    let pv = many1Satisfy isLetter 
    let parseLine = pv .>> skipString ": " .>>. sepBy1 pv (skipChar ' ') .>> spaces
    let parser = many1 parseLine

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error

let test = "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"

let connections (ls: (string * string list) list) =
    ls
    |> List.collect (fun (n,cs) -> cs |> List.collect (fun c -> [(n,c);(c,n)]))
    |> List.groupBy fst
    |> List.map (fun (n,cs) -> (n, List.map snd cs))
    |> Map.ofList
    
let partition (cs: Map<string,string list>) =
    // get the number of connections to vertices to the set that are not in the set
    let links (p: Set<string>) =
        p
        |> Set.toList
        |> List.collect (fun v -> cs[v] |> List.filter (fun d -> Set.contains d p |> not) |> List.map (fun d -> (v,d)))
        |> List.length
        
    let rec split (p1: Set<string>) (p2: Set<string>) =
        let l = links p1
        printfn "%A: links: %d" p1 l
        if l = 3 then
            (p1,p2)
        else
            // find the vertex in p2 with the least connections to p2 if added to p1
            let toMove =
                p2
                |> Set.toList
                |> List.map (fun v -> (v, Set.add v p1 |> links))
                |> List.minBy snd
                |> fst
            
            // move that vertex from p2 to p1
            split (Set.add toMove p1) (Set.remove toMove p2)
        
    let p1 = cs.Keys |> Seq.head |> Set.singleton
    let p2 = cs.Keys |> Seq.skip 1 |> Set.ofSeq
    
    split p1 p2

let part1 (s: string) =
    let p1,p2 = s |> Parser.parse |> connections |> partition
    (Set.count p1) * (Set.count p2)

let part2 (s: string) =
    printfn "Aoc2023.Day25.part1"

