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
    |> List.map (fun (n,cs) -> (n, List.map snd cs |> Set.ofList))
    |> Map.ofList
    
let partition (cs: Map<string,Set<string>>) =
    let rec split (p1: Set<string>) (p2: Set<string>) (links: Set<string * string>) =
        if Set.count links = 3 then
            (p1,p2)
        else
            // find the vertex in links with the least connections to p2
            let n,ls =
                links
                |> Set.toList
                |> List.map (fun (_,v) -> (v, cs[v] |> Set.intersect p2))
                |> List.minBy (fun (_,d) -> Set.count d)
            
            // move that vertex from p2 to p1
            let links =
                links
                |> Set.filter (fun (_,d) -> d <> n)
                |> Set.union (ls |> Set.map (fun l -> (n,l)))
            split (Set.add n p1) (Set.remove n p2) links
        
    let p1 = cs.Keys |> Seq.head
    let p2 = cs.Keys |> Seq.skip 1 |> Set.ofSeq
    let links = cs[p1] |> Set.map (fun c -> (p1,c))
    
    split (Set.singleton p1) p2 links

let part1 (s: string) =
    let p1,p2 = s |> Parser.parse |> connections |> partition
    (Set.count p1) * (Set.count p2)

let part2 (s: string) =
    printfn "Aoc2023.Day25.part1"

