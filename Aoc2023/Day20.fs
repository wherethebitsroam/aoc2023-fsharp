module Aoc2023.Day20

open FParsec

type OnOff = On | Off
type PulseKind = Low | High
type ModuleKind = FlipFlop of OnOff | Conjunction of Map<string,PulseKind> | Broadcast
type Module = {kind: ModuleKind; name: string; dsts: string list}

module Module =
    let fromParse (c: char option) (s: string) (dest: string list) =
        let kind = 
            match c with
            | None -> Broadcast
            | Some '%' -> FlipFlop Off
            | Some '&' -> Conjunction Map.empty
            | Some x -> failwithf "bad prefix: %A" x
        {kind = kind; name = s; dsts = dest}
        
    let mkMap (ms: Module list) =
        ms
        // set conjunction inputs
        |> List.map (fun m ->
            match m.kind with
            | Conjunction _ ->
                let inputs =
                    ms
                    |> List.filter (fun i -> List.contains m.name i.dsts)
                    |> List.map (fun i -> (i.name, Low))
                    |> Map.ofList
                {m with kind = Conjunction inputs}
            | _ -> m
            )
        |> List.map (fun m -> (m.name,m))
        |> Map.ofList
        
    let inputs (modules: Map<string,Module>) =
        modules.Values
        |> Seq.collect (fun m -> m.dsts |> List.map (fun dst -> (dst,m)))
        |> Seq.groupBy fst
        |> Seq.map (fun (name,blah) -> (name, blah |> Seq.map snd |> List.ofSeq))
        |> Map.ofSeq

module Parser =
    let parseDests = sepBy (many1Satisfy isLetter) (skipString ", ") .>> spaces
    let parseModule = pipe3 (opt (anyOf "%&")) (many1Satisfy isLetter .>> skipString " -> ") parseDests Module.fromParse
    let parser = many1 parseModule

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let test = "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"

let test2 = "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
"

type Pulse = {src: string; dst: string; kind: PulseKind}
type Stats = {High: int; Low: int}

module Stats =
    let empty = {High = 0; Low = 0 }
    let add (pulses: Pulse list) (stats: Stats) =
        let low, high = pulses |> List.partition (fun p -> p.kind = Low)
        { Low = stats.Low + List.length low; High = stats.High + List.length high }

let button (modules: Map<string,Module>) =
    let updateKind (m: Module) (kind: ModuleKind) (modules: Map<string,Module>) =
        Map.add m.name {m with kind = kind} modules
        
    let mkPulses (m: Module) (kind: PulseKind) =
        m.dsts |> List.map (fun dst -> {src = m.name; dst = dst; kind = kind })
    
    let rec run (modules: Map<string,Module>) (pulses: Pulse list) (allPulses: Pulse list) =
        match pulses with
        | p::ps ->
            let allPulses = allPulses @ [p]
            match Map.tryFind p.dst modules with
            | None ->
                // destination is not a module
                run modules ps allPulses
            | Some m ->
                match m.kind with
                | Broadcast ->
                    run modules (ps @ mkPulses m p.kind) allPulses
                | FlipFlop state ->
                    match p.kind with
                    | High ->
                        // do nothing
                        run modules ps allPulses
                    | Low ->
                        let pulse, state =
                            match state with
                            | Off -> High, On
                            | On -> Low, Off
                        run (updateKind m (FlipFlop state) modules) (ps @ mkPulses m pulse) allPulses
                | Conjunction inputs ->
                    let inputs = Map.add p.src p.kind inputs
                    let pulse =
                        if inputs |> Map.values |> Seq.forall (fun k -> k = High) then
                            Low
                        else
                            High
                    run (updateKind m (Conjunction inputs) modules) (ps @ mkPulses m pulse) allPulses
        | [] ->
            (modules, allPulses)
            
    run modules [{src = "button"; dst = "broadcaster"; kind = Low}] List.empty

let pressButton (times: int64) (modules: Map<string,Module>) =
    let rec press (count: int64) (modules: Map<string,Module>) (stats: Stats) =
        if (times = 0 || count < times) then
            let modules, pulses = button modules
            let count = count + 1L
            if count % 100000L = 0L then
                printfn "count: %d" count
            pulses
            |> List.iter (fun p ->
                match p.src, p.kind with
                | "sn", High -> printfn "sn -High-> %d" count
                | "vq", High -> printfn "vq -High-> %d" count
                | "sr", High -> printfn "sr -High-> %d" count
                | "rf", High -> printfn "rf -High-> %d" count
                | _ -> ()
                )
            press count modules (Stats.add pulses stats)
        else
            stats
    
    press 0 modules Stats.empty
    
let part1 (s: string) =
    let modules = s |> Parser.parse |> Module.mkMap
    let stats = modules |> pressButton 1000
    stats.High * stats.Low
    
let dot (modules: Map<string,Module>) =
    printfn "digraph g {"
    modules |> Map.iter (fun k v -> v.dsts |> List.iter (printfn "%s -> %s;" k))
    printfn "}"
    
let rec gcd a b =
    match (a,b) with
    | x,y when x = y -> x
    | x,y when x > y -> gcd (x-y) y
    | x,y -> gcd x (y-x)

let lcm a b = a*b/(gcd a b)
    
let part2 (s: string) =
    // rx is the only pure output
    // let modules = s |> Parser.parse |> Module.mkMap
    // pressButton 0 modules
    
    // rx gets a low when these are high:
    // vq -High-> 3917
    // sr -High-> 3923
    // sn -High-> 3967
    // rf -High-> 4021
    
    lcm (lcm 3917L 3923L) (lcm 3967L 4021L)

