module Aoc2023.Day19

open FParsec

type Part = {x: int; m: int; a: int; s: int}

module Part =
    let fromList (l: int list) =
        match l with
        | [x;m;a;s] -> {x = x; m = m; a = a; s = s}
        | _ -> failwithf "bad part: %A" l
    
    let sum (p: Part) =
        p.x + p.m + p.a + p.s

type Op = Gt | Lt
type Dest = Workflow of string | Accept | Reject

module Dest =
    let fromString (s: string) =
        match s with
        | "A" -> Accept
        | "R" -> Reject
        | _ -> Workflow s
            
type Rule = {attr: char; op: Op; value: int; dest: Dest}

module Rule =
    let fromParse (attr: char) (op: char) (value: int) (dest: string) =
        let op =
            match op with
            | '>' -> Gt
            | '<' -> Lt
            | _ -> failwithf "bad op: %A" op
        let dest = Dest.fromString dest
        { attr = attr; op = op; value = value; dest = dest }
    
    let matches (part: Part) (rule: Rule) =
        let v =
            match rule.attr with
            | 'x' -> part.x
            | 'm' -> part.m
            | 'a' -> part.a
            | 's' -> part.s
            | _ -> failwithf "bad rule attr: %A" rule.attr
        
        match rule.op with
        | Gt -> v > rule.value
        | Lt -> v < rule.value
        
type Workflow = {Name: string; Rules: Rule list; Default: Dest }

module Workflow =
    let formParse (name: string) (rules: Rule list) (def: string) =
        {Name = name; Rules = rules; Default = Dest.fromString def }

module Parser =
    let workflowName = many1Satisfy isLetter .>> skipChar '{'
    let rule = pipe4 (anyOf "xmas") (anyOf "<>") (pint32 .>> skipChar ':') (many1Satisfy isLetter .>> skipChar ',') Rule.fromParse
    let workflow = pipe3 workflowName (many1 (attempt rule)) (many1Satisfy isLetter .>> skipChar '}' .>> spaces) Workflow.formParse
    let pattr = anyOf "xmas" >>. skipChar '=' >>. pint32
    let part = skipChar '{' >>. (sepBy pattr (skipChar ',')) .>> skipChar '}' .>> spaces
    let parser = (many1 workflow) .>> spaces .>>. (many1 part) .>> spaces

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error

let test = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"

let acceptPart (workflows: Map<string,Workflow>) (part: Part) =
    let rec processWorkflow (wf: Workflow) =
        let dest =
            wf.Rules
            |> List.tryPick (fun r -> if Rule.matches part r then Some r.dest else None)
            |> Option.defaultValue wf.Default
            
        match dest with
        | Workflow x -> processWorkflow workflows[x]
        | Accept -> true
        | Reject -> false
    
    processWorkflow workflows["in"]

let part1 (s: string) =
    let workflows, parts = s |> Parser.parse
    let parts = parts |> List.map Part.fromList
    let workflows = workflows |> List.map (fun w -> (w.Name,w)) |> Map.ofList
    
    parts |> List.filter (acceptPart workflows) |> List.sumBy Part.sum
    
type Range = {lo: int; hi: int}

module Range =
    let split (op: Op) (value: int) (r: Range) =
        match op with
        | Gt ->
            if r.lo > value then
                (Some r, None)
            elif r.hi <= value then
                (None, Some r)
            else
                (Some {lo = value+1; hi = r.hi}, Some { lo = r.lo; hi = value })
        | Lt ->
            if r.hi < value then
                (Some r, None)
            elif r.lo >= value then
                (None, Some r)
            else
                (Some {lo = r.lo; hi = value-1 }, Some { lo = value; hi = r.hi })
    
    let size (r: Range) =
        int64 r.hi - int64 r.lo + 1L

type PartRange = {x: Range; m: Range; a: Range; s: Range}

module PartRange =
    let size (pr: PartRange) =
        [Range.size pr.x; Range.size pr.m; Range.size pr.a; Range.size pr.s]
        |> List.reduce (*)

let partRanges (workflows: Map<string,Workflow>) =
    // returns (match range, match dest, non-match range)
    let processRule (r: Rule) (pr: PartRange) =
        match r.attr with
        | 'x' ->
            let succ, fail = Range.split r.op r.value pr.x
            (succ |> Option.map (fun r -> {pr with x = r}), r.dest, fail |> Option.map (fun r -> {pr with x = r}))
        | 'm' ->
            let succ, fail = Range.split r.op r.value pr.m
            (succ |> Option.map (fun r -> {pr with m = r}), r.dest, fail |> Option.map (fun r -> {pr with m = r}))
        | 'a' ->
            let succ, fail = Range.split r.op r.value pr.a
            (succ |> Option.map (fun r -> {pr with a = r}), r.dest, fail |> Option.map (fun r -> {pr with a = r}))
        | 's' ->
            let succ, fail = Range.split r.op r.value pr.s
            (succ |> Option.map (fun r -> {pr with s = r}), r.dest, fail |> Option.map (fun r -> {pr with s = r}))
        | _ -> failwithf "bad rule attr: %A" r.attr
        
    let rec processRules (rules: Rule list) (def: Dest) (pr: PartRange) =
        match rules with
        | r::rs ->
            let succ, dest, fail = processRule r pr
            
            let succ =
                match succ with
                | None -> []
                | Some pr ->
                    match dest with
                    | Workflow x -> processWorkflow workflows[x] pr
                    | Accept -> [pr]
                    | Reject -> []
                    
            let fail =    
                match fail with
                | None -> []
                | Some pr -> processRules rs def pr
            
            succ @ fail
            
        | [] ->
            match def with
            | Workflow x -> processWorkflow workflows[x] pr
            | Accept -> [pr]
            | Reject -> []
        
    and processWorkflow (wf: Workflow) (pr: PartRange) =
        processRules wf.Rules wf.Default pr
        
    processWorkflow workflows["in"] {
        x = {lo = 1; hi = 4000}
        m = {lo = 1; hi = 4000}
        a = {lo = 1; hi = 4000}
        s = {lo = 1; hi = 4000}
    }

let part2 (s: string) =
    let workflows, _ = s |> Parser.parse
    let workflows = workflows |> List.map (fun w -> (w.Name,w)) |> Map.ofList
    let prs = workflows |> partRanges
    prs |> List.sumBy PartRange.size
