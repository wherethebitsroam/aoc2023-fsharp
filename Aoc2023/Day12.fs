module Aoc2023.Day12

open System
open FParsec

module Parser =
    let nums = sepBy1 pint32 (skipChar ',')
    let row = many1 (anyOf ".#?") .>> spaces .>>. nums .>> spaces
    let parser = many1 row

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error
        
let implode (xs:char list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()
    
let implodeArray (xs:char array) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> Array.iter (sb.Append >> ignore)
    sb.ToString()
        
let explode (s:string) = [for c in s -> c]

let explodeArray (s:string) = [|for c in s -> c|]
        
let test = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"

// Attempt to make all possible masks from the checks
// and match against the pattern -> too slow
module Attempt3 =
    let masks (checks: int list) (len: int) =
        // there needs to be at least 1 space between each group
        // hence the ( - (List.length checks) + 1)
        let spaces = len - (List.sum checks) - (List.length checks) + 1
        
        let rec loop (spaces: int) (checks: int list) =
            match checks with
            | c::rest ->
                seq { 0..spaces }
                |> List.ofSeq
                |> List.collect (fun space ->
                    let checkMask = (List.replicate space false) @ (List.replicate c true)
                    let spaceMask = if List.isEmpty rest then [] else [false]
                    loop (spaces - space) rest
                    |> List.map (fun mask -> checkMask @ spaceMask @ mask)
                    )
            | [] ->
                [List.replicate spaces false]
                
        loop spaces checks
        
    let maskMatches (pat: char list) (mask: bool list) =
        List.zip pat mask
        |> List.forall (fun (c,m) -> c = '?' || if m then c = '#' else c = '.')
        
    let matches (pat: char list) (checks: int list) =
        masks checks (List.length pat)
        |> List.filter (maskMatches pat)
        |> List.length
        

// recursively match the pattern -> too slow
// might have been ok with caching?
module Attempt4 =
    let isPatEnd (pat: char list) =
        match pat with
        | [] -> false
        | x::_ -> x <> '#'
        
    let findMatches (pat: char list) (check: int) =
        let patLen = List.length pat
        if check > patLen then
            []
        else
            let filter (i: int) =
                if i > 0 && (pat |> List.take i |> List.last) = '#' then
                    false
                else
                    let h, t = pat |> List.skip i |> List.splitAt check
                    (List.forall (fun c -> c <> '.') h) && (isPatEnd t)
                
            seq { 0..(patLen-check) }
            |> Seq.filter filter
            |> List.ofSeq
        
    let matches (pat: char list) (checks: int list): int64 =
        let rec matches' (pat: char list) (checks: int list) =
            let patLen = List.length pat
            match checks with
            | c::rest ->
                let indexes =
                    match rest with
                    | [] -> findMatches pat c
                    | rest ->
                        // required remaining space
                        let remain = (List.sum rest) + (List.length rest)
                        if patLen >= (c + 1 + remain) then
                            // find all possible matches for c
                            findMatches (List.take (patLen - remain) pat) c
                        else
                            []
                            
                let indexesToMatches (i: int): int64 =
                    let h, t = List.splitAt i pat
                    if List.contains '#' h then
                        0
                    else 
                        matches' (List.skip (c+1) t) rest
                
                let indexMatches = indexes |> List.map indexesToMatches
                indexMatches |> List.sum
                
            | [] -> if (List.contains '#' pat) then 0 else 1
            
        // add a trailing dot
        matches' (pat @ ['.']) checks


// break the pattern up into groups of [#?]
// run the checks against each group
// __cache the results__
module Attempt5 =
    let mutable cache: Map<string,int64> = Map.empty
    
    let mkKey (groups: string list) (checks: int list) = $"%A{groups}%A{checks}"
    
    let rec getMatches (pat: char array) (checks: int list): int64 =
        let patLen = Array.length pat
        match checks with
        | [] -> if Array.contains '#' pat then 0 else 1
        | c::cs ->
            let maxStart = patLen - (List.length cs) - (List.sum checks)
            let maxStart =
                match Array.tryFindIndex (fun x -> x = '#') pat with
                | None -> maxStart
                | Some x -> min x maxStart
            if maxStart < 0 then
                0
            else
                let mutable matches: int64 = 0
                for i in 0..maxStart do
                    if (i = 0 || pat[i-1] = '?') && (i+c >= patLen || pat[i+c] = '?') then
                        matches <- matches + getMatchesCached pat[i+c+1..] cs
                matches
                
    and getMatchesCached (pat: char array) (checks: int list): int64 =
        let key = mkKey [implodeArray pat] checks
        match Map.tryFind key cache with
        | Some v -> v
        | None ->
            let v = getMatches pat checks
            cache <- Map.add key v cache
            v
            
    let rec getMax (x: int) (checks: int list) =
        match checks with
        | [] -> 0
        | c::cs ->
            if c <= x then
                1 + getMax (x - c - 1) cs
            else
                0
    
    let rec checkGroups (groups: string list) (checks: int list): int64 =
        match groups, checks with
        | [], [] -> 1
        | gs, [] ->
            // groups remaining, but no checks
            // if all remaining groups are only ?'s then were fine
            if gs |> List.forall (fun g -> g.Contains('#') |> not) then
                1
            else
                0
        | [], _ -> 0 // no groups, but checks remaining
        | g::gs, checks ->
            let min = if g.Contains('#') then 1 else 0
            let max = getMax g.Length checks
            let mutable matches: int64 = 0
            for i in min..max do
                let h, t = List.splitAt i checks
                let hMatches = getMatchesCached (explodeArray g) h
                let tMatches = checkGroupsCached gs t
                matches <- matches + (hMatches * tMatches)
                
            matches
            
    and checkGroupsCached (groups: string list) (checks: int list) =
        let key = mkKey groups checks
        match Map.tryFind key cache with
        | Some v -> v
        | None ->
            let v = checkGroups groups checks
            cache <- Map.add key v cache
            v
        
    let matches (pat: char list) (checks: int list) =
        let s = implode pat
        let groups = s.Split('.', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        checkGroups groups checks
        
let part1 (s: string) =
     s
     |> Parser.parse
     |> List.map (fun (pat, checks) ->  Attempt5.matches pat checks)
     |> List.sum

let expand (times: int) ((pat, checks): char list * int list) =
    let newPat = List.replicate times (pat @ ['?']) |> List.collect id
    let newPat = newPat |> List.take (List.length newPat - 1)
    let newChecks = List.replicate times checks |> List.collect id
    (newPat, newChecks)

let part2 (s: string) =
    s
    |> Parser.parse
    |> List.map (expand 5)
    |> List.indexed
    |> List.map (fun (i, (pat, checks)) ->
        printfn "%d" (i+1)
        Attempt5.matches pat checks)
    |> List.sum
    
