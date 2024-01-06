module Aoc2023.Day24

open FParsec

type Point = {x:float; y: float; z: float}
module Point =
    let create x y z = {x=x;y=y;z=z}
    
    let str p = $"%.0f{p.x}, %.0f{p.y}, %.0f{p.z}"

type Hailstone = {p: Point; v: Point}
module Hailstone =
    let create p v = {p = p; v = v}
    
    let isPositiveT (x: float) (h: Hailstone) =
        // t > 0
        // t = (x - h.p.x)/h.v.x
        (x - h.p.x)/h.v.x > 0
    
    let path (h: Hailstone) =
        // turn vector into (a,b) from the linear path y = ax + b
        // x = h.p.x + t*h.v.x
        // t = (x - h.p.x)/h.v.x
        // y = h.p.y + t*h.v.y
        // t = (y - h.p.y)/h.v.y
        // (y - h.p.y)/h.v.y = (x - h.p.x)/h.v.x
        // y = h.v.y*(x - h.p.x)/h.v.x + h.p.y
        //    = (h.v.y/h.v.x)*x + h.p.y - h.v.y*h.p.x/h.v.x
        (h.v.y/h.v.x, h.p.y - h.p.x*h.v.y/h.v.x)
    
    let str h = $"%s{Point.str h.p} @ %s{Point.str h.v}"
    
    let intersectXY (h1: Hailstone) (h2: Hailstone) =
        let a1, b1 = path h1
        let a2, b2 = path h2
        if a1 = a2 then
            // parallel
            None
        else
            // a1x + b1 = a2x + b2
            let x = (b2 - b1)/(a1 - a2)
            Some (x, a1*x + b1)

module Parser =
    let pnum = pfloat .>> skipChar ',' .>> spaces
    let parseXYZ = pipe3 pnum pnum pfloat Point.create
    let parseLine = pipe2 (parseXYZ .>> spaces .>> skipChar '@' .>> spaces) (parseXYZ .>> spaces) Hailstone.create
    let parser = many1 parseLine

    let parse (s: string) =
        match run parser s with
        | Success (result,_,_) -> result
        | Failure (error,_,_) -> failwith error

let test = "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"

let rec pairs<'a> (ls: 'a list) =
    match ls with
    | [] -> []
    | l::ls -> (List.map (fun l2 -> (l,l2)) ls) @ pairs ls 

let part1 (s: string) =
    // let low = 7.0
    // let high = 27.0
    // test
    let low = 200000000000000.0
    let high = 400000000000000.0
    s
    |> Parser.parse
    |> pairs
    |> List.filter (fun (h1,h2) ->
        match Hailstone.intersectXY h1 h2 with
        | None ->
            false
        | Some (x,y) ->
            if (Hailstone.isPositiveT x h1) && (Hailstone.isPositiveT x h2) then
                low <= x && x <= high && low <= y && y <= high
            else
                false
        )
    |> List.length
    
let part2 (s: string) =
    // Solving for the stone throw x,y,z @ a,b,c
    // and the first 3 hailstones:
    // 194592040768564, 332365743938486, 196880917504399 @ 160, -81, 182
    // 119269259427296, 151358331038299, 32133087271013 @ 320, 350, 804
    // 137316267565914, 280950442046082, 163349784223749 @ 252, -89, 298
    // Which hits the first hailstone at time t, the 2nd at u and 3rd at v
    //
    // using Wolfram:
    // Solve[194592040768564+160t==x+a t &&
    //   332365743938486-81t==y+b t &&
    //   196880917504399+182t==z+c t &&
    //   119269259427296+320u==x+a u &&
    //   151358331038299+350u==y+b u &&
    //   32133087271013+804u==z+c u &&
    //   137316267565914+252v==x+a v &&
    //   280950442046082-89v==y+b v &&
    //   163349784223749+298v==z+c v, {x,y,z,a,b,c,t,u,v}]
    //
    // {{x->140604613634294,y->224390889669946,z->206098283112689,a->242,b->83,c->168,t->658383257735,u->273530182141,v->328834606838}} 
    
    140604613634294L + 224390889669946L + 206098283112689L

