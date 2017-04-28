module FinFun.Contracts

open FSharpx.Functional
open NodaTime

let (-<) a b = a |> b
let (>-) a b = a <| b    


type Currency =
    | USD
    | EUR
    | GBP


type Date =
    { instant : Instant
      timeStep : uint32
    }


let date0() : Date =
    { instant  = undefined
      timeStep = 0u
    }


// Random variable

type RV<'a when 'a : comparison> = private RV of 'a[]

[<AutoOpen>]
module RV =
    let map (f : 'a -> 'b) (x:RV<'a>) : RV<'b> =
        match x with | RV a -> RV <| Array.map f a

    let map2 (f : 'a -> 'b -> 'c) (x:RV<'a>) (y:RV<'b>) : RV<'c> =
        match x, y with | RV a, RV b -> RV <| Array.map2 f a b

    let map3 (f : 'a -> 'b -> 'c -> 'd) (x:RV<'a>) (y:RV<'b>) (z:RV<'c>) : RV<'d> =
        match x, y, z with | RV a, RV b, RV c -> RV <| Array.map3 f a b c


// Process

[<NoEquality;NoComparison>]
type PR<'a when 'a : comparison> = private PR of seq<RV<'a>> with
    member this.Item (d:Date) : RV<'a> =
        match this with
        | PR s -> Seq.item (int d.timeStep) s


[<AutoOpen>]
module PR =
    let konst (x:'a) : PR<'a> =
        let rec nextSlice count = seq {
            yield RV <| Array.create count x
            yield! nextSlice (count+1)
        }
        PR <| nextSlice 1

    let dateFrom (d:Date) : PR<Date> =
        let rec timeSlices count date = seq {
            yield RV <| Array.create count date
            yield! timeSlices (count+1) {timeStep = date.timeStep + 1u; instant = date.instant.Plus(Duration.FromDays 1) }
        }
        PR <| timeSlices 1 d

    let date() = dateFrom <| date0()

    let map (f : 'a -> 'b) (x:PR<'a>) : PR<'b> =
        match x with PR y -> PR <| Seq.map (RV.map f) y

    let map2 (f : 'a -> 'b -> 'c) (x:PR<'a>) (y:PR<'b>) : PR<'c> =
        match x, y with PR a, PR b -> PR <| Seq.map2 (RV.map2 f) a b

    let map3 (f : 'a -> 'b -> 'c -> 'd) (x:PR<'a>) (y:PR<'b>) (z:PR<'c>) : PR<'d> =
        match x, y, z with PR a, PR b, PR c -> PR <| Seq.map3 (RV.map3 f) a b c

    let cond (b:PR<bool>) (tru:PR<'a>) (fls:PR<'a>) : PR<'a> =
        map3 (fun i t e -> if i then t else e) b tru fls

    let inline plus  x y = map2 (fun a b -> a +  b) x y
    let inline minus x y = map2 (fun a b -> a -  b) x y
    let inline mult  x y = map2 (fun a b -> a *  b) x y
    let inline div   x y = map2 (fun a b -> a /  b) x y
    let inline eq    x y = map2 (fun a b -> a =  b) x y
    let inline ne    x y = map2 (fun a b -> a <> b) x y
    let inline lt    x y = map2 (fun a b -> a <  b) x y
    let inline le    x y = map2 (fun a b -> a <= b) x y
    let inline gt    x y = map2 (fun a b -> a >  b) x y
    let inline ge    x y = map2 (fun a b -> a >= b) x y


// Observable

[<NoEquality;NoComparison>]
type Obs<'a when 'a : comparison> = private Obs of (Date -> PR<'a>) with
    member this.Item (d:Date) : PR<'a> =
        match this with
        | Obs s -> s d


[<AutoOpen>]
module Obs =
    let konst (x: 'a) : Obs<'a> = Obs (fun _ -> PR.konst x)

    let map (f : 'a -> 'b) (x : Obs<'a>) : Obs<'b> =
        match x with | Obs a -> Obs <| fun d -> PR.map f (a d)

    let map2 (f : 'a -> 'b -> 'c) (x : Obs<'a>) (y : Obs<'b>) : Obs<'c> =
        match x, y with | Obs a, Obs b -> Obs <| fun d -> PR.map2 f (a d) (b d)

    let date : Obs<Date> = Obs <| dateFrom

    let inline plus  x y = map2 (fun a b -> a +  b) x y
    let inline minus x y = map2 (fun a b -> a -  b) x y
    let inline mult  x y = map2 (fun a b -> a *  b) x y
    let inline div   x y = map2 (fun a b -> a /  b) x y
    let inline eq    x y = map2 (fun a b -> a =  b) x y
    let inline ne    x y = map2 (fun a b -> a <> b) x y
    let inline lt    x y = map2 (fun a b -> a <  b) x y
    let inline le    x y = map2 (fun a b -> a <= b) x y
    let inline gt    x y = map2 (fun a b -> a >  b) x y
    let inline ge    x y = map2 (fun a b -> a >= b) x y


// Contract

[<NoEquality;NoComparison>]
type Contract =
    | Zero
    | One  of Currency
    | Give of Contract
    | And  of Contract * Contract
    | Or   of Contract * Contract
    | Cond    of Obs<bool>  * Contract * Contract
    | Scale   of Obs<float> * Contract
    | When    of Obs<bool>  * Contract
    | Anytime of Obs<bool>  * Contract
    | Until   of Obs<bool>  * Contract


[<AutoOpen>]
module Contract =
    let zero : Contract = Zero

    let one  : Currency -> Contract = One

    let give : Contract -> Contract = Give

    let and' : Contract -> Contract -> Contract = curry And

    let or'  : Contract -> Contract -> Contract = curry Or

    let cond    : Obs<bool>  -> Contract -> Contract -> Contract = curry3 Cond

    let scale   : Obs<float> -> Contract -> Contract             = curry Scale

    let when'   : Obs<bool>  -> Contract -> Contract             = curry When

    let anytime : Obs<bool>  -> Contract -> Contract             = curry Anytime

    let until   : Obs<bool>  -> Contract -> Contract             = curry Until


