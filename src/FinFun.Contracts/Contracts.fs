module FinFun.Contracts

open System
open System.Collections.Generic

open FSharpx.Functional
open NodaTime

let (-<) a b = a |> b
let (>-) a b = a <| b    


type Currency =
    | USD
    | EUR
    | GBP


type Date =
    inherit IComparable<Date>
    type DateFactory<'d when 'd :> Date> =
        abstract member time0 : Date


type RV<'a when 'a : comparison> =
    inherit IEquatable<RV<'a>>


type PR<'a when 'a : comparison> =
    inherit IEquatable<PR<'a>>
    abstract member Item : Date -> RV<'a>


// Observables primitives


[<NoEquality;NoComparison>]
type Obs<'a> =
    | Konst of 'a
    | Map of (obj -> 'a) * Obs<obj>
    | Lift2 of (obj -> obj -> 'a) * Obs<obj> * Obs<obj>
    | Date
    | Negate of Obs<'a> * Obs<'a>
    | Plus of Obs<'a> * Obs<'a>
    | Minus of Obs<'a> * Obs<'a>
    | Mult of Obs<'a> * Obs<'a>
    | Div of Obs<'a> * Obs<'a>
    | Eq of Obs<obj> * Obs<obj>
    | Ne of Obs<'a> * Obs<'a>
    | Lt of Obs<'a> * Obs<'a>
    | Le of Obs<'a> * Obs<'a>
    | Gt of Obs<'a> * Obs<'a>
    | Ge of Obs<'a> * Obs<'a>


[<AutoOpen>]
module Obs =
    let konst : 'a -> Obs<'a> = Konst

    let map (f : 'a -> 'b) (x : Obs<'a>) : Obs<'b> = Map (f, x)

    let lift2 (f : 'a -> 'b -> 'c) (x : Obs<'a>) (y : Obs<'b>) : Obs<'c> = Lift2 (f, x, y)

    let date : Obs<'a> = Date

    let (+) (x : Obs<'a>) (y : Obs<'a>) : Obs<'a> = Plus (x, y)

    let (-) (x : Obs<'a>) (y : Obs<'a>) : Obs<'a> = Minus (x, y)

    let (*) (x : Obs<'a>) (y : Obs<'a>) : Obs<'a> = Mult (x, y)

    let (/) (x : Obs<'a>) (y : Obs<'a>) : Obs<'a> = Div (x, y)

    let (==) (x : Obs<'a>) (y : Obs<'a>) : Obs<bool> = Eq (x, y)

    let (<) (x : Obs<'a>) (y : Obs<'a>) : Obs<bool> = Lt (x, y)

    let (<=) (x : Obs<'a>) (y : Obs<'a>) : Obs<bool> = Le (x, y)

    let (>) (x : Obs<'a>) (y : Obs<'a>) : Obs<bool> = Gt (x, y)

    let (>=) (x : Obs<'a>) (y : Obs<'a>) : Obs<bool> = Ge (x, y)


// Contract primitives

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
    let zero : Contract= Zero

    let one : Currency -> Contract = One

    let give : Contract -> Contract = Give

    let and' : Contract -> Contract -> Contract = curry And

    let or' : Contract -> Contract -> Contract = curry Or

    let cond : Obs<bool> -> Contract -> Contract -> Contract = curry3 Cond

    let scale : Obs<float> -> Contract -> Contract = curry Scale

    let when' : Obs<bool> -> Contract -> Contract = curry When

    let anytime : Obs<bool> -> Contract -> Contract = curry Anytime

    let until : Obs<bool> -> Contract -> Contract = curry Until


