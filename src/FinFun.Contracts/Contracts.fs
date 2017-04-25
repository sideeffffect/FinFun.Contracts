module FinFun.Contracts

open System.Collections.Generic

open FSharpx.Functional
open NodaTime

let (-<) a b = a |> b
let (>-) a b = a <| b    


type Currency =
    | USD
    | EUR
    | GBP


type TimeStep = uint32


type Date =
    { instant : Instant
      timeStep : uint32
    }
    static member from(s) = {instant = Instant.FromUnixTimeSeconds 0L; timeStep = s}


let time0() : Date = Date.from 0u


type RV<'a> = 'a[]


type PR<'a> = { unPr : (RV<'a>)[] } 


type Obs<'a> = Obs of IReadOnlyDictionary<Date, PR<'a>> with
    override this.ToString() =
        match this with
        | Obs f -> let x = f.[time0()]
                   sprintf "(Obs %A)" x.unPr.[0]


type Contract =
    | Zero
    | One  of Currency
    | Give of Contract
    | And  of Contract * Contract
    | Or   of Contract * Contract
    | Cond    of (Obs<bool>)  * Contract * Contract
    | Scale   of (Obs<float>) * Contract
    | When    of (Obs<bool>)  * Contract
    | Anytime of (Obs<bool>)  * Contract
    | Until   of (Obs<bool>)  * Contract


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

