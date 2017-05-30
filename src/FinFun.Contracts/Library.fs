module FinFun.Contracts.Library

open FSharpx.Functional

open FinFun.Contracts.Core


[<AutoOpen>]
module PR =
    let cond (b:PR<bool>) (tru:PR<'a>) (fls:PR<'a>) : PR<'a> =
        map3 (fun i t e -> if i then t else e) b tru fls

(**
    let date() = dateFrom <| date0()

    let inline (_+ ) x y = map2 (fun a b -> a +  b) x y
    let inline (_- ) x y = map2 (fun a b -> a -  b) x y
    let inline (_* ) x y = map2 (fun a b -> a *  b) x y
    let inline (_/ ) x y = map2 (fun a b -> a /  b) x y
    let inline (_= ) x y = map2 (fun a b -> a =  b) x y
    let inline (_<>) x y = map2 (fun a b -> a <> b) x y
    let inline (_< ) x y = map2 (fun a b -> a <  b) x y
    let inline (_<=) x y = map2 (fun a b -> a <= b) x y
    let inline (_> ) x y = map2 (fun a b -> a >  b) x y
    let inline (_>=) x y = map2 (fun a b -> a >= b) x y

    let inline (.&&) x y = map2 (fun a b -> a && b) x y
    let inline (.||) x y = map2 (fun a b -> a || b) x y

    let inline abs    x = map (fun a -> abs  a) x
    let inline signum x = map (fun a -> sign a) x
    let inline not    x = map (fun a -> not  a) x
**)


[<AutoOpen>]
module Obs =
    let inline (.+ ) x y = map2 (fun a b -> a +  b) x y
    let inline (.- ) x y = map2 (fun a b -> a -  b) x y
    let inline (.* ) x y = map2 (fun a b -> a *  b) x y
    let inline (./ ) x y = map2 (fun a b -> a /  b) x y
    let inline (.= ) x y = map2 (fun a b -> a =  b) x y
    let inline (.<>) x y = map2 (fun a b -> a <> b) x y
    let inline (.< ) x y = map2 (fun a b -> a <  b) x y
    let inline (.<=) x y = map2 (fun a b -> a <= b) x y
    let inline (.> ) x y = map2 (fun a b -> a >  b) x y
    let inline (.>=) x y = map2 (fun a b -> a >= b) x y

    let (.&&) x y = map2 (fun a b -> a && b) x y
    let (.||) x y = map2 (fun a b -> a || b) x y

    let inline abs    x = map (fun a -> abs  a) x
    let inline signum x = map (fun a -> sign a) x
    let inline not    x = map (fun a -> not  a) x

    let at (d:Date) : Obs<bool> = date .= (konst d)

    let between (t1:Date) (t2:Date) : Obs<bool> = (konst t1 .< date) .&& (date .< konst t2)


[<AutoOpen>]
module Contract =
    let andGive (c:Contract) (d:Contract) : Contract = c |> and' <| give d

    let european (t:Date) (u:Contract) : Contract = when' (at t) (u |> or' <| zero)

    let american (t1:Date) (t2:Date) (u:Contract) : Contract = anytime (between t1 t2) u
