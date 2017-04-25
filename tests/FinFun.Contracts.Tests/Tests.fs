module FinFun.Contracts.Tests

open FsUnitTyped
open NUnit.Framework

open FinFun.Contracts

[<Test>]
let ``Simple contract combinator test`` () =
    let u = one USD
    let e = one EUR
    let andGive c d = c -<and'>- give d

    let actual = andGive u e
    let expected = And (One USD, Give (One EUR))
    expected |> shouldEqual actual
