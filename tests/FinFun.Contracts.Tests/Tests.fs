module FinFun.Contracts.Tests

open FsCheck.NUnit
open FsUnitTyped
open NUnit.Framework

open FinFun.Contracts.Core
open FinFun.Contracts.Library

[<Test>]
[<Ignore("Contract doesn't implement equality")>]
let ``Simple contract combinator test`` () =
    let u = one USD
    let e = one EUR

    let actual = andGive u e
    let expected = And (One USD, Give (One EUR))
    expected |> shouldEqual actual

[<FsCheck.NUnit.Property>]
let ``hello always returns 42``(x:int) =
  (fun _ -> 22 + 20) x = 42
