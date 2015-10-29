module Tests

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open NumberSeries

[<Test>]
let ``Should generate a series in accordance with the spec``() =
    let testSeries = Generate.GenerateSeries 1.0 5062.5 5
    testSeries |> should equal [1.5; 4.0; 6.5; 10.75; 17.25 ]

[<Test>]
let ``Should Pick the special numbers in accordance with the spec``() = 
    let testSeries = Generate.GenerateSeries 1.0 5062.5 5
    let (special1, special2) = Generate.PickSpecial 160.0 testSeries
    special1 |> should equal 6.5
    special2 |> should equal 6.5
    // And test the alternate version too
    let (special1', special2') = Generate.PickSpecialAlt 160.0 2.5 (List.toArray testSeries)
    special1' |> should equal 6.5
    special2' |> should equal 6.5


[<Test>]
let ``Ensure length is always honoured`` () = 
    let matchLen len = let outputLength = List.length (Generate.GenerateSeries 1.0 5062.5 len)
                       if len >= 0 then outputLength = len else outputLength = 0
    // Using FsCheck to generate a variety of inputs
    Check.VerboseThrowOnFailure matchLen

[<Test>]
let ``Ensure there are no duplicates in the series`` () = 
    let testForDuplicates x y len = let series = Generate.GenerateSeries x y len
                                    let deDuped = series |> Seq.distinct |> Seq.toList
                                    List.length deDuped = List.length series
    // Using FsCheck again to ensure the series generator is exposed to the full horror
    // of a variety of inputs.
    Check.QuickThrowOnFailure testForDuplicates

[<Test>]
let ``Ensure the series is in ascending order`` () = 
    let checkOrder x y = let series = Generate.GenerateSeries x y 10
                         // Can't guarantee the length will actually be 10 thanks to NaN and infinty (thanks FsCheck!)                         
                         if series.Length < 2 then true
                                   else series.[0] < series.[series.Length - 1]                         
    Check.QuickThrowOnFailure checkOrder

[<Test>]
let ``RoundQuarter should round to the nearest quarter`` () = 
    Generate.RoundQuarter 10.10 |> should equal 10.00
    Generate.RoundQuarter 10.21 |> should equal 10.25
    Generate.RoundQuarter 10.48 |> should equal 10.50
    Generate.RoundQuarter 10.63 |> should equal 10.75
    Generate.RoundQuarter 10.98 |> should equal 11.00    
    Generate.RoundQuarter 12.12 |> should equal 12.00    
                                                    

