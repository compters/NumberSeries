open NumberSeries

[<EntryPoint>]
let main argv =     
    Generate.benchmarkImplementations 100 // 1000000
    // let res' = PickSpecial2 4000.0 160.0 2.4999999999999996 testSeries
    // printfn "%A" res'

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
