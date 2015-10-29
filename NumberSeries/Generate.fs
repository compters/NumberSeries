namespace NumberSeries

module Generate = 
    open System
    
    // Simple function to round to the nearest Quarter
    let RoundQuarter v = Math.Round(round(v * 4.0) / 4.0, 2)

    // Y is a constant as per the instructions
    let y = 1000.0

    // For edge cases in generating the series
    let NotValidFloat x = Double.IsInfinity x || Double.IsNaN x

    let GenerateSeries x y length =        
        let firstNumber = ((0.5 * x ** 2.0) + (30.0 * x) + 10.0) / 25.0
        let growthRate = (0.02 * y) / 25.0 / firstNumber
        if length < 1 || NotValidFloat firstNumber then [] // Prevent against bad inputs
        else if NotValidFloat growthRate || growthRate = 0.0 then [firstNumber] // Only one possible output
        else                                         
            seq { 
                yield RoundQuarter firstNumber 
                // I initially had Infinity here but for very small values of growthRate and firstNumber
                // the sequence would converge to 0.0 before it was even rounded. This is a nice blunt hammer
                // to guard against that, a more elegant solution is left up to the reader!
                for i in 1.0 .. double(length * 10000) do 
                    yield growthRate * (firstNumber ** double(i))  |> RoundQuarter
            } |> Seq.distinct        // Make distinct before truncating otherwise we could take too few
              |> Seq.truncate length // Ensure that we take up to what we need 
              |> Seq.sort 
              |> Seq.toList
    
    // My initial choice of algorithm, O(n) worst case                                           
    let PickSpecial z = 
        function // incomplete pattern match warning can be ignored, is handled by first case        
            | l when List.length l < 3 -> failwith "Cannot be called on a list smaller than 3 elements"
            | first :: rest as lst ->  let approx = y / z  
                                       let dist x = x - approx |> abs // Point-free (but less readable): abs << ((-) approx)
                                       // I chose a recursive function here as I have control over where
                                       // to terminate recursion (i.e when I find the "best" match.)
                                       // A similar effect could be achieved with mutable variables and a 
                                       // Seq.takeWhile, but wouldn't be as idiomatic
                                       let rec findSpecial lst currentDistance best = 
                                             // Making d lazy means we only have to calculate this once, rather than
                                             // having the calculation in multiple places in the pattern match below,
                                             // and we don't evaluate if there is an empty list. Having to use .Force() 
                                             // is a little ugly though. 
                                             let d = lazy dist (List.head lst)
                                             match lst with
                                                | [] -> best // No more to check, cannot do any better
                                                | x :: _  when d.Force() > currentDistance -> best // List is ordered, so we must be getting further away from our goal - cannot do any better.
                                                | x :: xs when d.Force() < currentDistance -> findSpecial xs (d.Force()) x // This is our new best, record that fact and recurse.
                                                | x :: xs when d.Force() = currentDistance -> max x best // Get the biggest if equal distance, next item in the list must be bigger so terminate.                                                       
                                       let special1 = List.nth lst (List.length lst - 3)
                                       (special1, findSpecial rest (dist first) first)

    // I was talking to a friend who indicated I could approximate the position in the list using logarithms,
    // after some discussion I came up with the following. For comparison this takes approx 0.000919ms per per
    // invocation on my machine, the other approach takes 0.006773ms, so this approach is far quicker. O(1)
    let PickSpecialAlt z growthRate (series : array<float>) =    
        if Array.length series < 3 then failwith "Cannot be called on an array smaller than 3 elements"
        else
            let firstNumber = series.[0]
            let leeway = 2 // Found small errors using 1, increasing to 2 removed all errors
            let approx = y / z

            // Mathematical workings:
            // Given:           approx = growthRate * (firstNumber ^ i)
            // Move growthRate: approx / growthRate = firstNumber ^ i
            let approx' = RoundQuarter (approx / growthRate)
            // approx' = firstNumber ^ i
            // The logarithm is the answer to the question "What do I have to raise firstNumber to, to get approx'"
            // So log(approx', firstNumber) will give us i back (approximately)                                            
            let i' = Math.Log(approx', firstNumber) 

            // Now to get the best of our 4 candidates
            let lowerBound = int(floor i') - leeway
            let upperBound = int(ceil i') + leeway
            let indexRange = [ for i in lowerBound .. upperBound do if i >= 0 && i < Array.length series then yield i ]
            // For the sort below
            // First this compares the differences, if they are equal then it returns the biggest value
            let byDistance (a, b) (a', b') = if b < b' then -1 else if b > b' then 1 else if a' > a then 1 else -1                                
            let special1 = series.[Array.length series - 3]
            let special2 = match indexRange with 
                                | [] -> series.[0]
                                | xs -> xs |> List.map (fun i -> (series.[i], abs(series.[i] - approx)))  // Pair up into (value, difference)
                                           |> List.sortWith byDistance                                    // Sort by distance
                                           |> (fun lst -> fst(lst.[0]))                                   // Take the best      
            (special1, special2)

    let benchmarkImplementations benchmarkCount = 
        let testSeries = GenerateSeries 1.0 5062.5 benchmarkCount
        let sw = new Diagnostics.Stopwatch()
        sw.Start()
        let resultsA = [| for i in 1 .. benchmarkCount -> PickSpecial (float i) testSeries |]
        sw.Stop()
        printfn "A: Took %Ams for %A entries (%Ams per entry)" (sw.ElapsedMilliseconds) benchmarkCount (float(sw.ElapsedMilliseconds) / float(benchmarkCount))
        sw.Reset()
        // Alternate method wants random access so better as an array than a linked list
        let arraySeries = List.toArray testSeries
        sw.Start()
        let resultsB = [| for i in 1 .. benchmarkCount -> PickSpecialAlt (float i) 2.5 arraySeries |]
        sw.Stop()
        printfn "B: Took %Ams %A entries (%Ams per entry)" (sw.ElapsedMilliseconds) benchmarkCount (float(sw.ElapsedMilliseconds) / float(benchmarkCount))
        // Lastly lets check the results actually match!
        let uncurry f (a, b) = f a b
        let brokenResults = Seq.zip resultsA resultsB |> Seq.filter (uncurry (<>)) |> Seq.length
        printfn "Number of mismatching records: %A/%A" brokenResults benchmarkCount

