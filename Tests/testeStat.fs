#load "Statistics.fs";;

let prob n p = 
    let bin (n, k) = async { return (Statistics.binomial (n, k)) }
    [1..n] 
    |> List.rev
    |> Seq.map (fun k ->
                async { let! b = bin (n, k)
                        printfn "Indice: %A: %A" k b
                        return (float b) * ( p ** (float k)) * ( (1.-p) ** (float (n-k))) } )
    |> Async.Parallel 
    |> Async.RunSynchronously
    |> Seq.sum;;


Statistics.binomial (90, 1);;