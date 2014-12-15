module Fatoracao
#light
let factor num =
    let pseudoprimes =  seq { yield 2
                              yield! Seq.initInfinite (fun n -> 2 * n + 1) |> Seq.skip 1 }
    num 
    |>  Seq.unfold (fun x -> 
                    if x = 1 then None 
                    else 
                         pseudoprimes
                         |> Seq.pick (fun p -> if x % p = 0 then Some(p, x / p) else None) 
                         |> Some)
//
//
//let factor2 num =
//    let pseudoprimes =  seq { yield 2
//                              yield! Seq.initInfinite (fun n -> 2 * n + 1) |> Seq.skip 1 }
//                              
//    let fator primos = 
//    num 
//    |>  Seq.unfold (fun x -> 
//                    if x = 1 then None 
//                    else 
//                         pseudoprimes
//                         |> Seq.pick (fun p -> if x % p = 0 then Some(p, x / p) else None) 
//                         |> Some)
//
////[1. .. 10.] |> Seq.fold (*) 1.;;
//factor 362887007 |> Seq.to_list;;
//
//let num = 362887007
//let n
//pseudoprimes;;
// Seq.initInfinite (fun n -> 2*n + 1) |> Seq.app


[1..20] |> Seq.map (fun x -> (double x, (double x)**2.0)) |> Visualize.gridview
