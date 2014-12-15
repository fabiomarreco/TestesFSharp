#load "Visualize.fs";;
open System;;
open System.Text;;
open System.Collections.Generic;;

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
             let res = f x
             cache := (!cache).Add(x,res)
             res;;

let rec binomial =
    let b (n, r) = 
        match r with
        | r when (r = 0 || r = n)  -> 1
        | r -> binomial(n - 1, r) + binomial(n - 1, r - 1)
    memoize b ;;

let Price r n rd ru S K =
    let D = 1. + rd
    let U = 1. + ru
    let p = ((1. + r) - D) / (U - D)
    let payoff (j:int) (n:int) = max (S * (U ** (float j)) * (D ** (float (n-j))) - K) 0.
    //let k0 = (int (ceil ( -1. * ((float n) * log (D) - log ( S / K)) / log (U / D))))
    Seq.initInfinite (
        fun (j:int) ->
            (float (binomial (n, j))) * (p ** (float j)) * ((1.-p) ** (float (n-j))) * (payoff j n))
        |> Seq.take n 
        //|> Seq.skip k0
        |> Seq.sum;;

let funK n = [0..260] |> Seq.map ( fun k -> ((float k), (Price 0.08 n -0.03 +0.15 40. (float k))));;

funK 10;;
funK 10 |> Visualize.plot;;

        
        
