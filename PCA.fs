module PCA
#light
//#load "Calendario.fs";
open System;;
let hoje = DateTime (2010, 07, 13);
let historicoCurva = Curves.load_vertex ("CRVBRABMF_RATE_DI1", new DateTime (1999, 01, 01), hoje) 
                     |> Seq.toList 
                     |> List.sortBy (fun (d, _) -> - d.ToOADate())
                     |> Seq.take 500 
                     |> Seq.toList
                     |> List.rev

historicoCurva |> Seq.map fst |> Visualize.visualize_unit

let brazil_otc = Calendario.feriados ("CALBRAOTC");
let networkdays = Calendario.networkingdays (brazil_otc) ;;
let networkhoje = networkdays (hoje);
let historicoNDU = historicoCurva |> Seq.map (fun (data, curve) -> 
                                                          curve |> Seq.map (fun (d, m, v) -> (float (networkdays d m), v)))
                                  |> Seq.toList;;

let prazosHoje = historicoNDU |> List.rev
                              |> Seq.head 
                              |> Seq.map fst 
                              |> Seq.toList;


let historicoCurvaInterpolada = 
    let interpolaCurva prazos curva = 
        prazos |> Seq.map (fun p -> (p, Statistics.interpola_flat_forward (curva |> Seq.toList) p))
    historicoNDU |> Seq.map (fun c -> interpolaCurva prazosHoje (c |> Seq.toList)
                                      |> Seq.map (fun (p, v) -> (p, 100.0 * (Math.Pow (v, 252.0/p)-1.0))))


let valores = historicoCurvaInterpolada //|> Seq.pairwise |> Seq.map (fun (prevc, nextc) ->  nextc |> Seq.zip prevc|> Seq.map (fun (prev, next) -> (fst next, (snd next) - (snd prev)))) // transforma em retorno
                                        |> Seq.map (Seq.map (fun (p, v) -> v)) 
                                        |> matrix;;
// valores |> salva_matrix @"c:\temp\valores.txt" ;;
//let variancas = [for i in 0..(valores.NumCols-1) -> valores.Column i |> Statistics.variance]

let covar = Statistics.covar_matrix valores;
let variance = covar.Diagonal;
let volatility = variance |> Vector.map sqrt;
let correlacao = covar |> Matrix.mapi (fun i j v -> v / sqrt(variance.Item(i) * variance.Item(j)))
    
let (eigenValues, eigenVectors) = LinearAlgebra.seigen covar

eigenValues * (100.0 / (eigenValues |> Seq.sum)) |> Seq.scan (+) 0.0 |> Seq.skip 1 |> Seq.toList
let R = valores.Row 0 |> vector
let z = eigenVectors.Transpose * R 
let b = eigenVectors.Columns (0, 5) 
let novaCurva = b * z.GetSlice (None, Some(4)) |> Seq.zip R |> Seq.toList
novaCurva |> Visualize.plot

b |> Matrix.foldByRow (fun p v -> p + v*v) (Vector.zero b.NumRows) |> Seq.zip3 R [1..R.Length] |> Seq.sortBy (fun (_, _, t) ->t) |> Seq.toList 

(*
eigenValues  |> Matrix.ofVector |> salva_matrix @"c:\temp\eigenvalues.txt" ;;
eigenVectors |> salva_matrix @"c:\temp\eigenvectors.txt" ;;
covar |> salva_matrix @"c:\temp\covar.txt" ;;
covar.Diagonal |> Matrix.ofVector |> salva_matrix @"c:\temp\variance.txt";;
valores |> salva_matrix @"c:\temp\valores.txt";;
correlacao |> salva_matrix @"c:\temp\correlacao.txt";;

*)


let vol2 = volatility * (1.0 / volatility.Norm);
let correl2 = 
    let result = correlacao |> Matrix.map (fun x-> x**2.0)
    let ajuste = result |> Matrix.sum
    result |> Matrix.map (fun x-> x / ajuste)

    
let delta = correl2
            |> Matrix.mapi (fun i j v -> vol2.Item(i) * vol2.Item(j) / v)


let (eigenValues, LinearAlgebra.eigenVectors) = LinearAlgebra.seigen delta
eigenValues  |> Matrix.ofVector |> LinearAlgebra.salva_matriz @"c:\temp\eigenvalues.txt" ;;
eigenVectors |> LinearAlgebra.salva_matriz @"c:\temp\eigenvectors.txt" ;;


let v = vector [1.0; 2.0; 3.0];;

valores * (100.0 / (valores |> Seq.sum)) |> Seq.scan (+) 0.0 |> Seq.skip 1 |> Seq.toList
eigenValues |> LinearAlgebra.salva_matriz @"c:\temp\valores.txt";;


historicoCurvaInterpolada |> Seq.toList |> List.rev |> Seq.take 10

valores  |> Visualize.visualize_matriz


historicoCurva |> Seq.map fst |> Seq.mapi (fun i d -> (i, d)) |> Visualize.gridview