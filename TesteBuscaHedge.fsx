#light
#r "FSharp.PowerPack.dll";
#load  "IOData.fs"
#load "Statistics.fs";;
#load "Curves.fs";;
#load "Visualize.fs";;
#load "ListExt.fs";;
#load "LinearAlgebra.fs";;
#load "Calendario.fs";;

(*
let correl = [[1.0;0.3;0.7];
              [0.3;1.0; 0.4];
              [0.7;0.4;1.0]] |> matrix;

let volats = [0.02; 0.3; 0.43] |> vector;

let covar = 
    let v2 = Matrix.initDiagonal volats
    v2 * correl * v2;
let w = [100.0;250.0;300.0] |> vector;

let E = [[30.0;10.0];
         [0.0;0.0];
         [1.0;1.0]] |> matrix;

    *)


let covar = LinearAlgebra.carrega_matriz @"c:\temp\covar.txt";
let E = LinearAlgebra.carrega_matriz @"c:\temp\E.txt";
let w = LinearAlgebra.carrega_matriz @"c:\temp\w.txt" |> Matrix.toVector;
let p = [1.81595400000000007;-7.37999999999999989;-0.0800000000000000017;-1.01000000000000001] |> vector
    
let varAtual = w.Transpose * covar * w |> sqrt |> (*) 1.644853627;

let varNovo xi =
    let w2 = w + E * xi;
    w2.Transpose * covar * w2 |> sqrt |> (*) 1.644853627;

let xMin = - ((LinearAlgebra.inversem (E.Transpose * covar * E)) * E.Transpose) * covar * w;
let varMin = 
    varNovo xMin
             
             
let alvo = 3326503592566.17871

let A = E * (LinearAlgebra.inversem (E.Transpose * covar * E));
let B = E.Transpose * covar;
let C = (Matrix.identity (covar.Dimensions |> fst) - A * B) * w;
let D = A * p ;

let (lambda1, lambda2) = 
    let a = C.Transpose * covar * C - alvo
    let b = C.Transpose * covar * D
    let c = (1.0/4.0) * (D.Transpose * covar * D)
    let delta = b*b - 4.0 * a * c;
    ((-b + sqrt (delta)) / (2.0 * a), (-b + sqrt (delta)) / (2.0 * a))

let x = 
    let l = lambda1
    let einv = E.Transpose * covar * E |> LinearAlgebra.inversem
    let p2 = p * (0.5 / l)
    einv * ( p2 - E.Transpose * covar * w)
    

varNovo x
alvo |> sqrt |> (*) 1.644853627 |> (fun a -> a - varNovo x )
