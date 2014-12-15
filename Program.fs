module Program
    // Learn more about F# at http://fsharp.net

#light
open System;
open System.Data;
open System.Data.SqlClient;
open Microsoft.FSharp.Math;
open Microsoft.FSharp.Control;
//open Microsoft.FSharp.Math.Experimental;


(*
#I "C:\Marreco\Projetos\TestesFSharp";;
#r "System.Xml.Linq.dll";;
#r "FSharp.PowerPack.Math.Providers.dll";;
#r "FSharp.PowerPack.dll";;
#load "Visualize.fs";;
#load "IOData.fs";;
#load "Calendario.fs";;
#load "Curves.fs";;
#load "Basis.fs";;
#load "Juros.fs";;
#load "Instrumento.fs";;
#load "LTN.fs";;
*)

let resultado = Curves.load_vertex ("CRVBRABMF_RATE_DI1", new DateTime (1999, 01, 01), DateTime.Today);


printfn "%A" resultado;

Console.ReadKey();

(*
let brazil_otc = Calendario.feriados ("CALBRAOTC");
let networkdays = Calendario.networkingdays (brazil_otc) ;;
let hoje = new DateTime (2009, 09, 08);
let networkhoje = networkdays (hoje);
let ndus = resultado |> Seq.map (fun (data, maturity, valor) -> (data, maturity, (networkdays (data))(maturity)));;
let r = ndus |> Seq.take (50) |> Seq.to_array
Visualize.gridview (ndus)
let curvaHoje = resultado 
                |> List.filter ( fun (data, _, _) -> data = new DateTime (2009,2,16)) 
                |> List.map ( fun  (data, vencimento, valor) -> (vencimento , networkhoje (vencimento), valor))

let x = vector [ 1.0;2.0;3.0]

let m = matrix[[1.; 0.; 1. ]
               [0.; 1.; 0.]
               [0.; 0.; 1.]];


let pseudo_inverse (m:matrix) = LinearAlgebra.Inverse(m.Transpose * m) * m.Transpose

//LinearAlgebra.Inverse (m);;
//pseudo_inverse (m);;

//m;;
let t = LinearAlgebra.SolveLinearSystem (m)
let a = t (x);


//Visualize.gridview (resultado);;

*)