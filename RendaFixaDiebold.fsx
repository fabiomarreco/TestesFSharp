#light
// Carregamento dos dados 
#r "FSharp.PowerPack.dll";
#load  "IOData.fs"
#load "Statistics.fs";;
#load "Curves.fs";;
#load "Visualize.fs";;
#load "ListExt.fs";;
#load "LinearAlgebra.fs";;
#load "Calendario.fs";;

// Utilizacao de bibiliotecas
open System;
open System.IO;
open System.Xml;
open Microsoft.FSharp.Math;;
open LinearAlgebra;

type Matrix<'a> with
    member this.ColumnsSeq =  
        seq { for i in 0..this.NumCols-1 -> this.Column(i) }
    member this.RowsSeq =  
        seq { for i in 0..this.NumRows-1 -> this.Row(i) }

let delta t = 
    let lambda = 0.1047
    let tau = t / 7.0
    [1.0;(1.0 - exp(-tau * lambda))/(tau * lambda);((1.0 - exp(-tau * lambda))/(tau * lambda)) - exp (- tau * lambda)] |> vector 

(*
let varY t = (delta t).Transpose * Sigma * (delta t)
varY |> ListExt.mapAndZip  [(1.0)..(1000.0)] |> Visualize.plot

let delta t = 
    [ 1.0; (1.0 - exp(-lambda*t)) / (lambda * t); (1.0 - exp (-lambda * t)) / (lambda*t) - exp (-lambda * t) ] |> vector
*)

let Sigma = [[0.25;-0.07;-0.04];
            [-0.07;0.26;0.03];
            [-0.04;0.03;0.04]] |> matrix
 

let varY t = (delta t).Transpose * Sigma * (delta t)
let covY t1 t2 = (delta t1).Transpose * Sigma * (delta t2);

let F (v:list<float>) (t:float) =
    let expoente = if List.isEmpty (v) then 1.0
                   else (1.0 / (float v.Length)) 
    let ajusteCorrel = 
        if List.isEmpty(v) then 1.0
        else
            v |> List.map (fun t2 -> 
                           (sqrt ((varY t) * (varY t2)) - (covY t t2))** ( expoente))
                           // let correl = (covY t t2) / sqrt ((varY t) * (varY t2))
                            //(1.0-correl) ** ( expoente)) 
                           |> List.fold (*) 1.0
    (varY t) * ajusteCorrel
    //(varY t)
    //((varY t) ** (expoente)) * ajusteCorrel;

varY |> ListExt.mapAndZip  [(1.0)..(1000.0)] |> Visualize.plot

let vertices (N:int) (prazoMinimo:float) (prazoMaximo:float) = 
    let rec prox_vertice (vertices:list<float>)  = 
            let vertice = (fun i -> F vertices i) 
                          |> ListExt.mapAndZip [prazoMinimo..prazoMaximo] 
                          |> Seq.maxBy snd |> fst
            let n_vertices = vertice :: vertices
            if n_vertices.Length = N then n_vertices 
            else prox_vertice n_vertices 
    prox_vertice [] |> List.sort




let plot2 (curva:seq<'a * float>) max n = 
    let absssica = curva |> Seq.map fst |> Seq.toArray
    let ordena = curva |> Seq.map snd |> Seq.toArray
    let form = new Form (Visible = true, TopMost = true, Size = new Drawing.Size (808, 327))
    let surface = new NPlot.Windows.PlotSurface2D(Dock = DockStyle.Fill, Title = (n.ToString() + "o Vértice, Máximo = " + max.ToString() + " dias"))
    form.Controls.Add (surface);
    let lp = new NPlot.LinePlot(ordena, absssica, Pen = new Pen( Color.Blue, 3.0f ), Label = "Funcao")
    do surface.Add (lp) |> ignore
    do surface.Refresh() |> ignore
    do surface.CopyToClipboard() |> ignore

let mostra c = 
    let vs = if c <> 0 then vertices c 1.0 2520.0 else []
    let fs = F vs |> ListExt.mapAndZip [(1.0)..(2520.0)] 
    let max = fs |> Seq.maxBy snd |> fst
    plot2 fs max (c+1)
    
    
mostra 5


let varDie = prazos_vertices |> Seq.map float |> Seq.map varY |> vecto.

c = 1
plot2 (mostra 1 ) 1 1

vertices  10 1.0 1000.0


F
F [51.0;400.0;1.0; 25.0; 10.0; 4.0; 19.0; 14.0;2.0; 23.0;6.0;16.0;2500.0;200.0;84.0] |> ListExt.mapAndZip  [(1.0)..(0.1)..(2500.0)] |> Visualize.plot

