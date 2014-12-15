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

//Parametros gerais
//let prazos_vertices = [[5..4..21]; [30..10..90]; [120..30..360]; [540..180..2340] ] |> Seq.concat
let prazos_vertices = [[5; 7; 10; 15; 21; 32; 42; 52; 63];[84..21..252];[378..126..756];[1008..252..2520]]|> Seq.concat
//prazos_vertices |> Visualize.visualize_unit
//let prazos_vertices = [|21; 42; 84; 120; 180; 252; 360; 540 |]

let data_fim = DateTime (2009, 02, 02)
let janela = 772

//Carregamento da curva de DI1
let (datas, valores) = 
    let resultado =
        let data_inicio = DateTime (2000,01,01)
        Curves.load_vertex ("CRVBRABMF_RATE_DI1", data_inicio, data_fim)
        |> Seq.sortBy (fun (d, _) -> - d.ToOADate())
        |> Seq.take janela
        |> Seq.toList
        |> List.rev 
        |> Seq.map (fun (dt, vertices) -> 
                            let vertices_prazo = 
                                vertices 
                                |> Seq.map (fun (inicio, fim, valor) -> 
                                            (float (Calendario.ndu_brasil inicio fim), valor))
                                |> Seq.filter (fun (p,_) -> p > 0.0)
                                |> Seq.toList
                            
                            
                            let interpolado = 
                                let taxas_interp = vertices_prazo 
                                                   |> List.map (fun (p, v) -> (p, 100.0 * ( v ** (252.0 /p) - 1.0)))
                                                   |> Statistics.interpola_akima
                                prazos_vertices |> Seq.map float |> Seq.map taxas_interp
                                (*
                            let interpolado = 
                                prazos_vertices 
                                |> Seq.map (fun p -> 
                                        ((float p)/252.0, Statistics.interpola_flat_forward vertices_prazo (float p)))
                                |> Seq.map (fun (p, v) -> 100.0 * ((v ** (1.0/p))-1.0) )
                               *)
                            (dt, interpolado))
        |> Seq.sortBy fst
        |> Seq.cache
    (resultado |> Seq.map fst, resultado |> Seq.map snd |> matrix)

let retornos = 
        valores.RowsSeq 
        |> Seq.pairwise
        |> Seq.map (fun (prev, next) -> next - prev)
        |> matrix


// Fixamos o valor do lambda
let lambda = 0.1047

/// Calculo dos betas para cada data utilizando OLS (Ordinary Least Square)
let calcula_betas lmbd = 
    // Calculo da matrix com os factor loadings
    let delta = 
        let loading1 p = 1.0
        let loading2 p = (1.0 - exp( - p * lmbd)) / (p * lmbd)
        let loading3 p = loading2(p) - exp (-p * lmbd)
        prazos_vertices 
        |> Seq.map (fun dias -> 
                    let p = (float dias)/252.0
                    [| loading1(p); loading2(p); loading3(p)|])
        |> matrix
    let dd = LinearAlgebra.inversem(delta.Transpose * delta) * delta.Transpose
    let betas = valores.RowsSeq
                |> Seq.map (fun y -> dd * y.Transpose)
                |> matrix
    (delta, betas)

let (deltas, betas) = calcula_betas lambda    


let calcula_correl v = 
    let covar = v |> Statistics.covar_matrix 
    let volatilities = covar.Diagonal |> Vector.map sqrt
    covar |> Matrix.mapi (fun r c v -> v / (volatilities.Item(r) * volatilities.Item(c)))


// O valores dos betas fora passados para o matlab, executando o garch para calculo das variancas
// Os resultados seguem:
//betas |> Visualize.visualize_matriz
(*
  Mean: ARMAX(0,0,0); Variance: GARCH(1,1)
                               Standard          T     
  Parameter       Value          Error       Statistic 
 -----------   -----------   ------------   -----------
           C    15.896         0.014461      1099.2280
           K    0.084385       0.012389         6.8112
    GARCH(1)    0.11157        0.034042         3.2774
     ARCH(1)    0.88796        0.089445         9.9275

(Sigma, mu) = 5.0704   15.8961
---------------------------------------------------------------------------------------------------------------------
Beta2:
                               Standard          T     
  Parameter       Value          Error       Statistic 
 -----------   -----------   ------------   -----------
           C    -0.62436       0.011214       -55.6776
           K    0.037731       0.0047067        8.0165
    GARCH(1)    0.20678        0.040928         5.0523
     ARCH(1)    0.79322        0.07574         10.4729
(Sigma, mu) = 0.4093   -0.6244
---------------------------------------------------------------------------------------------------------------------
Beta3:
                               Standard          T     
  Parameter       Value          Error       Statistic 
 -----------   -----------   ------------   -----------
           C    4.8543         0.036577       132.7132
           K    0.29387        0.056154         5.2333
    GARCH(1)    0.11919        0.036253         3.2876
     ARCH(1)    0.88081        0.079871        11.0279
(Sigma, mu) = 0.9468    4.8543
========================================================================================================
BETA1:
ESTIMACAO PARA RETORNO PRIMEIRA DIFERENCA
                               Standard          T     
  Parameter       Value          Error       Statistic 
 -----------   -----------   ------------   -----------
           C    -0.0075087     0.0046531       -1.6137
           K    0.00074242     9.9526e-005      7.4596
    GARCH(1)    0.90649        0.0020776      436.3166
     ARCH(1)    0.093509       0.0033496       27.9163

ans =

    0.1799   -0.0075

BETA2:
                               Standard          T     
  Parameter       Value          Error       Statistic 
 -----------   -----------   ------------   -----------
           C    0.005265       0.00481          1.0946
           K    0.0006522      0.00010944       5.9595
    GARCH(1)    0.91019        0.0022845      398.4177
     ARCH(1)    0.089811       0.0036863       24.3631

ans =

    0.1870    0.0053

BETA3

                               Standard          T     
  Parameter       Value          Error       Statistic 
 -----------   -----------   ------------   -----------
           C    0.0017289      0.012384         0.1396
           K    0.0098052      0.00073682      13.3074
    GARCH(1)    0.86843        0.004999       173.7201
     ARCH(1)    0.13157        0.0058468       22.5038

ans =

    0.4890    0.0017
*)



//#############################################################

let valores_normalizados = 
    let medias = valores.ColumnsSeq |> Seq.map (fun c -> c |> Seq.average) |> Seq.toList 
    valores |> Matrix.mapi (fun i j v -> v - medias.Item(j))
    
    
let (z, b) = valores |> Statistics.covar_matrix |> LinearAlgebra.seigen |> (fun (l, b) -> (l, b))

z |> Visualize.visualize_unit
b |> Visualize.visualize_matriz


z |> Seq.map (fun x -> 100.0*x/ Seq.sum (z)) |> Seq.scan (+) 0.0 |> Seq.skip 1 |> Visualize.visualize_unit

let ultimo_valor = valores.Row 771 |> RowVector.transpose

b.Transpose * ultimo_valor


medias |> Seq.toList |> Seq.nth 0 |> Seq.toList |> Seq.fold (+) 0.0

medias |> Seq.toList |> Seq.nth 0 |> Seq.toList |> Seq.skip 200 |> Seq.sum

let v = valores
let correl = v |>  calcula_correl
let varianca = (v |> Statistics.covar_matrix).Diagonal

varianca |> Vector.mapi (fun i v -> 
                            let soma = (correl.Row(i)) |> Seq.map (fun x -> x) |> Seq.sum
                            soma)
                            //v* ( 1.0 - soma))
         |> Seq.zip prazos_vertices
         |> Visualize.plot

valores |> calcula_correl |> Visualize.visualize_matriz


b*z

let z1 = z.GetSlice (Some(0), Some(2))
let b1 = b.Columns (0, 3)

b1.RowsSeq |> Seq.map (Seq.fold (fun p n -> p + n*n) 0.0) 
           |> Seq.zip prazos_vertices 
           |> Seq.sortBy snd |> Seq.map fst
           |> Seq.take 10 
           |> Seq.toList


b1.RowsSeq |> Seq.map (Seq.fold (fun p n -> p + n*n) 0.0) 
           |> Seq.zip prazos_vertices 
           |> Seq.sortBy (fun (_, x) -> -x)
           |> Visualize.gridview



b1.Transpose* ultimo_valor


let delta t = 
    let tau = (float t) / 7.0// 252.0
    [1.0;(1.0 - exp(-tau * lambda))/(tau * lambda);((1.0 - exp(-tau * lambda))/(tau * lambda)) - exp (- tau * lambda)] |> vector 


prazos_vertices |> Seq.map (fun t -> ([13.72; -0.72;1.46] |> vector).Transpose * (delta t)) |> Seq.toList

(*
let Sigma = 
    [[0.25;-0.07;-0.04];
    [-0.07;0.26;-0.03];
    [-0.04;-0.03;0.4]] |> matrix
*)

let Sigma = 
    [[0.25;0.0;-0.10];
    [0.0;0.26;0.0];
    [-0.10;0.0;0.4]] |> matrix


let variancas = (valores |> Matrix.map (fun v -> v ) |> Statistics.covar_matrix |> Matrix.map (fun v -> v * 0.1)).Diagonal 

variancas |> Seq.zip (prazos_vertices |> Seq.map float) |> plot2 




let plot3 (curva:seq<'a * float>) texto = 
    let absssica = curva |> Seq.map fst |> Seq.toArray
    let ordena = curva |> Seq.map snd |> Seq.toArray
    let form = new Form (Visible = true, TopMost = true, Size = new Drawing.Size (808, 327))
    let surface = new NPlot.Windows.PlotSurface2D(Dock = DockStyle.Fill, Title = texto)
    form.Controls.Add (surface);
    let lp = new NPlot.LinePlot(ordena, absssica, Pen = new Pen( Color.Blue, 3.0f ), Label = "Funcao")
    do surface.Add (lp) |> ignore
    do surface.Refresh() |> ignore
    do surface.CopyToClipboard() |> ignore


prazos_vertices |> Seq.map (fun x-> (delta x).Transpose * Sigma * (delta x))

valores |> Matrix.map (fun v -> v ) |> Statistics.covar_matrix |> Visualize.visualize_matriz 

prazos_vertices 
    |> Seq.map float
    |> Seq.map (fun t1 -> 
            let cov a b = (delta a).Transpose * Sigma * (delta b)
            prazos_vertices
            |> Seq.map float
            |> Seq.map (fun t2 -> (cov t1 t2) / sqrt ((cov t1 t1) * (cov t2 t2))))
    |> matrix
    |> salva_matriz @"c:\temp\correldiebold.txt"
    |> Visualize.visualize_matriz
    
prazos_vertices 
    |> Seq.map (fun t1 -> 
            let cov a b = (delta a).Transpose * Sigma * (delta b)
            prazos_vertices
            |> Seq.map (fun t2 -> (cov t1 t2) / sqrt ((cov t1 t1) * (cov t2 t2))))
    |> matrix
    |> salva_matriz @"c:\temp\correldiebold.txt";
            


prazos_vertices |> Seq.map float |> vector |> Matrix.ofVector |> LinearAlgebra.salva_matriz @"c:\temp\prazos.txt"

valores |> calcula_correl |> salva_matriz @"c:\temp\correlvalores.txt"
retornos |> calcula_correl |> salva_matriz @"c:\temp\correlretornos.txt"

datas |> Visualize.visualize_unit

valores.RowsSeq |> Seq.take 1

deltas |> Visualize.visualize_matriz
betas |> Visualize.visualize_matriz

valores.Row 167
datas |> Seq.nth 167
prazos_vertices |> Visualize.visualize_unit
valores |> LinearAlgebra.salva_matriz @"c:\temp\valores.txt"
retornos |> LinearAlgebra.salva_matriz @"c:\temp\retornos.txt"

deltas |> LinearAlgebra.salva_matriz @"c:\temp\delta.txt"
betas |> LinearAlgebra.salva_matriz @"c:\temp\betas.txt"
