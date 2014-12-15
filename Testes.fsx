#light
// Carregamento dos dados 
#r "FSharp.PowerPack.dll";
#load "Statistics.fs";;
#load "Curves.fs";;
#load "Visualize.fs";;
#load "ListExt.fs";;
#load "LinearAlgebra.fs";;
#load "Calendario.fs";;

#r @"c:\gac\alglib.dll"
// Utilizacao de bibiliotecas
open System;
open System.IO;
open System.Xml;
open Microsoft.FSharp.Math;;
open LinearAlgebra;
open Statistics;
open alglib;

open System.Text.RegularExpressions;
open System;
open System.Drawing;
open NPlot;
open System.Net;
open System.Windows.Forms;
open System.Drawing;
open System.IO;
open System.Data;
open System.Windows.Forms;
open NPlot.Windows;
open IOData;

let surf = IOData.rcPhoenix<float * float * float> "Select cast (v.Maturity as int) - cast (v.Date as int), v.ValorDimensao, v.Value from tbVerticeSuperficie v inner join tbDiretorio d on d.ID = v.SurfaceID and d.name = 'PETR4' and v.Date = '2011-06-27'" |> Seq.toList

let delta v t r s k = 
    let d1 = (log (s/k) + (r - v * v / 2.0)) / (v * sqrt (t))
    NormSDist d1

let surfDelta = surf |> Seq.map (fun (t, k, v) -> (t, delta (v/100.0) (t/365.0) 0.1 23.4 k, v)) 
                     |> Seq.groupBy (fun (x, _, _) -> x) |> Seq.toList

let alocacoes = [0.1; 0.25; 0.5; 0.75; 0.9];

surfDelta |> Seq.map (fun (t, l) -> 
                        let grau = 2;
                        let vect d = [0..grau] |> Seq.map (fun i -> d ** (float i)) |> rowvec
                        let X = l |> Seq.map (fun (_, d, _) -> vect d) |> matrix
                        let v = l |> Seq.map (fun (_, _, v) -> v) |> vector
                        let b = (LinearAlgebra.inversem(X.Transpose * X)  * (X.Transpose)) * v
                        let calc x =(vect x) * b
                        alocacoes |> Seq.map calc |> vector)
          |> matrix
          |> Visualize.visualize_matriz
                        


//****************** TESTE DE INTERPOLACAO
let Ys = [|114.25753646; 37.971490574; 38.728271858; 45.198057558; 31.686560334; 45.681931959; 47.961332013; 37.714944411; 41.429949986; 35.609547117; 30.438804163; 32.467387663; 28.191476257; 28.754863917; 29.725951858; 31.601261888; 38.77315118; 32.447721183; 30.987951693; 28.459259747; 51.296425294; 40.380321955; 28.594766797; 40.3467305; 28.836754576; 45.480110701; 50.268976018; 54.758956041; 59.232363328; 63.213743437|];
let Xs = [|2.8903717579; 2.9897142012; 2.9957322736; 3.0855729776; 3.0910424534; 3.1302631665; 3.1570004212; 3.1730412885; 3.1780538303; 3.2188758249; 3.2534704696; 3.258096538; 3.295836866; 3.3279095859; 3.3322045102; 3.363149314; 3.36729583; 3.3914836895; 3.4011973817; 3.4619788539; 3.4657359028; 3.5228248698; 3.5263605246; 3.5801800372; 3.5835189385; 3.6344232683; 3.6858749451; 3.734808386; 3.7841896339; 3.8286413965|]


let valor = 3.6003214271;
let valores = Ys |> Seq.zip Xs |> Seq.toList  
interpola_akima valores 3.6003214271

let x =  (valores |> List.map fst |> List.toArray)
let y =  (valores |> List.map snd |> List.toArray)
let s = ref (new spline1d.spline1dinterpolant())
let s2 = ref (new spline1d.spline1dinterpolant())
let rep = ref (new spline1d.spline1dfitreport())
let info = ref 0
let n = valores.Length

do spline1d.spline1dbuildcubic(x, y, n, 1, +1.0, 1, -1.0, s) |> ignore //4, info, s, rep) |> ignore
do spline1d.spline1dbuildakima(x, y, n, s2) |> ignore //4, info, s, rep) |> ignore

let surface = ListExt.mapAndZip [18.0..0.2..50.0] (fun v -> spline1d.spline1dcalc (s, (log v))) |> Visualize.plot 


let akima = NPlot.LinePlot ();
let vv = ListExt.mapAndZip [18.0..0.2..50.0] (fun v -> spline1d.spline1dcalc (s2, (log v))) ;
akima.AbscissaData <- vv |> Seq.map fst |> Seq.toArray;
akima.OrdinateData <- vv |> Seq.map snd |> Seq.toArray;
akima.Pen <- new Pen( Color.Green, 3.0f );
surface.Add (akima);
surface.Refresh()

//let surface = ListExt.mapAndZip [18.0..0.2..50.0] (fun v -> spline1d.spline1dcalc (s2, (log v))) |> Visualize.plot 
let lp = NPlot.LinePlot ();
lp.AbscissaData <- Xs |> Seq.map exp |> Seq.toArray
lp.OrdinateData <- Ys
lp.Pen <- new Pen( Color.Magenta, 1.0f );
surface.Add (lp);
surface.Refresh()

let pt = NPlot.PointPlot ();
pt.AbscissaData <- Xs |> Seq.map exp |> Seq.toArray
pt.OrdinateData <- Ys
pt.Marker <- new Marker (Marker.MarkerType.Diamond, 10, new Pen( Color.Red, 3.0f ), true)
surface.Add (pt);
surface.Refresh()


let xx = Xs |> Seq.skip 1  |> Seq.map exp  |> vector;
let yy = Ys |> Seq.skip 1  |> vector;
let beta = ((xx.Transpose * xx) ** (-1.0) * xx).Transpose * yy
let alfa = ( xx.Transpose * yy ) / (xx.Transpose * xx)

//let surface = ListExt.mapAndZip [18.0..0.2..50.0] (fun v -> spline1d.spline1dcalc (s2, (log v))) |> Visualize.plot 
let xVals = [|18.0..0.2..50.0|];;
let yVals = xVals |> Seq.map (fun x -> x * beta + alfa) |> Seq.toArray
let lpLin = NPlot.LinePlot ();
lpLin.AbscissaData <- xVals
lpLin.OrdinateData <- yVals
lpLin.Pen <- new Pen( Color.Green, 1.0f );
surface.Add (lpLin);
surface.Refresh()

let xxx = xx |> Seq.map (fun x-> [|1.0; x; x** 2.0|]) |> matrix
let beta2 = (LinearAlgebra.inversem(xxx.Transpose * xxx)  * (xxx.Transpose)) * yy
let xxVals = xVals |> Seq.map (fun x-> [1.0; x; x*x]) |> matrix
let yVals2 = xxVals * beta2
let lpLin2 = NPlot.LinePlot ();
lpLin2.AbscissaData <- xVals
lpLin2.OrdinateData <- yVals2 |> Seq.toArray
lpLin2.Pen <- new Pen( Color.Magenta, 1.0f );
surface.Add (lpLin2);
surface.Refresh()




let xxx = xx |> Seq.map (fun x-> [|1.0; x; x** 2.0; x * x * x|]) |> matrix
let beta2 = (LinearAlgebra.inversem(xxx.Transpose * xxx)  * (xxx.Transpose)) * yy
let xxVals = xVals |> Seq.map (fun x-> [1.0; x; x*x; x*x*x]) |> matrix
let yVals2 = xxVals * beta2
let lpLin2 = NPlot.LinePlot ();
lpLin2.AbscissaData <- xVals
lpLin2.OrdinateData <- yVals2 |> Seq.toArray
lpLin2.Pen <- new Pen( Color.Black, 1.0f );
surface.Add (lpLin2);
surface.Refresh()
//****************** FIM TESTE DE INTERPOLACAO

type Matrix<'a> with
    member this.ColumnsSeq =  
        seq { for i in 0..this.NumCols-1 -> this.Column(i) }
    member this.RowsSeq =  
        seq { for i in 0..this.NumRows-1 -> this.Row(i) }

//Parametros gerais
//let prazos_vertices = [[5..4..21]; [30..10..90]; [120..30..360]; [540..180..2340] ] |> Seq.concat
let prazos_vertices = [[5..4..21]; [30..10..90]; [120..30..360]; [540..180..2340] ] |> Seq.concat
//let prazos_vertices = [|21; 42; 84; 120; 180; 252; 360; 540 |]

let data_fim = DateTime (2010, 08, 03)
let janela = 500

let historico_curva = 
    Curves.load_vertex ("CRVBRABMF_RATE_DI1", DateTime (2000,01,01), data_fim)
    |> Seq.sortBy (fun (d, _) -> - d.ToOADate())
    |> Seq.take janela
    |> Seq.toList
    |> List.rev 
    |> Seq.map (fun (dt, vertices) -> (dt, vertices 
                                           |> Seq.map (fun (inicio, fim, valor) -> 
                                                (float (Calendario.ndu_brasil inicio fim), valor))))
    |> Seq.toList


let calcula_loadings (lambda:float) (tau:float) = 
    [1.0; 
    (1.0 - exp (-tau*lambda))/(tau * lambda); 
    (1.0 - exp(-tau * lambda))/(tau * lambda) - exp(-tau * lambda)] 
    
let lambda = 2.25

let betas = 
    historico_curva 
    |> Seq.sortBy fst
    |> Seq.map (fun (_, curva) ->
                let delta = curva |> Seq.map fst
                            |> Seq.skip 1
                            |> Seq.map (fun p -> calcula_loadings lambda ((float p) / 252.0))
                            |> matrix
                            
                delta 
                let y = curva |> Seq.map snd |> Seq.skip 1 |> vector
                let dd = LinearAlgebra.inversem (delta.Transpose * delta) * delta.Transpose
                dd * y)



calcula_loadings 2.0 ((float 2160) / 252.0)

betas|> Visualize.visualize_matriz
betas|> LinearAlgebra.salva_matriz @"c:\temp\betas.txt"

//-------------------------------------------------------------
let m = 
    [[0.496883115; 0.357973302; 0.412704869; 0.766310915];
     [0.18813864;  0.159971491; 0.639908339; 0.056593808];
     [0.802471456; 0.739744577; 0.918867704; 0.857781224];
     [0.964224948; 0.605208499; 0.958837241; 0.338506608]] |> matrix;

let sym = m |> Matrix.mapi (fun i j v -> if (i > j) then v else m.Item(j,i))

let (eval, evec) = sym |> LinearAlgebra.eigen

evec * (evec * eval)



let result = evec * (eval * evec.Transpose())
