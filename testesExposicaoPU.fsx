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

let inicio = DateTime (2000,01,01)
let fim = DateTime (2011,10,25)

let vUSD_DDI = IOData.rcPhoenix<DateTime * DateTime * float> 
                    @"select vUSD.Date as Data, vDDI.Maturity as Maturity, vUSD.Value / vDDI.Value as S_DDI
                        from tbDiretorio dUSD 
                        inner join tbVerticeCurva vUSD on vUSD.CurveID = dUSD.ID
                        inner join tbVerticeCurva vDDI on vDDI.Date = vUSD.Date
                        where dUSD.Code = 'CRVBRAOTC_PRICE_BRL/USD'
                        and vUSD.Date = vUSD.Maturity
                        and vDDI.CurveID = (select id from tbDiretorio where code = 'CRVBRABMF_RATE_DDI')
                        order by 1, 2"
                |> Seq.groupBy (fun (d, _, _) -> d)
                |> Seq.cache                         
                |> Seq.toList

let DI =  (Curves.load_vertex ("CRVBRABMF_RATE_DI1", inicio, fim)) |> Map.ofSeq
let idCurvaNova = IOData.rcPhoenix<int> "select id from tbDiretorio where code = 'CRVBRAOTC_PRICE_BRL/USD_TESTE_FUT'" |> Seq.nth 0



let interpolaDI (dataCurva:DateTime) (di:seq<DateTime * DateTime * float>) (maturity:DateTime) = 
        let periodo m = float (Calendario.ndu_brasil dataCurva m)
        let diPeriodo = di |> Seq.map (fun (_, m, v) -> (periodo m, v)) 
                           |> Seq.toList 
                           |>  Statistics.interpola_flat_forward 
        maturity |> periodo |> diPeriodo



let usd = vUSD_DDI 
          |> Seq.filter (fun (d, _) -> DI.ContainsKey (d))
          |> Seq.map (fun (d, v) -> (d, v, DI.Item(d)))
          |> Seq.map (fun (d, ddi, di) -> 
                        let interpolador = interpolaDI d di
                        (d, ddi 
                            |> Seq.map (fun (d, m, dol) -> (d, m, dol * (interpolador m)))))


let sqls = usd |> Seq.map snd |> Seq.concat
               |> Seq.map (fun (d, m, v) -> 
                    String.Format ("insert into tbVerticeCurva (CurveID, Date, Maturity, Value) Values ({0}, '{1:yyyy-MM-dd}', '{2:yyyy-MM-dd}', {3});", idCurvaNova, d, m, v))


System.IO.File.WriteAllLines (@"c:\temp\curvaDOLAR.sql", (sqls |> Seq.toArray));;