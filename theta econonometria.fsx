#light
#load @"C:\Utilities\startup.fsx"
#load "Yahoo.fs";;
#load "Statistics.fs";;
open System;
open System.IO;

//Pega historico de precos

let retornos = 
        seq { use sw = new StreamReader (@"c:\temp\dow.txt")
              while not sw.EndOfStream do
                    yield Double.Parse (sw.ReadLine()) }
        |> Seq.pairwise
        |> Seq.map (fun (prev, next) -> log (next/prev));;


//transforma em hisitorico de retornos
let retornos = Yahoo.historico_acao "^DJI" 
               |> Seq.toList 
               |> Seq.map snd
               |> Seq.pairwise 
               |> Seq.map (fun (prev, next) -> log (next/prev));;

//funcao para gerar quantis de uma serie
let quantile serie (percentual:float) = 
    let indice = (int (percentual * (float (Seq.length (serie)-1))))
    serie |> Seq.sort |> Seq.nth indice;;

//funcao para pegar o percentual em relacao a um quantil    
let percentile (serie:seq<float>) (quantile:float) = 
    let len = Seq.length (serie)
    [0..(len-1)] |> Seq.map (fun i -> (float i)/(float (len-1)))
                 |> Seq.zip ( Seq.sort (serie))
                 |> Seq.toList
                 |> (fun s -> Statistics.interpola_linear s quantile)
 
//agrupa uma serie em janelas de tamnaho n
let agrupa_serie n serie = serie |> Seq.zip [0..Seq.length(retornos)] 
                                 |> Seq.groupBy ( fun (idx, _) -> Math.Truncate ((float idx) / (float n))) 
                                 |> Seq.map (fun (_, s) -> s |> Seq.map snd)
                                 

let theta_gev n p = 
    let u = quantile retornos (p)
    let Ku = float (retornos |> agrupa_serie n |> Seq.filter ( fun window -> Seq.min (window) <= u) |> Seq.length)
    let Nu = float (retornos |> Seq.filter (fun cotacao -> cotacao <= u) |> Seq.length)
    Ku / Nu


let theta = [| 0.02; 0.03; 0.04; 0.05; 0.06 |] |> Seq.map (theta_gev 21) |> Seq.average


[0.001; 0.005; 0.01; 0.05] |> Seq.map (fun p -> quantile retornos p)


use sw = new StreamWriter (@"c:\teste.txt")
    retornos |> Seq.iter (fun r -> sw.WriteLine (XmlConvert.ToString (r)));;


[-0.140133;-0.075899;-0.052777;-0.007475] |> Seq.map (fun q-> percentile retornos q);;


[-0.176265677;-0.104013767;-0.078005936;-0.027048861] |> Seq.map (fun q-> percentile retornos q);;

retornos |> Seq.zip (Seq.initInfinite (fun i-> i))|>  Visualize.plot 



Statistics.gera_histograma 30 retornos |> Visualize.plot 