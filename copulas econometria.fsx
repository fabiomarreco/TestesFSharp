#light
#load @"C:\Utilities\startup.fsx"
#load "Yahoo.fs";;
#load "Statistics.fs";;
#load "ListExt.fs";;
open System;
open System.Xml;
open System.IO;
open System.Text;

let salva_dados arquivo (lista:seq<float>) = 
    use sw = new StreamWriter (arquivo, false, Encoding.Default)
    lista |> Seq.iter(fun x -> sw.WriteLine(XmlConvert.ToString(x)));
        
        

let pega_retornos ticker =
        Yahoo.historico_acao ticker
        |> Seq.sortBy fst
        |> Seq.pairwise
        |> Seq.map (fun (prev, next) -> (fst(next), log ( snd(next) /  snd(prev))))
        |> Seq.toList
        

let retornos = ListExt.merge_lists (pega_retornos "^BVSP") (pega_retornos "^DJI") fst snd
               |> Seq.choose ListExt.chose_merged_tuple 


let x = retornos |> Seq.map ( fun (d, _, _) -> d)

let valores_retornos = retornos |> Seq.map (fun (_,v1, v2) -> (v1, v2))

let copula valores_retornos = 
    let N = (float (retornos |> Seq.length))
    let normalizados =  valores_retornos 
                        |> Seq.sortBy fst
                        |> Seq.mapi (fun i -> fun (v1,v2) -> ((float i)/N, v2))
                        |> Seq.sortBy snd
                        |> Seq.mapi (fun j -> fun (x,v2) -> (x, (float j)/N))
    
    [1..(int N)] |> Seq.zip [1..(int N)] 
           |> Seq.map (fun (i, j) -> 
                        let ii = (float i)/N
                        let jj = (float j)/N
                        let count = normalizados 
                                    |> Seq.filter (fun (Xi, Yi) -> (Xi <= ii) && (Yi <= jj))
                                    |> Seq.length
                        (ii, jj, (float count) / N))
    
copula valores_retornos |> Visualize.gridview


copula valores_retornos |> Seq.toList

let N = (float (retornos |> Seq.length))
let valores_retonros = retornos |> Seq.map (fun (_,v1, v2) -> (v1, v2))
let max1 = valores_retonros |> Seq.map fst |> Seq.max
let max2 = valores_retonros |> Seq.map snd |> Seq.max
let min1 = valores_retonros |> Seq.map fst |> Seq.min
let min2 = valores_retonros |> Seq.map snd |> Seq.min
let normalizados =  valores_retonros 
                    |> Seq.mapi (fun i -> fun (v1,v2) -> ((v1-min1)/(max1-min1), (v2-min2)/(max2-min2)))
                    
normalizados |> Visualize.gridview                    
valores_retornos  |> Visualize.gridview

valores_retonros |> Seq.map snd
                 |> Statistics.gera_histograma 50 
                 |> Visualize.gridview


valores_retornos |> Seq.map snd |> salva_dados @"c:\temp\dow.txt"