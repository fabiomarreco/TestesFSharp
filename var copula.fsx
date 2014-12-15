#light
#load @"C:\Utilities\startup.fsx"
#load "Yahoo.fs";;
#load "Statistics.fs";;
#r @"c:\gac\alglib.dll";;
#load "ListExt.fs";;
open System;
open System.Xml;
open System.IO;
open System.Text;

let salva_dados arquivo (lista:seq<float>) = 
    use sw = new StreamWriter (arquivo, false, Encoding.Default)
    lista |> Seq.iter(fun x -> sw.WriteLine(XmlConvert.ToString(x)));
        
let carrega_dados (arquivo:string) = 
        seq { use sw = new StreamReader (arquivo, Encoding.Default)
              while not sw.EndOfStream do
                    let nums = String.split [' '; '\t'] (sw.ReadLine())  |>  List.map (fun x -> XmlConvert.ToDouble(x))
                    yield (nums.Head, nums.Tail.Head) };;
        
let carrega_dados_serie (arquivo:string) = 
        seq { use sw = new StreamReader (arquivo, Encoding.Default)
              while not sw.EndOfStream do
                    yield (sw.ReadLine()) |> fun x -> XmlConvert.ToDouble(x.Trim()) };;

let pega_retornos ticker =
        Yahoo.historico_acao ticker
        |> Seq.sortBy fst
        |> Seq.pairwise
        |> Seq.map (fun (prev, next) -> (fst(next), log ( snd(next) /  snd(prev))))
        |> Seq.toList
        

let retornos = ListExt.merge_lists (pega_retornos "^GDAXI") (pega_retornos "^DJI") fst snd
               |> Seq.choose ListExt.chose_merged_tuple 


let x = retornos |> Seq.map ( fun (d, _, _) -> d)

let valores_retornos = retornos |> Seq.map (fun (_,v1, v2) -> (v1, v2))


let N = (float (retornos |> Seq.length))
let valores_retonros = retornos |> Seq.map (fun (_,v1, v2) -> (v1, v2))
let max1 = valores_retonros |> Seq.map fst |> Seq.max
let max2 = valores_retonros |> Seq.map snd |> Seq.max
let min1 = valores_retonros |> Seq.map fst |> Seq.min
let min2 = valores_retonros |> Seq.map snd |> Seq.min
let normalizados =  valores_retonros 
                    |> Seq.mapi (fun i -> fun (v1,v2) -> ((v1-min1)/(max1-min1), (v2-min2)/(max2-min2)))
                    
valores_retornos |> Seq.map fst |> Seq.map ( fun x -> x * 100.00) |> salva_dados @"c:\temp\dax.txt"
valores_retornos |> Seq.map snd |> Seq.map ( fun x -> x * 100.00) |> salva_dados @"c:\temp\dow.txt"

let prob_bvsp = valores_retornos |> Seq.map snd 
                                 |> Seq.map ( fun x -> x * 100.00) 
                                 |> Seq.map (Statistics.StudentDist 7)
                                 |> salva_dados @"c:\temp\dow.txt";;



let prob_all = residuos |> Seq.map ( fun (b, d) -> (Statistics.StudentDist 7 (b * 1.00),  Statistics.StudentDist 8 (d * 1.00)))
                        |> Visualize.gridview

let uvs = carrega_dados @"c:\temp\uvs.txt";;

let residuos = carrega_dados @"c:\temp\juntos.txt";;

let prob_residuos = carrega_dados @"c:\temp\juntos.txt" 
                    |> Seq.map ( fun (b, d) -> (Statistics.StudentDist 7 (b),  Statistics.StudentDist 8 (d)))
                    |> Visualize.gridview


let prob_residuos = valores_retornos
                    |> Seq.map ( fun (b, d) -> (Statistics.StudentDist 7 (b * 100.00),  Statistics.StudentDist 8 (d * 100.00)))
                    |> Visualize.gridview



let gerado = uvs |> Seq.choose (fun (u, v) -> match (u, v) with
                                                | (0.0, _) -> None
                                                | (_, 0.0) -> None
                                                | (1.0, _) -> None
                                                | (_, 1.0) -> None
                                                | (x, y) -> Some (x, y))
                 |> Seq.map (fun (u, v) -> (Statistics.StudentInv 7 u,  Statistics.StudentInv 8 v))

let novos_retornos = carrega_dados @"c:\temp\dax\simulacoes_bb7.txt" |> Seq.map (fun (x, y) -> (Statistics.StudentInv 7 x, Statistics.StudentInv 7 y)) |> Seq.toList


//funcao para gerar quantis de uma serie
let quantile serie (percentual:float) = 
    let indice = (int (percentual * (float (Seq.length (serie)-1))))
    serie |> Seq.sort |> Seq.nth indice;;
    
    
Série	μ ̂_(t+1)	σ ̂_(t+1)
Ibovespa	0.022%	0.4963%
Dow Jones	0.066%	0.4028%
    

let retorno_carteira = novos_retornos |> Seq.map (fun (r1, r2) -> (r1 * 1.461145 + 0.05639475, r2 * 1.451969 + 0.1054607))
                                      |> Seq.map (fun (x, y) -> 0.5*x + 0.5*y)


let retorno_carteira = novos_retornos |> Seq.map (fun (x, y) -> 0.3*x + 0.7*y)
                                      
[0.001; 0.01; 0.05; 0.16] |> Seq.map (fun p-> 100.0*p, quantile retorno_carteira p)

retorno_carteira

  seq [(0.1, -3.996790113); (1.0, -2.60275963); (5.0, -1.624544473)]
  
  retorno_carteira |> Statistics.gera_histograma 50 |> Visualize.gridview
  
valores_retornos |> Seq.map fst |> Seq.map (fun x-> x*100.0)|> salva_dados @"c:\temp\bvsp.txt"
valores_retornos |> Seq.map snd |> Seq.map (fun x-> x*100.0)|> salva_dados @"c:\temp\dow.txt"


valores_retornos |> Seq.map fst |> Seq.map (fun x-> x*100.0)
                                |> Seq.toList |> List.rev
                                |> Seq.take 10
                                |> Statistics.variance
                                |> sqrt

let res_dax = carrega_dados_serie @"c:\temp\dax\residuo_dax.txt"
let res_dow = carrega_dados_serie @"c:\temp\dax\residuo_daw.txt"

res_bvsp |> Seq.zip res_dow |> Visualize.gridview


[1; 2] |> Seq.zip ["a"; "b"]

let probs_residuos = res_dow |> Seq.zip res_dax
                     |> Seq.map ( fun (b, d) -> (Statistics.StudentDist 7 b,  Statistics.StudentDist 7 d))
                     |> Visualize.gridview
retorno_carteira |> Statistics.gera_histograma 50 |> Visualize.gridview



retorno