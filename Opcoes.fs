module Opcoes
#light

open System;
open System.Collections.Generic;
open Yahoo;
open System.Xml;
open System.Net;



//Gera a densidade de probabilidade dos retornos e a distribuicao acumulada
let S0 = Yahoo.ultimo_preco_acao "BBAS3.SA";;
let pdf = Yahoo.retornos_acao "BBAS3.SA" |> Seq.map snd |> Seq.take 300 |> Statistics.gera_histograma 30 |> Seq.cache
let cdf = pdf |> Statistics.acumula_pdf |> Seq.cache;


//Define o que deve ser um payof, funcao que recebe o valor do ativo e retorna o payoff do derivativo
type payoff  = float -> float
let call (K:float)  (S:float) = Math.Max (S - K, 0.);
let put (K:float)(S:float)  = Math.Max (K - S, 0.);
let asset S:float = S;
let (@) (qt:float) (p:payoff) = fun x -> qt * p(x);

   
         
//Gera os cenários para S0
let valores = Statistics.generate_dist cdf 10000 -5  |> Seq.map (fun ret -> S0 * (1. + ret));


Visualize.plot (Statistics.gera_histograma 30 valores )

//Estratégia 
let strategy S =     
    [
      call (S0 - 3.) ;
      -2. @ (call S0);
      call (S0 + 3.)
    ] |> Seq.map ( fun ctg_claim -> ctg_claim S)  |> Seq.sum;

let hist_payoffs = valores  |> Seq.map strategy 
                            |> Statistics.gera_histograma 30 ;


Visualize.plot hist_payoffs;


let gera_distribuicao_payoff simulations seed pdf estrategia = 
    let bars = pdf |> List.length
    let cdf = pdf |> Seq.scan (fun (_, acc) (v, x) -> (v, acc + x)) (0., 0.) |> Seq.skip 1 |> Seq.cache
    let valores = Statistics.generate_dist cdf simulations seed
    valores  |> Seq.map estrategia |> Statistics.gera_histograma 30 
                                
                                
    
let black_scholes s k r t v =
    let d1 = ((log(s /k)) * (r - v*v/2.0)*t) / (v * sqrt (t))
    let d2 = d1 - v * sqrt(t)
    s * Statistics.NormSDist(d1) - k * exp(-r*t)*Statistics.NormSDist(d2);;