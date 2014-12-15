module TitulosPublicos
#light
open System;

let mercado = new Curves.Mercado()

let LFT emissao vencimento = 
    Instrumento.TituloCupom (emissao, vencimento, 1000., (Juros.CDI mercado.JurosPre) , Juros.Periodicidades.Bullet);;

let LTN emissao vencimento = 
    Instrumento.TituloCupom (emissao, vencimento, 1000., (Juros.Prefixado 0. Basis.NDU_252), Juros.Periodicidades.Bullet);;

let NTNF emissao vencimento = 
    Instrumento.TituloCupom (emissao, vencimento, 1000., (Juros.Prefixado 0.1 (fun _ -> 0.5)), Juros.Periodicidades.Semestral);;
    
let NTNB emissao vencimento =   
    let ultimo_ipca_data (data:DateTime) = mercado.IPCA 
                                               |> Seq.pairwise 
                                               |> Seq.pick (fun x -> match x with
                                                                     | ((a, valor), (p, _)) when (a <= data) && (p > data) -> Some (valor)
                                                                     | _ -> None )
    Instrumento.TituloCupom (emissao, vencimento, (1000. / (ultimo_ipca_data (emissao))), (Juros.Prefixado 0.06 (fun _ -> 0.5)), Juros.Periodicidades.Semestral)
  

let ntnb = NTNB (DateTime (2000,07,15)) (DateTime (2013,05,15)) :> Instrumento.IInstrumento;;
Visualize.gridview (ntnb.Fluxos())

let instr = NTNB (DateTime (2000,07,15)) (DateTime (2013,05,15)) :> Instrumento.IInstrumento;;


let hoje = new DateTime (2009,02,16);

let ntnf vencimento = Instrumento.TituloCupom (hoje, vencimento, 1000., (Juros.Prefixado 0.1 Basis._30360), Juros.Periodicidades.Semestral)

let teste = ntnf (DateTime(2010,04,01)) :> Instrumento.IInstrumento
printfn "%A" (teste.Fluxos());;

let datainicio = DateTime (2000,03,03);;
let datafim = DateTime.Today;;

//Juros.CDI curva_di1 datainicio datafim
//let inicio = DateTime.Now
//let result = (juros_cdi curva_di1 datainicio datafim)
//[for i in 1..200 ->  (juros_cdi curva_di1 datainicio datafim)]
//let tempo = DateTime.Now - inicio 
//printfn "%A" tempo.TotalMilliseconds
//printfn "%A" result

//let lft = TituloCupom (hoje, vencimento, 1000., (prefixado 0. Basis.NDU_252), _)
        
(*
let hoje = new DateTime (2009,02,16);
let ntnf = new TituloCupom (hoje, hoje.AddYears (2), 1000., (fun x -> Math.Pow (1.1, 1./2.)-1.), periodicidade_mensal (6))
let teste = ntnf :> IInstrumento;;
teste.Fluxos();;

let teste = periodicidade_mensal 6 hoje |> Seq.take (4) |> Seq.to_list
teste;;

let vencimento = new DateTime (2011,02,16);
let ltn = new Bullet (hoje, vencimento, 100000.) :> IInstrumento

let teste = hoje ::  [ hoje.AddMonths (6) ]
*)