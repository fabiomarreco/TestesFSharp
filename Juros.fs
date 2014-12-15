module Juros
#light        
open System;

type DirecaoPeriodo = 
    | Forward  = 1
    | Backward = -1
    
//Representa uma periodicidade de pagamento
type Periodicidade = DirecaoPeriodo -> DateTime -> seq<DateTime>
        
        
//Representa uma periodicidade em periodos mensais        
let rec periodicidade_mensal (meses:int) (direcao:DirecaoPeriodo) (data_inicio:DateTime) = 
    let periodicidade = periodicidade_mensal meses direcao 
    seq {
        yield data_inicio 
        yield! periodicidade (data_inicio.AddMonths (int(direcao) * meses))
        }

let bullet (direcao:DirecaoPeriodo)  (data:DateTime) = seq { yield data; }

type CalculoJuros  = (DateTime * DateTime) -> float
type CalculoPeriodo  = (DateTime * DateTime) -> float

type Periodicidades = 
    static member Mensal    = periodicidade_mensal 1
    static member Semestral = periodicidade_mensal 6
    static member Anual     = periodicidade_mensal 12
    static member Bullet    = bullet
    
let Prefixado (taxa:float) (calculoPeriodo:CalculoPeriodo) (datainicio:DateTime, datafim:DateTime) = 
    let periodo = calculoPeriodo (datainicio, datafim)
    Math.Pow (1. + taxa, periodo)-1.
    
//Calculo do CDI acumulado
let CDI (curva_di1:Curves.Curva) (datainicio:DateTime, datafim:DateTime) = 
        let rec acumulado lista = 
            match lista with
            | h::[] -> 
                    let curva = snd h 
                                |> Seq.map (fun (dt, mt, valor) -> (Basis.NDU_252(dt, mt), valor))
                                |> Seq.toList
                    let interpolacao =  Statistics.interpola_flat_forward (curva) 
                    let valor = interpolacao (Basis.NDU_252 (fst h, datafim))
                    //printfn "%A %A" (fst h) curva 
                    valor
            | h::t -> 
                    let (dt, mt , valor) = snd h |> Seq.nth 0
                    let cdi_over = valor
                    cdi_over * acumulado (t)
            | [] -> 1. 
        curva_di1  
        |> Seq.filter (fun (dt, _) -> dt >= datainicio && dt <= datafim) 
        |> Seq.toList
        |> acumulado
