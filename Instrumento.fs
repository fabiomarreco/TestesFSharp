module Instrumento
#light

open System;
//Tipo de fluxo possivel
type TipoFluxo = 
    | Captacao = 0
    | Amortizacao = 1
    | Juros = 2


//Representa um fluxo 
type Fluxo = (TipoFluxo * DateTime * float)
    

//Tipo abstrato de instrumento
type IInstrumento = 
    abstract member Fluxos : unit -> seq<Fluxo>
    
    
//Representa um título bullet (paga apenas um fluxo, sem cupom)
type Bullet(emissao:DateTime, vencimento:DateTime, principal:float) =
    interface IInstrumento with
        member self.Fluxos() = seq { yield (TipoFluxo.Captacao, emissao, principal)
                                     yield (TipoFluxo.Amortizacao, vencimento, principal) } 

//Representa um título com pagamento de cupons fixos;
type TituloCupom (emissao:DateTime, vencimento:DateTime, principal:float, calculo_juros:Juros.CalculoJuros, periodicidade:Juros.Periodicidade) = 
    let valor_jurosfluxo (dataAnterior, dataPosterior) =  (TipoFluxo.Juros, dataPosterior, principal * calculo_juros(dataAnterior, dataPosterior))
    let datas_fluxos = seq { yield! periodicidade Juros.DirecaoPeriodo.Backward vencimento 
                                    |> Seq.takeWhile ( fun dt -> dt > emissao) 
                             yield emissao } |> Seq.toList |> List.rev 
    interface IInstrumento with 
        member self.Fluxos() = 
            let fluxos_juros =  datas_fluxos
                                |> Seq.pairwise 
                                |> Seq.map valor_jurosfluxo
            seq {   yield (TipoFluxo.Captacao, emissao, -principal)
                    yield! fluxos_juros 
                    yield (TipoFluxo.Amortizacao, vencimento, principal) }
