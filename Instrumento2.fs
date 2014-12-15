module Instrumento2
#light
open System;

//Tipo de fluxo possivel
type TipoFluxo = 
    | Captacao = 0
    | Amortizacao = 1
    | Juros = 2


//Tipo abstrato de instrumento
type IMercado = 
    abstract member DataAnalise : DateTime
    //abstract member Curvas // > Colocar informacao de curvas
    //abstract member Curvas // > Colocar informacao de curvas
    
//Fluxo de caixa    
[<Struct>]
type Fluxo (aTipo : TipoFluxo, aDataBase : DateTime, aDataBaixa : DateTime, aValor : IMercado -> float) =
    member x.Tipo = aTipo
    member x.DataBase = aDataBase
    member x.DataBaixa = aDataBaixa
    member x.Valor = aValor

//Instrumento, representado por uma lista de fluxos
type IInstrumento = 
    abstract member Fluxos : list<Fluxo>
    
//Instrumento generico que recebe uma lista de fluxos no construtor
type InstrumentoGenerico (Fluxos : list<Fluxo>) =
    interface IInstrumento with
        member x.Fluxos = Fluxos;
        



