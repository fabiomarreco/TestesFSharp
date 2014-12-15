module Curves
#light

open System;
open System.Data.SqlClient;

type Curva = seq<DateTime * seq<DateTime * DateTime * float>>

// Carrega os vertices de uma curva
let load_vertex (codigo:string, dataInicio:DateTime, dataFim:DateTime) = 
    let cmdStr = @" select v.Date, v.Maturity, v.Value 
                    from tbVerticeCurva v 
                    inner join tbDiretorio d on d.ID = v.CurveID 
                    where 
                    v.Date between '" + dataInicio.ToString ("yyyy-MM-dd") + "' and '" + dataFim.ToString ("yyyy-MM-dd") + "'
                    and d.Code = '" + codigo + "'
                    order by v.Date, v.Maturity
                    "
    IOData.rcPhoenix<DateTime * DateTime * float> cmdStr 
    |> Seq.groupBy (fun (d, _, _) -> d)
    |> Seq.cache

//Carrega a serie 
let load_serie (codigo:string, janela:int, dataFim:DateTime)  = 
    IOData.rcPhoenix<DateTime * float> ("select top " + janela.ToString() + " c.data, c.valor from tbCotacaoSerieHistorica c inner join tbDiretorio d on d.ID = c.IDSerie where c.Data <= '" + dataFim.ToString ("yyyy-MM-dd") + "' and d.Code = '" + codigo + "' order by c.Data desc") 
        |> Seq.toList
        |> List.rev

