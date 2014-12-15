module Basis
#light 
open System;


let NDU_252 (datainicio:DateTime, datafim:DateTime) = 
    let ndu = Calendario.ndu_brasil datainicio datafim  
    (float ndu) / 252.
    
let _30360 (datainicio:DateTime, datafim:DateTime) = 
    let ajusta_data (data:DateTime) = 
        let d = data.AddMonths (1)
        let ultimo_dia_mes = (new DateTime (d.Year, d.Month, 1)).AddDays (-1.)
        match data with
            | d when d = ultimo_dia_mes -> (float d.Year, float d.Month, 30.)
            | d -> (float d.Year, float d.Month, float d.Day)
    let (ano_inicio, mes_inicio, dia_inicio) = ajusta_data datainicio
    let (ano_fim, mes_fim, dia_fim) = ajusta_data datafim
    ano_fim - ano_inicio + (mes_fim - mes_inicio) / 12. + (min (dia_fim - dia_inicio) 30.) / 360.;
 
