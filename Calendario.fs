module Calendario


open System;
open System.Data.SqlClient;
open System.Xml.Linq;
open System.Xml;
open System.Xml.XPath;

// Carrega a lista de feriados dado uma connection string e um codigo de calendario
let feriados calendarCode =
    let cmd = "select content from tbDiretorio where code = '" + calendarCode + "'"
    let strXml = IOData.rcPhoenix<string> (cmd) |> Seq.nth 0
    [for x in (XElement.Parse (strXml)).Elements() -> DateTime.Parse(x.Attribute(XName.Get("date")).Value)] 
    |> List.filter (fun d -> (d.DayOfWeek <> DayOfWeek.Saturday) && (d.DayOfWeek <> DayOfWeek.Sunday))
    |> List.sort;
                                                    
// Calcula os dias uteis entre duas datas dado a data inicial, final e o calendario.
let networkingdays (feriados:list<DateTime>) initial final = 
    let count_feriados = feriados |> List.filter ( fun d -> (d >= initial) && (d <= final)) |> List.length
    let dias_corridos = (final.Subtract (initial)).Days
    let w_ini = initial.DayOfWeek
    let weeks = dias_corridos / 7
    let resultado = dias_corridos - count_feriados - 2 * weeks - match final.DayOfWeek with
                                                                 | w_final when w_final < w_ini -> 2
                                                                 | _ -> 0
    resultado;


//Calendario Brazil OTC
let ndu_brasil = networkingdays (feriados ("CALBRAOTC"));
