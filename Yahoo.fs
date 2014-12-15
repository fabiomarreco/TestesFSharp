module Yahoo
#light

open System;
open System.Net;
open System.Collections.Generic;
open Yahoo;
open System.Xml;
open System.Net;
open System.Text.RegularExpressions;

//###### VALORES DE MERCADO ############ BUSCA VALORES DO YAHOO
let historico_acao ticker = 
    let link = @"http://ichart.finance.yahoo.com/table.csv?s=" + ticker + "&d=9&e=21&f=2010&g=d&a=0&b=3&c=2000&ignore=.csv"
    let client = new WebClient()
    (client.DownloadString (link)).Split ('\n') 
                                  |> Seq.skip 1 
                                  |> Seq.choose (fun s -> match s.Split (',') |> Seq.toList with
                                                          |h::t when h.Length > 0 -> 
                                                                try
                                                                   Some (DateTime.Parse (h), XmlConvert.ToDouble (t |> List.rev |> Seq.head))
                                                                with
                                                                | ex -> printfn "Linha: '%s'; Head: '%s'; Tail: '%A'; Message: '%s'" s h t ex.Message
                                                                        None
                                                          |_ -> None)
 
// historico_acao "^DJI" |> Seq.toList;;
//Este método serve para pegar o histograma de uma ação a partir do site do yahoo
//Será utilizado abaixo como histograma para geracao da simulacao de monte-carlo
//mas não é o foco principal do calculo aqui.
let retornos_acao ticker = 
    historico_acao ticker |> Seq.pairwise 
                          |> Seq.map ( fun (next, prev) -> (fst (next), (snd (next) / snd (prev) - 1.0)))
                    
// Busca o valor spot do ativo                                                
let ultimo_preco_acao ticker =                                  
    let feeder = new Yahoo.YahooFeeder([TickerOptions.Last_Trade_Price_Only])
    feeder.Feed ([ticker]) |> Seq.map (fun kvp -> kvp.Value.OptionResults |> Seq.head) 
                                   |> Seq.map (fun kvp -> XmlConvert.ToDouble (kvp.Value.Value.ToString()))
                                   |> Seq.head;
    


let busca_contratos_bovespa (data:DateTime) =
    let regex = Regex(@"^01
                        (?<nomesociedade>.{12})
                        (?<tipopapel>.{10})
                        (?<vencimento>.{8})
                        (?<numeroserie>.{7})
                        (?<tipo>.{3}) #070 Compra, 080 VENDA
                        (?<codigopapel>.{12})
                        (?<indicador>.{1})
                        (?<fatorcotacao>.{7})
                        (?<precoexercicio>.{13}) # * 100 
                        (?<posicaocoberta>.{15})
                        (?<posicaotravada>.{15})
                        (?<posicaodescoberta>.{15})
                        (?<posicaototal>.{15})
                        (?<quantidatitulares>.{7})
                        (?<quantidalancadores>.{7})
                        (?<distribuicao>.{3})
                        (?<estilo>.{1})
                        (?<tipoativo>.{3}) #IND = Indice",  RegexOptions.IgnoreCase ||| RegexOptions.Multiline ||| RegexOptions.IgnorePatternWhitespace ||| RegexOptions.Compiled)
    let str = 
        let url = String.Format (@"http://www.cblc.com.br/cblc/ControleRisco/Posicoes/Download/ROPC{0:yyyyMMdd}.dat", data)
        let ws = new WebClient()
        ws.DownloadString (url)
    [for m in regex.Matches (str) ->
        [for i in [1..(m.Groups.Count-1)] -> 
            let g = m.Groups.Item(i)
            (regex.GroupNameFromNumber(i), g.Value.Trim())] |> Map.ofList]
