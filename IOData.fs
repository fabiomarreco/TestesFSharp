module IOData
#light
open System
open System.Collections.Generic
open System.Data
open System.Data.SqlClient
open System.Windows.Forms
//#r "FSharp.PowerPack.dll";


let f = Seq.map (fun x -> x *2)


let l = [1..10]
l |> f


let SqlCommandNonReader (connectionString:string) commandString =
        let conn = new SqlConnection (connectionString)
        conn.Open() |> ignore
        printfn "Executando query %s" commandString
        let cmd = conn.CreateCommand (CommandText = commandString)
        cmd.ExecuteNonQuery() |> ignore;
        conn.Close() |> ignore;;

let SqlCommand<'a> (connectionString:string) commandString : seq<'a> =
    //opener = metodo interno que irá executar a query e retornar um DataReader
    let open_reader() =
        let conn = new SqlConnection (connectionString)
        conn.Open() |> ignore
        printfn "Executando query %s" commandString
        let cmd = conn.CreateCommand (CommandText = commandString)
        cmd.ExecuteReader (CommandBehavior.CloseConnection);

    //generator = metodo interno que irá receber um data reader e retornar um objeto do tipo 'a
    let generator (reader : IDataReader) =
        if reader.Read() then
            let t = typeof<'a>
            match t with 
            | t when t.IsValueType || t = typeof<string> -> 
                Some (reader.GetValue (0) :?> 'a)
            | _ ->
                let props = t.GetProperties()
                let types =
                    props
                    |> Seq.map (fun x -> x.PropertyType)
                    |> Seq.toArray
                let cstr = match t.GetConstructor(types) with
                            | x when x = null  -> (t.GetConstructors ()).[0]
                            | x -> x
                
                let values = Array.create reader.FieldCount (new obj())
                reader.GetValues(values) |> ignore
                let values =
                    values
                    |> Array.map 
                        (fun x -> match x with | :? DBNull -> null | _ -> x)
                        
                Some (cstr.Invoke(values) :?> 'a)
        else
            None
    let rec monta_resultado reader = 
        seq { 
            match generator (reader) with
            | Some(x) -> yield x
                         yield! monta_resultado reader 
            | None -> do reader.Close() |> ignore      }
    monta_resultado (open_reader())
    
     

//let connectionString = @"Data Source=svrrisk01\sql2k501;Initial Catalog=rcPhoenix312;User ID=produtos;Password=rsksql125"
//let mutable connectionString = @"Data Source=172.18.0.10\sql2k501;Initial Catalog=rcPhoenix315;User ID=produtos;Password=rskSQL125"
//let mutable connectionString = @"Data Source=localhost;Initial Catalog=rcPhoenix315Local;User ID=sa;Password="
let mutable connectionString = @"Data Source=10.1.32.170\sql2k801;Initial Catalog=rcPhoenix316;integrated security=sspi;User ID=;Password="


let rcPhoenix<'a> commandStr = SqlCommand<'a> connectionString commandStr;;



(*

rcPhoenix<string> ("select top 1 code from tbDiretorio")
let t = typeof<string>

type Cotacao = 
    { 
        Data : DateTime;
        Vencimento : DateTime;
        Valor : float;
    } 
let cotacoes = rcPhoenix<(DateTime * DateTime * float)> "select top 10 date, maturity, value  from tbVerticeCurva";;
cotacoes |> Visualize.gridview
printfn "%A" cotacoes
*)

//rcPhoenix<string> "select content from tbDiretorio where code = 'CALBRAOTC'";;
