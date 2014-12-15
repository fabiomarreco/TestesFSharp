// Learn more about F# at http://fsharp.net

#light
open System;
open System.Reflection;
open System.Net;
open System.IO;
open System.Text.RegularExpressions;
open Microsoft.FSharp.Control.CommonExtensions 

//let url = @"http://www.stickystockings.com/files/Archive/Flipfix/80's%20(The%20Billboard)/";
let mascara = "href=\"(?<link>.*?\.mp3)\">(?<name>.*?)<";
let pastaDestino = @"C:\temp\";

let executa (url:string) mascara (pastaDestino:string) = 
    if (Directory.Exists (pastaDestino) = false) then 
        do Directory.CreateDirectory (pastaDestino) |> ignore
    let client = new WebClient();
    let html = client.DownloadString (url);
    let mapMatch (m:Match) = (m.Groups.Item(1).Value.Trim(), (m.Groups.Item(2).Value).Trim().Replace("..&gt;", ".mp3"))
    let regex = Regex (mascara, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
    let links = [for m in regex.Matches (html) -> mapMatch (m)]
    printfn "url: %A" url
    printfn "Downloading links %A" links 
    let downloadData (uri:Uri, arquivo) =  
        async { let client = new WebClient()
                if (File.Exists (arquivo) = false) 
                then printfn "downloading file... %s" (uri.ToString())
                     try client.DownloadFile (uri, arquivo) with | ex -> printfn "\n erro de download %s (%s)" arquivo ex.Message
                     printfn "\n-------------------------------------------------\r\ndownloaded file... %s\r\n---------------------------------" arquivo 
                else printfn "file... %s already downloaded" arquivo }
                    
                    
    let result = links      
                 |> Seq.map (fun (uri, file) -> (new Uri(url + uri), (pastaDestino + file)))
                 |> Seq.map downloadData
    result;

    
let main() = 
    let baseurl = @"http://muzika.hopto.org/music/";
    let downloads = 
        ["_Metal%20n%20Rock";
         "_Reggae";
         "_Dance"
        ] |> Seq.map (fun x-> 
                            executa (baseurl + x + @"/") mascara (@"C:\temp\pearl\" + x + @"\") 
                            |> Async.Parallel)
    do [for pasta in downloads -> Async.RunSynchronously pasta ] |> ignore
    printfn "finito!"


         // |> Seq.iter (fun x-> do x)
                            //  |> Seq.skip 1
                            //  |> Async.Parallel
                            //  |> Async.RunSynchronously
    
    //let downloads = executa url mascara pastaDestino 
    //do Async.RunSynchronously (Async.Parallel (downloads))


[for i in 1..10 -> do main()] |> ignore

Console.ReadKey();

//                let req = WebRequest.Create (uri)  
//                 let! rsp = req.GetResponseAsync()
//                 let string
//                 //use stream = rsp.GetResponseStream() 
                 
                    
//    

open System
open System.Net
open Microsoft.FSharp.Control.WebExtensions
let http url =
    async { let req =  WebRequest.Create(Uri url)
            use! resp = req.AsyncGetResponse()
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream)
            let contents = reader.ReadToEnd()
            return contents }
 
let sites = ["http://www.bing.com";
             "http://www.google.com";
             "http://www.yahoo.com";
             "http://www.search.com"]
 
let htmlOfSites =
    Async.Parallel [for site in sites -> http site ]
    |> Async.RunSynchronously
 