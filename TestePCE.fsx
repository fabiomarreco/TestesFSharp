#light
// Carregamento dos dados 
#r "FSharp.PowerPack.dll";
#load "Statistics.fs";;
#load "Curves.fs";;
#load "Visualize.fs";;
#load "ListExt.fs";;
#load "LinearAlgebra.fs";;
#load "Calendario.fs";;

// Utilizacao de bibiliotecas
open System;
open System.IO;
open System.Xml;
open Microsoft.FSharp.Math;;
open LinearAlgebra;




File.ReadAllLines (@"c:\temp\fabio.txt")
|> Seq.skip 2
|> Seq.agre
|> (fun s -> match s