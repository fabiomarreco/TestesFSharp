#light

//#if INTERACTIVE
//#I "C:\Marreco\Projetos\TestesFSharp";;
//#endif

#r "System.Xml.Linq.dll";;
#r "FSharp.PowerPack.Math.Providers.dll";;
#r @"C:\GAC\NPlot.dll";;
#r "FSharp.PowerPack.dll";;
#r "FSharp.PowerPack.dll";;
#r @"c:\gac\Yahoo.dll";;

#load "Visualize.fs";;
#load "IOData.fs";;
#load "Medidas.fs";;
#load "Calendario.fs";;
#load "Curves.fs";;
#load "Basis.fs";;
#load "Juros.fs";;
#load "Instrumento.fs";;
#load "TitulosPublicos.fs";;
#load "Yahoo.fs"
#load "Statistics.fs";

open System;
open System.Data;
open System.Data.SqlClient;
open Microsoft.FSharp.Math;
open Microsoft.FSharp.Control;
open Microsoft.FSharp.Math.Experimental;

open System.Drawing;
open NPlot;
let x = 2;;


let memoize = 
    Statistics.memoize