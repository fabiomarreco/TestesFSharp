#light

#I @"C:\Users\fabio.marreco\Documents\My Dropbox\Projetos\TestesFSharp";;
//#endif

#r "System.Xml.Linq.dll";;
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

open Statistics

#load "RootFind.fs";;
open RootFind;;

let d1 v L r t sigma = 
    (log (v/L) + (r - 0.5*sigma**2.0)*t)/(sigma*sqrt(t))
    
let d2 v L r t sigma = 
    (d1 v L r t sigma) - sigma * sqrt(t)
    

let equity v L r t sigma = 
    let dd1 = d1 v L r t sigma
    let dd2 = d2 v L r t sigma
    (v * (Statistics.NormSDist dd1)) - exp (-r*t)* (Statistics.NormSDist(dd2))
    
    
let rec calcSigma L v E r t sigmaE sigmaV =
    let dd1 = d1 v L r t sigmaV
    let result = sigmaE * E /(v * (Statistics.NormSDist dd1))
    match abs (result - sigmaV) with
    | x when x < 0.0001 -> result
    | _ -> calcSigma L v E r t sigmaE result

let rec calcV L v E r t sigmaV =
    let dd1 = d1 v L r t sigmaV
    let dd2 = d2 v L r t sigmaV
    let result = (E + exp(-r*t) * (Statistics.NormSDist dd2)) / (Statistics.NormSDist dd1)
    match abs (result - v) with
    | x when x < 0.0001 -> result
    | _ -> calcV L result E r t sigmaV


let calcVS E L sigmaE r t = 
    let rec calcRec E L sigmaE r t sigmaV v
        let sv = calcSigma L v E r t sigmaE sigmaV
        let vv = calcV L v E r t sigmaV
        match abs (vv - v) with
        | x when x < 0.0001 -> (sv, vv)
        | _ -> calcRec E L sigmaE r t sv vv
        
    calcRec E L sigmaE r t simgaE (E - L)

let E = 4.129;;
let L = 1.098314;;
let sigmaE = 0.37;;
let r = 0.10;;
let t = 1.0;;




let v = E - L
let sigmaV = calcSigma L v E r t sigmaE sigmaV

let v = calcV L v E r t sigmaV

let pd = 
    let dd2 = d2 v L r t sigmaV
    (Statistics.NormSDist -dd2) * 100.0
