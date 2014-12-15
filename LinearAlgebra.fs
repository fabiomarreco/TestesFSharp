module LinearAlgebra
#light
open System;
open System.IO;
open System.Xml;

type Matrix<'a> with
    member this.ColumnsSeq =  
        seq { for i in 0..this.NumCols-1 -> this.Column(i) }
    member this.RowsSeq =  
        seq { for i in 0..this.NumRows-1 -> this.Row(i) }

let carrega_matriz arquivo =  
    (File.ReadAllText arquivo).Trim().Split('\n') |> Seq.map (fun x-> x.Trim().Split('\t') |> Seq.map (fun x-> XmlConvert.ToDouble (x)) |> Seq.toList) |> Seq.toList |> matrix;
    
let salva_matriz arquivo (m:matrix) = 
    let str = [0..(m.NumRows-1)] |> Seq.map m.Row 
                                 |> Seq.map (Seq.fold (fun s v -> s  + v.ToString() + "\t") "")
                                 |> Seq.fold (fun prev next -> prev + next + "\r\n") ""
    File.WriteAllText (arquivo, str, Text.Encoding.Default);

let eigen (m:matrix) = 
    let A = m.ToArray2D()
    let n = m.NumRows
    let wr = ref (Array.create n 0.0)
    let wi = ref (Array.create n 0.0)
    let vl = ref (Matrix.create n n 0.0 |> Matrix.toArray2D)
    let vr = ref (Matrix.create n n 0.0 |> Matrix.toArray2D)
    do alglib.evd.rmatrixevd (A, n, 1, wr, wi, vl, vr) |> ignore
    (!wr |> Vector.ofArray, !vr |> Matrix.ofArray2D);

let seigen (m:matrix) = 
    let A = m.ToArray2D()
    let n = m.NumRows
    let D = ref (Array.create n 0.0)
    let vec = ref (Matrix.create n n 0.0 |> Matrix.toArray2D)
    do alglib.evd.smatrixevd (A, n, 1, false, D, vec) |> ignore
    let vD = !D |> Array.rev |> Vector.ofArray
    let mVec = Matrix.ofArray2D (!vec)
                    |> fun x -> x.ColumnsSeq 
                    |> Seq.toList |> List.rev |> matrix
                    |> Matrix.transpose
    (vD, mVec)

let inversem (m:matrix) = 
    let A = ref (m.ToArray2D())
    let n = m.NumRows
    let info = ref 1
    let rep = ref (alglib.matinv.matinvreport())
    do alglib.matinv.rmatrixinverse (A, n, info, rep) |> ignore
    !A |> Matrix.ofArray2D;

let smatrixsolve (A:matrix) (b:vector) = 
    let AA = A |> Matrix.mapi (fun i j v -> if i >= j then v else 0.0)
    let bb = ref (b |> Vector.toArray)
    let n = A.NumCols
    let x = ref (Array.create n 0.0)
    do alglib.ssolve.smatrixsolve (AA.ToArray2D(), bb, n, true, x) |> ignore
    !x |> Vector.ofArray
    

let svd (A:matrix) = 
    let AA = A.ToArray2D()
    let m = A.NumRows
    let n = A.NumCols
    let W = ref (Array.create m 0.0)
    let U = ref (Matrix.create m m 0.0 |> Matrix.toArray2D)
    let VT = ref (Matrix.create m m 0.0 |> Matrix.toArray2D)
    do alglib.svd.rmatrixsvd (AA, m, n, 2, 2, 2, W, U, VT) |> ignore
    (!U |> Matrix.ofArray2D, !W |> Vector.ofArray, !VT |> Matrix.ofArray2D)
    
    