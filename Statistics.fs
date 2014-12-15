module Statistics
#light
//#r @"c:\gac\alglib.dll"
open System;
open alglib;
//Abreviacao para o tipo interpolador.
type Interpolador = list<float * float> -> float -> float


let interpola_akima (valores:list<float * float>) valor = 
    let x =  (valores |> List.map fst |> List.toArray)
    let y =  (valores |> List.map snd |> List.toArray)
    let s = ref (new spline1d.spline1dinterpolant())
    let rep = ref (new spline1d.spline1dfitreport())
    let info = ref 0
    let n = valores.Length
    //do spline1d.spline1dbuildcubic(x, y, n, 1, +1.0, 1, -1.0, s) |> ignore //4, info, s, rep) |> ignore
    do spline1d.spline1dbuildakima(x, y, n, s) |> ignore //4, info, s, rep) |> ignore
    spline1d.spline1dcalc(s, valor)
    

let gera_distribuicao inverseFunc = 
    let rnd = new System.Random()
    Seq.initInfinite (fun i -> rnd.NextDouble()) |> Seq.map inverseFunc

// Realiza interpolacao linear    
let interpola_linear (valores:list<float * float>) valor = 
    let rec busca_valor (lista) = 
          match lista with
          | h::[]  -> snd h
          | (x1,y1)::(x2,y2)::_ when (x1 <= valor) && (x2 >= valor) -> (valor - x1) * (y2 - y1) / (x2 - x1) + y1
          | (x, y)::_ when x > valor -> y
          | h::t -> busca_valor t
          | _ -> 0.0
    busca_valor (valores);

// Realiza interpolacao flat forward
let interpola_flat_forward (valores:list<float * float>) valor = 
    let rec busca_valor (lista) = 
          match lista with
          | (x,y)::[]  -> Math.Pow (y, valor / x)
          | (x1,y1)::(x2,y2)::_ when (x1 <= valor) && (x2 >= valor) -> y1 * Math.Pow (y2 / y1, (valor - x1) / (x2 - x1))
          | (x, y)::_ when x > valor -> Math.Pow (y, valor / x)
          | h::t -> busca_valor t
          | _ -> 0.0
    busca_valor (valores);


let aloca interpolacao alocacoes x = 
    let interp = alocacoes 
                 |> Seq.mapi (fun i x -> (x, (float (i)))) |> Seq.toList
                 |> (fun l -> interpolacao l x);
    alocacoes |> Seq.mapi (fun i _ -> 
                    match ((float i) - interp) with
                    | xx when (xx >= 0.0 && xx <= 1.0) -> 1.0 - xx
                    | xx when (xx >= -1.0 && xx <= 0.0) -> 1.0 + xx
                    | _ -> 0.0)
   

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
             let res = f x
             cache := (!cache).Add(x,res)
             res;;

let rec binomial (n_, r_) =
    let b (n, r) = 
        match r with
        | r when (r = 0 || r = n)  -> 1
        | r -> binomial(n - 1, r) + binomial(n - 1, r - 1)
    memoize b (n_, r_);;


let acumula_pdf pdf = pdf |> Seq.scan (fun (_, acc) (v, x) -> (v, acc + x)) (0., 0.) |> Seq.skip 1
 
//Gera um histograma a partir de uma lista de retornos.
let gera_histograma bars retornos  =
    let sorted = retornos |> Seq.sort
    let janela = sorted |> Seq.length
    let max = sorted |> Seq.max 
    let min = sorted |> Seq.min
    sorted |> Seq.groupBy (fun x -> floor((x - min) * (float bars) / (max - min)))
           |> Seq.map (fun (x, c) -> 
                        (min + (x+0.5) * ( max - min) / (float bars), (float (Seq.length (c)))/ (float (janela-1))));

//Sequencia dos valores possiveis do ativo S
//Gera os valores da simulacao
let generate_dist cdf count seed = 
    let cdf_inv = cdf |> Seq.map (fun (p, x) -> (x, p)) |> Seq.toList |> interpola_linear
    let rand_seq seed = 
         let rnd = new Random(seed)
         Seq.initInfinite (fun _ -> rnd.NextDouble())
    rand_seq seed |> Seq.take count |> Seq.map cdf_inv;
 
// media 
let mean xs =
    Seq.fold ( + ) 0.0 xs / float(Seq.length xs);;
 
//variancia
let variance xs =
    let variance_aux (m, s , k) x =
        let m' = m + (x - m) / k
        m', s + (x - m) * (x - m'), k + 1.0 
    let _, s, n2 = Seq.fold variance_aux (0.0, 0.0, 1.0 ) xs
    s / (n2 - 2.0);;

//desvio padrao
let standard_deviation x =
    sqrt (variance x) ;;
    
//Calcula a autocorrelacao de uma serie    
let autocorrelation serie lag =
    let avg = mean serie 
    let var = variance serie
    let s = serie |> Seq.windowed (lag+1)
                  |> Seq.map (fun x-> (x.[0] - avg) * (x.[lag] - avg))
                  |> Seq.sum
    s / ((float (Seq.length (serie) - lag)) * var);;

// simpson integration	
let simpson f a b =
    let h = (b - a) / 4096.0
    let h3 = h / 3.0
    let fA = f(a)
    let rec loop a count acc =
        match count with
        | count when count >= 4096 -> acc
        | count when count % 2 = 0 -> loop a (count + 1) ((4.0 * f(a + (float(count) * h))) + acc)
        | _ -> loop a (count + 1) ((2.0 * f(a + (float(count) * h)) + acc))
    let fB = f(b - h)
    ((loop a 2 fA) + fB) * h3
//***************************************** NUMERICAL RECEPIES ************************************************//
//Funcao gamma
let gammaln xx = 
    let cof = [|76.18009172947146; -86.50532032941677; 24.01409824083091; 
                    -1.231739572450155; 0.1208650973866179e-2; -0.5395239384953e-5|];
    let tmp = (xx + 5.5) - ((xx + 0.5) * log(xx + 5.5));
    let ser = cof |> Seq.mapi (fun i -> fun x -> x/((float i) + xx + 1.0)) |> Seq.sum |> (+) 1.000000000190015
    -tmp+log(2.5066282746310005*ser/xx);

// Incomplete beta function
let betai a b x =
    let betacf (a,b,x) =
        let MAXIT = 100.0
        let EPS = 3.0e-7
        let FPMIN = 1.0e-30
        let qab=a+b
        let qap=a+1.0
        let qam=a-1.0
        let mutable c=1.0 
        let mutable d=1.0-qab*x/qap
        if (abs(d) < FPMIN) then d <- FPMIN
        d <- 1.0/d
        let mutable h=d
        let mutable m = 1.0
        let mutable del = 2.0
        let mutable stop = false
        while not stop do
            let m2=2.0*m
            let mutable aa=m*(b-m)*x/((qam+m2)*(a+m2))
            d <- 1.0+aa*d 
            if (abs(d) < FPMIN) then d <- FPMIN
            c <- 1.0+aa/c
            if (abs(c) < FPMIN) then c <- FPMIN
            d <- 1.0/d
            h <- h* d*c
            aa <- -(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
            d <- 1.0+aa*d 
            if (abs(d) < FPMIN) then d <- FPMIN
            c <- 1.0+aa/c
            if (abs(c) < FPMIN) then c <- FPMIN
            d <- 1.0/d
            let del=d*c
            h <- h * del    
            m <- m + 1.0
            if (m >= MAXIT) then stop <- true
            if (abs(del-1.0) < EPS)then stop <- true
        done
        if (m > MAXIT) then failwith "a or b too big, or MAXIT too small in betacf"
        h
    if (x < 0.0 || x > 1.0) then  failwith "Bad x in routine betai"
    let bt = if (x = 0.0 || x = 1.0) then 0.0
             else exp(gammaln(a+b)-gammaln(a)-gammaln(b)+a*log(x)+b*log(1.0-x))
    if (x < (a+1.0)/(a+b+2.0)) then
        bt*betacf(a,b,x)/a
    else 
        1.0-bt*betacf(b,a,1.0-x)/b;


//Retorna probabilidade da distribuicao t-Student com v graus de liberadade
///let StudentDist t v = 1.0 - betai (v/2.0) (0.5) (v / (v + t**2.0))
let StudentDist v t = 
    try 
        alglib.studenttdistr.studenttdistribution (v, t)
    with
        | x -> failwith x.Message


let StudentInv v p =
    try 
        alglib.studenttdistr.invstudenttdistribution (v, p)
    with
        | x -> failwith x.Message


//--------------------------------------------------------------------------------------------------------
//Retorna probabilidade da distribuicao normal 0,1
let NormSDist (x:float) = 
    let P     = 0.2316419;
    let COEF = [|0.31938153; -0.356563782; 1.781477937; -1.821255978; 1.330274429|]

    let z = 1.0 / (1.0 + P * abs(x))
    let serie = COEF |> Seq.mapi (fun i-> fun c -> c * (z ** ((float i) + 1.0))) |> Seq.sum
    let normal = 1.0 - (1. / sqrt (2.* Math.PI)) * exp(-x*x/2.) * serie
    if (x < 0.0) then 1.0 - normal else normal

//Inversa da distribuição normal padrão
let  NormSInv (p:float) =
    let a i = Array.get [|-3.969683028665376e+01;  2.209460984245205e+02; -2.759285104469687e+02;  1.383577518672690e+02; -3.066479806614716e+01;  2.506628277459239e+00|] i
    let b i = Array.get [|-5.447609879822406e+01;  1.615858368580409e+02; -1.556989798598866e+02;  6.680131188771972e+01; -1.328068155288572e+01|] i
    let c i = Array.get [|-7.784894002430293e-03; -3.223964580411365e-01; -2.400758277161838e+00; -2.549732539343734e+00; 4.374664141464968e+00;  2.938163982698783e+00|] i
    let d i = Array.get [|7.784695709041462e-03; 3.224671290700398e-01; 2.445134137142996e+00;  3.754408661907416e+00|] i
    if ((p < 0.0) || (p > 1.0)) then failwith "Argument Invalid: probability should be between 0 and 1"
    let plow  = 0.02425
    let phigh = 1.0 - plow
  //{ Rational approximation for lower region: }
    match p with
    | p when p < plow -> let q = sqrt ( -2.0 * log (p))
                         (((((c(0)*q+c(1))*q+c(2))*q+c(3))*q+c(4))*q+c(5)) /
                            ((((d(0)*q+d(1))*q+d(2))*q+d(3))*q+1.0)
    | p when phigh < p -> let q = sqrt( -2.0 * log (1.0 - p))
                          -(((((c(0)*q+c(1))*q+c(2))*q+c(3))*q+c(4))*q+c(5)) /
                                ((((d(0)*q+d(1))*q+d(2))*q+d(3))*q+1.0)
    | _ ->  let q = p - 0.5
            let r =  q * q
            (((((a(0)*r+a(1))*r+a(2))*r+a(3))*r+a(4))*r+a(5))*q /
                (((((b(0)*r+b(1))*r+b(2))*r+b(3))*r+b(4))*r+1.0)


let newton_square a0 eps N = 
    let next N (x:float) = (x + N/x) / 2.0
    let rec repeat f a = 
        LazyList.consDelayed a (fun() -> repeat f (f a))
    let rec within (eps : float)  = function
        | LazyList.Cons(a, LazyList.Cons(b, rest)) when (abs (a - b)) <= eps -> b
        | x -> within eps (LazyList.tail x)
    within eps (repeat (next N) a0)
    

let gera_sigma_garch alfa beta residuos = 
    let llres = LazyList.ofSeq residuos
    let rec sd res (aPrev, sigmaPrev) = 
        seq {let sigma2 = alfa * (aPrev ** 2.0) + beta * sigmaPrev
             match res with
             | LazyList.Cons(h,t) -> yield sigma2
                                     yield! sd t (h, sigma2)
             | LazyList.Nil -> yield sigma2 }
    sd llres (0.0, 1.0)    
    
    
let covar_matrix (X:matrix) = 
    let Xt = X.Transpose
    let nCols = Xt.NumCols
    let nRows = Xt.NumRows
    let cols = [for j in 0..nCols-1 -> Xt.Column (j)]
    let mean = cols |> Seq.fold (+) (Vector.zero nRows) |> (*) (1.0 / (float nCols))
    let scatter = cols |> Seq.map (fun x-> (x - mean) * (x-mean).Transpose)
                       |> Seq.fold (+) (Matrix.zero nRows nRows)
    scatter * (1.0 / (float nCols))

