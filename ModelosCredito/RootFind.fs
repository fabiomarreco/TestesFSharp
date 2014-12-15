module RootFind

open System;;

let rec fixed_point f x =
    match f x with
    | f_x when x = f_x -> x
    | x -> fixed_point f x;;
    
    
let binary_search split cmp (x, c_x, y, c_y) =
    let m = split x y
    let c_m = cmp m
    match c_x = c_m, c_m = c_y with
    | true, false -> m, c_m, y, c_y
    | false, true -> x, c_x, m, c_m
    | _ -> raise (new System.Collections.Generic.KeyNotFoundException());;    

let find_root f (x:Vector<float>) (y:Vector<float>) =
    let split x y = (x + y) |> Vector.map (fun x -> x /2.0)
    let cmp x = compare (f x) 0.0
    (x, cmp x, y, cmp y)
    |> fixed_point (binary_search split cmp);;


