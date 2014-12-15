module ListExt

let merge_lists (list1:list<'a>) (list2:list<'a>) keySelector valueSelector=
    let l1 = list1 |> List.sort
    let l2 = list2 |> List.sort
    let rec merge_sorted (l1:list<'a>) (l2:list<'a>)=
      seq {  match (l1, l2) with
             | ( (lista1), []) -> yield! lista1 |> Seq.map (fun x-> (keySelector(x), Some(valueSelector(x)), None))
             | ( ([], lista2)) -> yield! lista2 |> Seq.map (fun x-> (keySelector(x), None, Some(valueSelector(x))))
             | ((h1::t1), (h2::t2)) -> match (keySelector(h1), keySelector(h2)) with
                                       | (k1,k2) when k1 = k2 -> yield (k1, Some(valueSelector(h1)), Some(valueSelector(h2)))
                                                                 yield! merge_sorted t1 t2
                                       | (k1,k2) when k1 < k2 -> yield (k1, Some(valueSelector(h1)), None)
                                                                 yield! merge_sorted t1 (h2::t2)
                                       | (k1,k2) when k1 > k2 -> yield (k2, None, Some(valueSelector(h2)))
                                                                 yield! merge_sorted (h1::t1) t2
                                       | _ -> failwith "keySelector não é função transiente" }
    merge_sorted l1 l2;


let chose_merged_tuple x =  match x with
                            | (h, Some(v1), Some(v2)) -> Some (h, v1, v2)
                            | _ -> None 
                            
                            
let mapAndZip sequencia funcao = 
    sequencia |> Seq.map (fun x-> (x, funcao x));;