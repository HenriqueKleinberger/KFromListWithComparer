module Exercice

let getIndex (index: int) comparer (list: _ list) =
    let split list =
      let rec aux l acc1 acc2 =
        match l with
          | [] -> (acc1,acc2)
          | [x] -> (x::acc1,acc2)
          | x::y::tail ->
            aux tail (x::acc1) (y::acc2)
      in aux list [] []

    let rec merge l1 l2 =
      match (l1,l2) with
        | (x,[]) -> x
        | ([],y) -> y
        | (x::tx,y::ty) ->
          if comparer y x then x::merge tx l2
          else y::merge l1 ty

    let rec mergesort list = 
      match list with
        | [] -> []
        | [x] -> [x]
        | _ ->
        let (l1,l2) = split list
          in merge (mergesort l1) (mergesort l2)

    if index > list.Length then raise (System.IndexOutOfRangeException("Index is Bigger than list length."))
    let sortedList = mergesort list
    let thSmallestItem = index - 1
    List.item thSmallestItem sortedList