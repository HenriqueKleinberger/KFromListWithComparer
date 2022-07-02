module Exercice

let split list =
  let rec aux l acc1 acc2 =
    match l with
      | [] -> (acc1,acc2)
      | [x] -> (x::acc1,acc2)
      | x::y::tail ->
        aux tail (x::acc1) (y::acc2)
  in aux list [] []

let rec merge l1 l2 comparer =
  match (l1,l2) with
    | (x,[]) -> x
    | ([],y) -> y
    | (x::tx,y::ty) ->
      if comparer y x then x::merge tx l2 comparer
      else y::merge l1 ty comparer

let rec mergesort list comparer = 
  match list with
    | [] -> []
    | [x] -> [x]
    | _ ->
    let (l1,l2) = split list
      in merge (mergesort l1 comparer) (mergesort l2 comparer) comparer

let getIndex (index: int) comparer (list: _ list) =
    if index > list.Length then raise (System.IndexOutOfRangeException("Index is Bigger than list length."))
    let sortedList = mergesort list comparer
    let thSmallestItem = index - 1
    List.item thSmallestItem sortedList