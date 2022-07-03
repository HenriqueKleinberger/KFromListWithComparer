module Assignment

// I'm using the merge sort algorithm for sorting the elements from the list.

// The merge sort algorithm has the complexity of O(n log n) in all cases because
// we always need to follow the same steps: split the array into n lists,
// and merge these lists one by one sorting them each time.

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
            | (x::tailX,y::tailY) ->
              if comparer x y then x::merge tailX l2
              else y::merge l1 tailY

        let rec mergesort list = 
          match list with
            | [] -> []
            | [x] -> [x]
            | _ ->
            let (l1,l2) = split list
              in merge (mergesort l1) (mergesort l2)
    
        let validation = 
            if index <= 0 then raise (System.IndexOutOfRangeException("Index should be bigger than 0."))
            if index > list.Length then raise (System.IndexOutOfRangeException("Index is Bigger than list length."))

        validation

        let sortedList = mergesort list
        let thSmallestItem = index - 1
        List.item thSmallestItem sortedList
