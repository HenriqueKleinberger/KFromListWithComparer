module Exercice
let getIndex (index: int) comparer (list: _ list) =
    if index-1 > list.Length then raise (System.IndexOutOfRangeException("Index is Bigger than list lenght."))
    let sortedList = comparer list
    let thSmallestItem = index - 1
    List.item thSmallestItem sortedList