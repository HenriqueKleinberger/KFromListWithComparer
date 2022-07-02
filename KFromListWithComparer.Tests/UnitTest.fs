module KFromListWithComparer.Tests

open NUnit.Framework
open Exercice

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.ShouldReturnTheKthFromTheListAfterListIsSorted() =
        let arg = [
            9
            3
            2
            7
            4
            1
        ]

        let rec quickSort list =
          match list with
          | [] -> []
          | n::ns ->
            let lessthan, greaterEqual = List.partition ((>) n) ns
            quickSort lessthan @ n :: quickSort greaterEqual

        let result = getIndex 2 quickSort arg

        Assert.AreEqual(2, result)

    [<Test>]
    member this.ShouldThrowErrorWhenIndexIsBiggerThanListLength() =
        let arg = [
            9
            3
            2
            7
            4
            1
        ]

        let rec quickSort list =
          match list with
          | [] -> []
          | n::ns ->
            let lessthan, greaterEqual = List.partition ((>) n) ns
            quickSort lessthan @ n :: quickSort greaterEqual

        
        let result = Assert.Throws<System.IndexOutOfRangeException>(fun () ->
            getIndex 10 quickSort arg |> ignore
        )
        Assert.AreEqual("Index is Bigger than list lenght.", result.Message)


    