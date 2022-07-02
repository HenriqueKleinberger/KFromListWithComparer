module KFromListWithComparer.Tests

open NUnit.Framework
open Exercice

[<TestFixture>]
type TestClass () =

    [<TestCase(1, 1)>]
    [<TestCase(2, 2)>]
    [<TestCase(3, 3)>]
    [<TestCase(4, 4)>]
    [<TestCase(5, 7)>]
    [<TestCase(6, 9)>]
    member this.ShouldReturnTheKthFromTheListAfterListIsSortedUsingIntComparer(index: int, expected: int) =
        let arg = [
            9
            3
            2
            7
            4
            1
        ]

        let rec comparer (el1: int) (el2: int) = el1 >= el2

        let result = getIndex index comparer arg

        Assert.AreEqual(expected, result)

    //[<Test>]
    [<TestCase(1, "I")>]
    [<TestCase(2, "hi")>]
    [<TestCase(3, "dog")>]
    [<TestCase(4, "code")>]
    [<TestCase(5, "house")>]
    [<TestCase(6, "israel")>]
    [<TestCase(7, "company")>]
    member this.ShouldReturnTheKthFromTheListAfterListIsSortedUsingStringComparer(index: int, expected: string) =
        let arg = [
            "israel"
            "code"
            "hi"
            "I" 
            "dog"
            "house"
            "company"
        ]

        let rec comparer (el1: string) (el2: string) = el1.Length >= el2.Length

        let result = getIndex index comparer arg

        Assert.AreEqual(expected, result)

    [<TestCase(7)>]
    [<TestCase(8)>]
    member this.ShouldThrowErrorWhenIndexIsBiggerThanListLength(index: int) =
        let arg = [
            9
            3
            2
            7
            4
            1
        ]

        let rec comparer (el1: int) (el2: int) = el1 >= el2

        
        let result = Assert.Throws<System.IndexOutOfRangeException>(fun () ->
            getIndex index comparer arg  |> ignore
        )
        Assert.AreEqual("Index is Bigger than list length.", result.Message)


    