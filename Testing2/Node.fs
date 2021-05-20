module Tests.NodeTests

open VisProg.Shared.Node
open Swensen.Unquote
open Xunit
open System

[<Fact>]
let ``Boxed Func executes`` () =
    let func = boxFn12 (fun x -> x + 5)
    let num = 10
    let res : int = num |> box |> func |> unbox
    res =! (num + 5)

[<Fact>]
let ``Boxed 2 arg Func Executes`` () =
    let func = boxFn22 (fun x y -> x + 5 + y)
    let num = 10
    let num2 = 5
    let boxednum1 = num |> box
    let boxedNum2 = num2 |> box

    let res : int =
        (boxedCurry func [ boxednum1; boxedNum2 ])
        |> unbox

    res =! (num + 5 + num2)

[<Fact>]
let ``Boxed 3 arg Func Executes`` () =
    let func = boxFn32 (fun x y z -> x + 5 + y + z)
    let args = [ 10; 20; 30 ]
    let boxedArgs = args |> List.map box

    let res : int = (boxedCurry func boxedArgs) |> unbox

    res =! ((args |> List.sum) + 5)
