module Tests.NodeTests

open VisProg.Shared.Node
open Swensen.Unquote
open Expecto

[<Tests>]
let tests =
    testList
        "NodeTests"
        [ test "Boxed Func Executes" {
            let func = boxFn (fun x -> x + 5)
            let num = 10
            let res : int = num |> box |> func |> unbox
            res =! (num + 5)
          }
          test "Boxed multi arg Func Executes" {
              let func = boxFn2 (fun x y -> x + 5 + y)
              let num = 10
              let num2 = 5
              let boxednum1 = num |> box
              let boxedNum2 = num2 |> box

              let res : int =
                  (boxedCurry func [ boxednum1; boxedNum2 ])
                  |> unbox

              res =! (num + 5 + num2)
          } ]
