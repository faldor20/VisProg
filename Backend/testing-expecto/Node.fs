module Tests.NodeTests

open VisProg.Shared.Node
open Swensen.Unquote
open Expecto
open VisProg.Shared.Node.Funcs
open VisProg.BackEnd.Node
[<Tests>]
let tests =
    testList
        "NodeTests"
        [   
            test "Boxed Func Executes" {
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
            }
            test "generic switch"{
                let id inp=
                    inp
                let conc a b=
                    a@b
                let intL()=
                    [0]
                let stL()=
                    [""]
                let idT=createMiddleNodeTemplate id boxFn "" ""
                let concT= createMiddleNodeTemplate conc boxFn "" ""
                let intLT= createMiddleNodeTemplate intL boxFn "" ""
                let stLT= createMiddleNodeTemplate stL boxFn "" ""
                let idN=MiddleNode(idT,[|None|])
                let concN=MiddleNode(concT,[|None|])
                let intLN1=MiddleNode(intLT,[||])
                let intLN2=MiddleNode(intLT,[||])
                let stLN1=MiddleNode(stLT,[||])

                intLN1|>join 0 idN
                idN|>join 0 concN
                intLN2|>join 1 concN
                intLN1|>disconnect 0 idN
                stLN1|>join 0 idN
            }
            test "methodinfo"{
                let a  (a:'a) (b:'a) (c:'b)(d:list<'b>)=
                    ()

                let inf=a.GetType().GetMethod(nameof a)
                let info=a.GetType().DeclaringType.GetMethod(nameof a)
                printfn "%A" info
                ()
            } 
        ]
