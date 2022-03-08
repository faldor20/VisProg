module Tests.NodeTests

open VisProg.Shared.Node
open Swensen.Unquote
open Expecto
open Expecto.Performance
open VisProg.Shared.Node.Funcs
open VisProg.BackEnd.Node
open FSharp.Control.Reactive
let id2 (inp:'a)=
    inp
let conc (a:'a list) (b:'a list)=
    a@b
let intL()=
    [[0]]|>Observable.ofSeq
let stL()=
    [[""]]|>Observable.ofSeq
let add a b=
    a+b
let print b=
    //printfn "output %A" b
    ()
let numberInput (nums: 'a list)=
    nums|>Observable.ofSeq
let testSpeed a b =
    List.iter2(fun x y->add x y|>print) a b
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
            test "changing types propigates those changes down generics "{
                let idT=DocumentGetter.CreateMiddleNodeTemplate(id2 ,"id" ,"")
                let concT= DocumentGetter.CreateMiddleNodeTemplate(conc ,"concatenate list" ,"")
                let intLT= DocumentGetter.CreateFirstNodeTemplate(intL ," int list start" ,"")
                let stLT= DocumentGetter.CreateFirstNodeTemplate(stL ,"string list start" ,"")
                let idN=MiddleNode(idT)
                let concN=MiddleNode(concT)
                
                let intLN1=MiddleNode(intLT,[||])
                let intLN2=MiddleNode(intLT,[||])
                let stLN1=MiddleNode(stLT,[||])

                intLN1|>join 0 idN
                idN|>join 0 concN
                intLN2|>join 1 concN
                intLN1|>disconnect 0 idN
                Expect.throws(fun ()-> stLN1|>join 0 idN) "DiD not throw exception"
            }
            
            test "Simple execution" {
                let numbers= DocumentGetter.CreateFirstNodeTemplate(numberInput,"sequence input","stream f numbers")
                let adder= DocumentGetter.CreateMiddleNodeTemplate(add ,"adder","a+b")
                let printer= DocumentGetter.CreateMiddleNodeTemplate(print, "printer","unit")
                let speed=DocumentGetter.CreateMiddleNodeTemplate(testSpeed,"speeeed","")

                let timer=System.Diagnostics.Stopwatch()
                let printN=MiddleNode(printer)
                let addN=MiddleNode(adder)
                let inp=(fun x->x+2)|>List.init (1000*1000)

                let speedN=MiddleNode(speed)
                let startN=  MiddleNode(numbers,[| inp |])
                let start2N= MiddleNode(numbers,[| inp |])
                
                startN|>join 0 addN
                start2N|>join 1 addN
                addN|>join 0 printN

                VisProg.Executer.runner3([start2N;startN])
                printfn"|Done|"
            
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
[<Tests>]
let sequencedTests=
    testSequenced<|testList "perfTests" [
    test "speedTest"{
        let numbers= DocumentGetter.CreateFirstNodeTemplate(numberInput,"sequence input","stream f numbers")
        let adder= DocumentGetter.CreateMiddleNodeTemplate(add ,"adder","a+b")
        let printer= DocumentGetter.CreateMiddleNodeTemplate(print, "printer","unit")
        let speed=DocumentGetter.CreateMiddleNodeTemplate(testSpeed,"speeeed","")

        let timer=System.Diagnostics.Stopwatch()
        let printN=MiddleNode(printer)
        let addN=MiddleNode(adder)
        let inp=(fun x->x+2)|>List.init (1000*1000)
        let speedN=MiddleNode(speed)

        //Mapping over in an inner function
        let startN=  MiddleNode(numbers,[| [inp] |])
        let start2N= MiddleNode(numbers,[| [inp] |])

        startN|>join 0 speedN
        start2N|>join 1 speedN
        timer.Restart()
        VisProg.Executer.runner3([start2N;startN])
        timer.Stop()
        timer.Reset()
        printfn "Observable Inner took %Ams"timer.Elapsed.TotalMilliseconds 

        //Using the Observable to map over
        let startN=  MiddleNode(numbers,[| inp |])
        let start2N= MiddleNode(numbers,[| inp |])
        startN|>join 0 addN
        start2N|>join 1 addN
        addN|>join 0 printN

        timer.Start()
        VisProg.Executer.runner3([start2N;startN])
        timer.Stop()
        printfn "Observable took %Ams"timer.Elapsed.TotalMilliseconds 
        timer.Reset()
        //Uinsg a normal List.map
        timer.Start()
        testSpeed inp inp
        timer.Stop()
        printfn "List.map took %Ams" timer.Elapsed.TotalMilliseconds
        
        }

    ]