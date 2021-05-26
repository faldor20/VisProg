module VisProg.Test2
open System
open Reflection.FSharpReflectionExtensions
open FSharp.Reflection
type Node ={
    Fn:obj
    InputType:Type list
    OutputType:Type
    mutable Next: Node list 
    Last:Node option array 
}
type InvokeResult = 
    | Success of obj
    | ObjectWasNotAFunction of Type

let dynamicFunction (fn:obj) (args:obj seq) =
    let rec dynamicFunctionInternal (next:obj) (args:obj list) : InvokeResult =
        match args.IsEmpty with
        | false ->
            let fType = next.GetType()
            if FSharpType.IsFunction fType then
                let (head, tail) = (args.Head, args.Tail)
                let methodInfo = 
                    fType.GetMethods()
                    |> Seq.filter (fun x -> x.Name = "Invoke" && x.GetParameters().Length = 1)
                    |> Seq.head
                let partalResult = methodInfo.Invoke(next, [| head |])
                dynamicFunctionInternal partalResult tail
            else ObjectWasNotAFunction fType
        | true ->
            Success(next)
    dynamicFunctionInternal fn (args |> List.ofSeq )

//Im ignoring multiple input nodes for now
let rec runSimple (last:IObservable<'a>) node =
    printfn "isFunction:%A data%A"
        (FSharpType.IsFunction(node.Fn.GetType()))
        (FSharp.Reflection.FSharpType.GetFunctionElements(node.Fn.GetType()))
    let method=node.Fn.GetType().GetMethod("Invoke")
    let fn a:'b=
        printfn "runnig thing %A" a
        match dynamicFunction node.Fn [|a|] with
        |InvokeResult.Success x->x
        |InvokeResult.ObjectWasNotAFunction x-> failwithf "dammit %A" x
        |>unbox
    printfn "type of func %A" (fn.GetType())
    let obsv=last|>Observable.map (fn)
    let res=node.Next|>List.collect (runSimple obsv)
    if res.Length=0 then 
        [obsv]
    else res

let minimalNode (fn:'a->'b) outputType nodes=
    {
        Fn= (box fn);
        InputType=[];
        OutputType=outputType
        Next= nodes;
        Last=Array.empty;
    }
let add2 a= 
    printfn "adding 2"
    a+2
let printer a= printfn "======WE DIDI IT======%i \n\n\n\n===YAY===" a

let ender= minimalNode printer typeof<unit> []
let mid= minimalNode add2 typeof<int> [ender]

let timer= System.Timers.Timer(1000.0)
timer.Start();
timer.Elapsed|>Observable.subscribe(fun x-> printfn "timer went off");
let obsv=timer.Elapsed|>Observable.map(fun x-> printfn"step1"; x )|>Observable.map (fun x-> x.SignalTime.Second*10)
let run()=
    runSimple obsv mid 
    |>List.map(fun x->
        printfn "subscribing observable"
        x.Subscribe(fun x->printfn "end"))
    Async.RunSynchronously<| Async.Sleep(10000)