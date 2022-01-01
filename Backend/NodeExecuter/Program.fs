// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open TypeShape.Core
open FSharp.Control.Reactive
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Patterns
open VisProg.Shared.Node
open VisProg.BackEnd.Node
open VisProg.Executer
open Microsoft.FSharp.Reflection
(* open VisProg.Node
open VisProg.Executer *)
//type Node=
//|Single of SingleNode
//|MultiForward of NodeBase<Type list,Node list ,Node >
//|MultiBackward of NodeBase<Type list,Node , Node List >
//|Multi of NodeBase<Type list ,Node list , Node list >
//|End of NodeBase<Type,bool,Node>
//|Start of NodeBase<Type,Node,bool >
//and SingleNode=
//    {
//    Fn:obj
//    InputType:Type
//    OutputType:Type
//    next:Node
//    Last:Node
//    }
//and MultiForward=
//    {
//    Fn:obj
//    InputType:Type
//    OutputType:Type
//    next:Node List
//    Last:Node
//    }
//and MultiBackward=
//    {
//    Fn:obj
//    InputType:Type List
//    OutputType:Type
//    next:Node
//    Last:Node List
//    }
//and Multi=
//    {
//    Fn:obj
//    InputType:Type List
//    OutputType:Type
//    next:Node List
//    Last:Node List
//    }
//and End=
//    {
//    Fn:obj
//    InputType:Type List
//    Last:Node List
//    }
//and Start=
//    {
//    Fn:obj
//    OutputType:Type
//    next:Node List
//    }

let add a b =
    printfn ("add")
    a + b

let multiply a b =
    printfn ("mulit")
    a * b

let plus1 a =
    printfn ("plus1")
    a + 1

let cos (a: int) = a |> float |> Math.Cos |> int

/// Evaluates expression untyped
let rec eval =
    function
    | Value (v, t) -> v
    | Coerce (e, t) -> eval e
    | NewObject (ci, args) -> ci.Invoke(evalAll args)
    | NewArray (t, args) ->
        let array = Array.CreateInstance(t, args.Length)

        args
        |> List.iteri (fun i arg -> array.SetValue(eval arg, i))

        box array
    | NewUnionCase (case, args) -> FSharpValue.MakeUnion(case, evalAll args)
    | NewRecord (t, args) -> FSharpValue.MakeRecord(t, evalAll args)
    | NewTuple (args) ->
        let t =
            FSharpType.MakeTupleType [| for arg in args -> arg.Type |]

        FSharpValue.MakeTuple(evalAll args, t)
    | FieldGet (Some (Value (v, _)), fi) -> fi.GetValue(v)
    | PropertyGet (None, pi, args) -> pi.GetValue(null, evalAll args)
    | PropertyGet (Some (x), pi, args) -> pi.GetValue(eval x, evalAll args)
    | Call (None, mi, args) -> mi.Invoke(null, evalAll args)
    | Call (Some (x), mi, args) -> mi.Invoke(eval x, evalAll args)
    | arg -> raise <| NotSupportedException(arg.ToString())

and evalAll args = [| for arg in args -> eval arg |]

let timer = new System.Timers.Timer(1000.0)
timer.Start()

timer.Elapsed
|> Observable.subscribe (fun x -> printfn "timer went off")

let obsv =
    timer.Elapsed
    |> Observable.map
        (fun x ->
            printfn "step1"
            x)
    |> Observable.map (fun x -> x.SignalTime.Second * 10)

(* let ender =
    createBoxedNodeTemplate (printfn "from the observable %A") boxFn "" ""

let middleb = createBoxedNode (multiply) boxFn2 "" ""
let middlea1 = createBoxedNode (multiply 2) boxFn "" ""
let middlea2 = createBoxedNode (plus1) boxFn "" ""
let starter = createFirstNode (obsv) "" "" ""

starter |> join 0 middlea1
starter |> join 0 middlea2
middlea1 |> join 0 middleb
middlea2 |> join 1 middleb
middleb |> join 0 ender *)






// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let test () =
    let a = obsv |> Observable.map (fun x -> x * 2)
    let b = a |> Observable.map (fun x -> x * 3)

    let c =
        a
        |> Observable.map (fun y -> printfn "aaaayyyeee%A" y)

    let d = b |> Observable.map (printfn "hiii%A")

    [| d; c |]
    |> Observable.zipArray
    |> Observable.wait
let prnt (a:'a) = 
    printfn "printing %A"a
    1
let multi (a) b = 
    printfn "multiplying %d and %d" a b
    a*b
let div (a) b = 
    a/b
let div2 (a)  = 
    printfn "dividing %d by2 " a 
    a/2
let runTest()=

    let prntNode= VisProg.BackEnd.Node.createMiddleNodeTemplate prnt boxFn "prints anything" "outputs an int 1."
    let multiNode= VisProg.BackEnd.Node.createMiddleNodeTemplate multi boxFn2 "multiplies" "outputs a*b"
    let divNode= VisProg.BackEnd.Node.createMiddleNodeTemplate div boxFn2 "divides" "output a/b"
    let div2Node= VisProg.BackEnd.Node.createMiddleNodeTemplate div2 boxFn "diveds a /2" "a/2"
    

    let first (a:int)=
        timer.Elapsed|>Observable.map(fun x->x.SignalTime.Second)
    let second (a:int)=
        [2;4;6;8;27;84]|>Observable.toObservable

    let firstTemp=createFirstNodeTemplate boxFn first "name" "firstlist" "numbers"
    let secondTemp=createFirstNodeTemplate boxFn second "name" "firstlist" "numbers"
    let firststartNode= FirstNode(firstTemp)
    let secondstartNode=FirstNode(secondTemp)
    let multi= MiddleNode(multiNode,[||])
    let div2=MiddleNode(div2Node,[||])
    let prnt= MiddleNode(prntNode,[||])

    let jn=VisProg.Shared.Node.Funcs.join
    firststartNode|>jn 0 multi
    secondstartNode|>jn 0 div2
    div2|>jn 1 multi
    multi|>jn 0 prnt

    VisProg.Executer.runner2 [firststartNode;secondstartNode]
    ()
    

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message

(* 
    let a (b: int) = b

    match shapeof<bool -> int -> float> with
    //the domain is the input type, the co-domain is the output type
    // i can use this to generate my node info
    | Shape.FSharpFunc x ->
        printfn "domain:%A" x.Domain
        printfn "codomain:%A" x.CoDomain *)
    //printfn "run test %A" (VisProg.Test2.run())
    //runner starter
    //test()
//    let res = runner2 starter
    runTest()
    Async.RunSynchronously <| Async.Sleep 10000
    //as we can see her the domain is allways the first input type and the codomain is the rest. This is becase of currying.
    // this means that a simple way of implimenting multi-input nodes is to just nest two nodes inside one another
    //the nodes are allways doing a kind of currying.
    //i could display it as one multi-node but internally i could just use a kind of curry-node. These nodes could maybenot passs through the observable
    //stuff and be handled a little differnently
    0 // return an integer exit code
