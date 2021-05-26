open FSharp.Quotations
open FSharp.Quotations.Patterns
open System.Runtime

let add1 a = a + 1

open System
open Microsoft.FSharp.Core.OptimizedClosures


// for 'T = 'T0 -> 'T1 -> ... -> 'Tn, returns the type of 'Tn
let codomain<'T> : 'T -> Type =
    let fsFunctionTypes =
        [ typedefof<FSharpFunc<_, _>>
          typedefof<FSharpFunc<_, _, _>>
          typedefof<FSharpFunc<_, _, _, _>>
          typedefof<FSharpFunc<_, _, _, _, _>>
          typedefof<FSharpFunc<_, _, _, _, _, _>> ]
        |> Seq.map (fun t -> t.GUID)
        |> Set.ofSeq

    let (|FSharpFunc|_|) (t: Type) =
        let isFSharpFunc (t: Type) =
            t <> null
            && t.IsGenericType
            && t.GetGenericTypeDefinition().GUID
               |> fsFunctionTypes.Contains

        match isFSharpFunc t, isFSharpFunc t.BaseType with
        | true, _ -> Some t
        | _, true -> Some t.BaseType
        | _ -> None

    let rec traverse =
        function
        | FSharpFunc func ->
            let funcTypes = func.GetGenericArguments()
            let returningType = funcTypes.[funcTypes.Length - 1]
            traverse returningType
        | other -> other

    fun f -> f.GetType() |> traverse

// examples:
printfn "%A" <| codomain 2

printfn "%A"
<| (codomain
    <| fun x y z w -> (x + y + Int32.Parse(z) + w).ToString())

printfn "%A" <| codomain codomain

type Node =
    { Fn: obj
      InputType: Type list
      OutputType: Type
      mutable Next: Node list
      Last: Node option array }

//Im ignoring multiple input nodes for now
let rec runSimple last node =
    let obsv = last |> Observable.map (unbox node.Fn)
    node.Next |> List.collect (runSimple obsv)

(* type NodeSimple<'T,'U> ={
    Fn:'T->'U
    mutable Next: NodeSimple<'T,"HELP"> list

}
 *)
let minimalNode fn outputType nodes =
    { Fn = fn
      InputType = []
      OutputType = outputType
      Next = nodes
      Last = Array.empty }

let add2 a = a + 2
let print a = printfn "%i" a
let ender = minimalNode (print) typeof<unit> []
let mid = minimalNode add2 typeof<int> [ ender ]

let timer = System.Timers.Timer(1000.0)
timer.Start()

let obsv =
    timer.Elapsed
    |> Observable.map (fun x -> x.SignalTime.Second * 10)

runSimple obsv mid
let a = typeof<Int64>

let b = typeof<Int32>
(* printfn "%A" <| a.IsInstanceOfType(b)
printfn "%A" <| b.IsInstanceOfType(a)
printfn "%A" <| b.IsAssignableFrom(a)
printfn "%A" <| b.IsAssignableTo(a)
printfn "%A" <| b.GetInterfaces() *)
let inline objectifyTuple< ^a, ^b, ^c, ^d, ^U> (tuple: ^U) =
    let boxed = (box tuple)

    match (boxed) with
    | :? (^a * ^b) as t ->
        let a, b = t
        box (box a, box b)
    | :? (^a * ^b * ^c) as t ->
        let a, b, c = t
        box (box a, box b, box c)
    | :? (^a * ^b * ^c * ^d) as t ->
        let a, b, c, d = t
        box (box a, box b, box c, box d)
    | _ -> failwith "non implmented tuple length"

let out =
    objectifyTuple<int, string, obj, obj, int * string> (1, "hi")

printfn "%A" (unbox out)
