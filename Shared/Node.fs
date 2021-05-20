module VisProg.Shared.Node

open System
open FSharp.Reflection
open TypeShape.Core
open TypeShape

let typeMatches =
    [ (typeof<SByte>,
       [ typeof<Int16>
         typeof<Int32>
         typeof<Int64>
         typeof<Single>
         typeof<Double>
         typeof<Decimal> ])
      (typeof<Int16>,
       [ typeof<Int16>
         typeof<Int32>
         typeof<Int64>
         typeof<Single>
         typeof<Double>
         typeof<Decimal> ])
      (typeof<Int32>,
       [ typeof<Int64>
         typeof<Single>
         typeof<Double>
         typeof<Decimal> ])
      (typeof<Int64>, []) ]

let hasImplicitConversion (source: Type) (destination: Type) =


    let matches =
        typeMatches
        |> List.exists
            (fun (x, matches) ->
                if source = x then
                    matches |> List.exists (fun y -> y = destination)
                else
                    false)

    matches
(*
    match( sourceCode ) with
    | t when t=(typedefof<SByte>)->
            match( destinationCode )with
            | t when t=typeof<Int16> || t= typeof<Int32> || t=typeof<Int64> ||  t= typeof<Single> || t= typeof<Double> ||  t= typeof<Decimal>->
                true
            |_->
                false
    (* | TypeCode.Byte->
            match( destinationCode )with
            | TypeCode.Int16 | TypeCode.UInt16 | TypeCode.Int32 | TypeCode.UInt32 | TypeCode.Int64 | TypeCode.UInt64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Int16->
            match( destinationCode )with
            | TypeCode.Int32 | TypeCode.Int64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.UInt16->
            match( destinationCode )with
            | TypeCode.Int32 | TypeCode.UInt32 | TypeCode.Int64 | TypeCode.UInt64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Int32->
            match( destinationCode )with
            | TypeCode.Int64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal ->
                true
            |_->
                false
    | TypeCode.UInt32->
            match( destinationCode )with
            | TypeCode.UInt32| TypeCode.UInt64| TypeCode.Single| TypeCode.Double| TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Int64->false
    | TypeCode.UInt64->
            match( destinationCode )with
            | TypeCode.Single| TypeCode.Double| TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Char->
            match( destinationCode ) with
            | TypeCode.UInt16 | TypeCode.Int32 | TypeCode.UInt32 | TypeCode.Int64 | TypeCode.UInt64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Single->
            ( destinationCode.Equals( TypeCode.Double) ) *)
    |_-> false *)



type Node =
    { ID: Guid
      Fn: obj
      InputType: Type list
      OutputType: Type
      mutable Next: Node list
      Last: Node option array }

(* let inline boxFn< ^a, ^b> (fn: ^a -> ^b) =
    (fun (x: obj) -> (x |> unbox |> fn) |> box)

let inline applyBoxedFn< ^a, ^b> (fn: ^a -> ^b) = (fun (x: obj) -> (x |> unbox |> fn))

let inline applyBoxedFn2 (fn: ^a -> ^b -> ^c) = applyBoxedFn fn

let inline boxFn2 (fn: ^a -> ^b -> ^c) =
    let boxed =
        (fun (x: obj) (y: obj) -> fn (unbox x) (unbox y) |> box)

    (fun (x: obj) -> (boxed x) |> box)

let inline unboxap (fn: ^a -> ^b) (x: obj) = fn (unbox x)
 *)

(* let inline unbox4 (fn: ^a -> ^b -> ^c -> ^d) =
    let res x = ((unboxap fn) x) |> box
    let res2 x = ((unboxap res) x) |> box
    let res3 x = ((unboxap res2) x) |> box
    res3 *)
///This function is a work of art.
/// It can be applied to a function of any length
/// if you apply it more than once to the same function you sucessivley box all its arguments and results
/// eg func=int->float->bool->float
/// let func2=boxFn12 func = obj(int)->obj( float->bool->float )
/// boxFn12 func2 = obj(int)->obj(obj(float)->obj(bool->float))
let inline boxFn (fn: ^a -> ^b) (x: obj) =
    let coDomain = fn (unbox x)
    coDomain |> box
(* let testBoxing (func: 'a->'b->'c->'d)=
    let func2=boxFn12 func
    let func3=boxFn12 func2
    let
 *)
let inline boxFnt (fn: ^a -> ^b) above (x: obj) : obj = (above (fn (unbox x))) |> box

let inline boxFn2 fn : (obj -> obj) = boxFnt fn boxFn
let inline boxFn3 fn = boxFnt fn boxFn2
let inline boxFn4 fn = boxFnt fn boxFn3
let inline boxFn5 fn = boxFnt fn boxFn4


(* let inline boxFn22 (fn: ^a -> ^b -> ^c) (x: obj) = (boxFn12 (fn (x |> unbox))) |> box

let inline boxFn32 (fn: ^a -> ^b -> ^c -> ^d) (x: obj) = (boxFn22 (fn (unbox x))) |> box
 *)
(* let inline boxer (g: ^a -> ^b) i : obj -> obj =

    (*     let res (x: obj) = ((unboxap g) x) |> box

    let reser x =
        let res (p: obj) = ((unboxap x) p) |> box
        res

    let mutable states = [ res ]

    for i in [ 0 .. i - 2 ] do
        states <- (reser states.Head) :: states

    states.Head *)
    let start f= boxFnt f boxFn12

    let mutable states = [ start ]

    for i in [ 0 .. i - 2 ] do
        printfn "%A" (states.Head.GetType())
        states <- ((fun x-> boxFnt x states.Head) ) :: states

    obj *)

///Automatically converts any **'a->'b->'c** to **obj('a)->obj( obj('b)->obj('c) )**
/// **obj('x)** is used to indicate that a vlue of type 'x is boxed
/// This is usefully if you want to call thf function using boxedCurry
(* let inline boxFnRec< ^a, ^b> (fn: ^a -> ^b) =
    let mutable times = 0

    let rec recurse (func: obj -> obj) =
        times <- times + 1
        printfn "loop %i %A" times (func.GetType())

        if times = 100 then
            failwith "to many loops somehing broke"

        match TypeShape.FromValue(func) with
        | Shape.FSharpFunc s ->
            match s.CoDomain with
            | Shape.FSharpFunc ss -> (boxFnt func recurse)
            | _ -> (boxFn12 func)
        | _ -> failwith "damn"

    let b = boxFn12 fn
    recurse b *)


let getFuncTypes (fn: ^a -> ^b) =
    let input = typeof< ^a>
    let output = typeof< ^b>
    (input, output)


type BoxedNode(fn) =
    let boxedFN = boxFn fn
    let input, output = getFuncTypes fn
    member x.ID : Guid = Guid.NewGuid()
    member x.Fn : obj -> obj = boxedFN
    member x.InputType : Type list = [ input ]
    member x.OutputType : Type = output
    member val Next: BoxedNode list = [] with get, set
    member x.Last : BoxedNode option array = Array.init 1 (fun x -> None)

let rec boxedCurry (fn: obj -> obj) (args: obj list) =
    match args with
    | [ arg ] -> fn arg
    | arg :: rest -> boxedCurry ((fn arg) :?> obj -> obj) rest
    | _ -> raise (new ArgumentException "Received an empty arg array. Cannot run function with no arguments")

let rec boxedObservableExec obsv (node: BoxedNode) =
    let osbvNext = obsv |> Observable.map node.Fn

    if node.Next.Length > 0 then
        node.Next
        |> List.collect (boxedObservableExec osbvNext)
    else
        [ osbvNext ]



let inline createNode<'T> (a: 'T) =
    match shapeof<'T> with
    | Shape.FSharpFunc x ->
        let rec getInputs (fn: IShapeFSharpFunc) inputs =
            let newInputs = fn.Domain.Type :: inputs

            match fn.CoDomain with
            | Shape.FSharpFunc x -> getInputs x newInputs
            | output -> (newInputs, output)

        let inputs, output = getInputs x []
        printfn "type:%A" x
        printfn "inputs %A" inputs
        //TODO it would be good to impliment a multibackward and multi. but for now i'm only allowing single outputs. You can decompose a tuple if thats what you want to do
        { Fn = a
          InputType = inputs
          OutputType = output.Type
          Next = []
          Last = Array.create inputs.Length None
          ID = Guid.NewGuid() }

let start a () = a

let inline createFirstNode<'a> (a: IObservable<obj>) =
    { Fn = start a
      InputType = []
      OutputType = typeof<'a>
      Next = []
      Last = Array.empty
      ID = Guid.NewGuid() }

//NOTE: it is very important that we not use the copy then update aproach of records. this will cause us to loose our refernce
let join inputNum (dest: Node) (source: Node) =
    if source.OutputType = dest.InputType.[inputNum]
       || hasImplicitConversion source.OutputType dest.InputType.[inputNum] then
        dest.Last.[inputNum] <- Some source
        source.Next <- dest :: source.Next
    else
        failwith (sprintf "OH no, the output type of %A does not match the input type of %A " source dest)

let disconnect inputNum (dest: Node) source =
    dest.Last.[inputNum] <- None
    source.Next <- source.Next |> List.except [ dest ]
