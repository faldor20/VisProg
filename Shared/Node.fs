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
