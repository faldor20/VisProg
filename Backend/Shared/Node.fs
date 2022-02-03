[<AutoOpen>]
module VisProg.Shared.Node.Node

open System
open FSharp.Reflection
open FSharp.Core.OptimizedClosures

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

    if destination = typeof<obj> then
        true
    else
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



(* type Node =
    { ID: Guid
      Fn: obj
      InputType: Type list
      OutputType: Type
      mutable Next: Node list
      Last: Node option array } *)



type NodeInfo =
    { Name: string
      Description: String
      InputNames: string list
      OutputNames: string list }

[<AbstractClass>]
type NodeTemplate(fn) =
    class
        member x.Fn : obj -> obj = fn
        abstract member InputsCount : int
    end

type NonGenericNodeTemplate(fn, input, output, nodeInfo) =
    inherit NodeTemplate(fn)
    member x.InputType : Type list = input
    member x.OutputType : Type = output
    override x.InputsCount = input.Length
    member val NodeInfo: NodeInfo = nodeInfo

type StartingNodeTemplate(fn, input, output, nodeInfo) =
    inherit NonGenericNodeTemplate(fn, input, output, nodeInfo)

type FirstNodeTemplate(fn, input, output, nodeInfo) =
    inherit NonGenericNodeTemplate(fn, input, output, nodeInfo)

[<AbstractClass>]
type Node(template) =
    member val ID: Guid = Guid.NewGuid()
    member val Template: NodeTemplate = template
    
    abstract member outputType : Type
    abstract member inputType : int -> Type

[<AbstractClass>]
type BeginningNode(template: NonGenericNodeTemplate) =
    inherit Node(template)
    override x.outputType = template.OutputType
    override x.inputType index = template.InputType.[index]
    member x.inputs : obj array = [||]

type StartingNode(template: StartingNodeTemplate) =
    inherit BeginningNode(template)

type FirstNode(template: FirstNodeTemplate) =
    inherit BeginningNode(template)

///The first node is differnet from the others beuase it's function must return an observable
/// Any inputs to the function will be rendered as a text box for you to enter input
//NOTE: it is very important that we not use the copy then update aproach of records. this will cause us to loose our refernce


///An alternative way to make the functions that doesnt require as much boxing and unboxing and preserves the type infomation
///possibly allowing for pattern matching
type testFunc =
    abstract member RunFunc : (obj list) -> obj

type testFunc1<'a, 'b>(fn) =
    let func : 'a -> 'b = fn

    interface testFunc with
        member x.RunFunc args = (fn (unbox args.[0])) |> box

type testFunc2<'a, 'b, 'c>(fn) =
    let func : 'a -> 'b -> 'c = fn

    interface testFunc with
        member x.RunFunc args =
            (fn (unbox args.[0]) (unbox args.[1])) |> box
