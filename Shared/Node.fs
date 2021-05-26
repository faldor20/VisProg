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


///A Complex type is a type that contains a generic type param, eg: ``list<int>`` represented as (list,[index of type param])
///a SimpleType is just the index of a type like ``int``
type GenericType =
    | SingleType of int
    | ComplexType of (Type * int list)



type NodeInfo =
    { Name: string
      Description: String
      InputNames: string list
      OutputNames: string list }

type NodeTemplate() =
    class
    end


type GenericNodeTemplate(inputTypes, outputTypes, typesList) =
    inherit NodeTemplate()
    let _inputTypes : GenericType list = inputTypes
    let _outputType : GenericType = outputTypes


type MiddleNodeTemplate(fn, input, output, nodeInfo) =
    inherit NodeTemplate()
    member x.Fn : obj -> obj = fn
    member x.InputType : Type list = input
    member x.OutputType : Type = output
    member val NodeInfo: NodeInfo = nodeInfo

type FirstNodeTemplate(fn, input, output, nodeInfo) =
    inherit MiddleNodeTemplate(fn, input, output, nodeInfo)

[<AbstractClass>]
type Node(template) =
    member val ID: Guid = Guid.NewGuid()
    member val Template: NodeTemplate = template
    abstract member outputType : Type
    abstract member inputType : int -> Type

type BoxedNode(template: MiddleNodeTemplate) =
    inherit Node(template)

    let last =
        Array.init (template.InputType.Length) (fun x -> None)

    override x.outputType = template.OutputType
    override x.inputType index = template.InputType.[index]
    member val Next: BoxedNode list = [] with get, set
    member val Last: BoxedNode option array = last
    member x.registerInputNode inputNum node = last.[inputNum] <- Some node

type FirstNode(template: FirstNodeTemplate) =
    inherit BoxedNode(template)

type GenericNode(template) =
    inherit BoxedNode(template)
    let nodeTypes : Type array = [||]
///The first node is differnet from the others beuase it's function must return an observable
/// Any inputs to the function will be rendered as a text box for you to enter input
//NOTE: it is very important that we not use the copy then update aproach of records. this will cause us to loose our refernce
