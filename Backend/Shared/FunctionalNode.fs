module VisProg.Shared.Node.FuncNode

open System
(* 
type GenericType =
    | SingleType of int
    | ComplexType of (Type * int array)

type UnknownType = obj

(* type GenericNodeTemplate=
    {
        InputTypes:GenericType List
        OutputType: GenericType

    }
type NonGenericNodeTemplate=
    {
        InputTypes:GenericType List
        OutputType: GenericType

    } *)

type SocketType =
    | Generic of GenericType
    | NonGeneric of Type

type NodeTemplate =
    { Fn: obj -> obj
      NodeInfo: NodeInfo
      InputTypes: SocketType list
      outputType: SocketType }
//type GenericNodeTemplate=NodeTemplate<GenericType>
//type NonGenericNodeTemplate=NodeTemplate<Type>

let getNodeInputCount (node: NodeTemplate) = node.InputTypes.Length

let resolveGenericType (typesList: Type option array) (genericType) =
    match genericType with
    | SingleType x ->
        typesList.[x]
        |> Option.defaultValue (typeof<UnknownType>)
    | ComplexType (baseType, indexs) ->
        let types =
            indexs
            |> Array.map (fun x -> typesList.[x])
            |> Array.map (Option.defaultValue (typeof<UnknownType>))

        baseType.MakeGenericType(types)
let getOutputType socket=
    match socket with
    |Generic a-> resolveGenericType
    |NonGeneric
type IHasNext=
    abstract Next:MiddleNode option array
    abstract outputType:SocketType
and NodeData =
    { ID: Guid
      Next: MiddleNode list
      Template: NodeTemplate }

and StartingNode =
    { NodeData: NodeData
      Inputs: obj array }
    member this.ID = this.NodeData.ID
    member this.Fn = this.NodeData.Template.Fn

and MiddleNode =
    { NodeData: NodeData
      Inputs: Node option array }
    member this.ID = this.NodeData.ID
    member this.Next = this.NodeData.Next
    member this.Fn = this.NodeData.Template.Fn
    member this.Template = this.NodeData.Template

type FirstNode = StartingNode

and Node =
    | MiddleNode of MiddleNode
    | FirstNode of FirstNode
    | StartingNode of StartingNode




let inline join inputnum (dest:MiddleNode) (source:NodeData)=
    let outputType = source.outputType
    let inputType = dest.inputType inputNum

    if arecompatibleTypes outputType inputType then
        
        source.Next <- dest :: source.Next
    else
        failwith (sprintf "OH no, the output type of %A does not match the input type of %A " source dest)
 *)