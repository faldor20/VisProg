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

//An attempt to create generic nodes where all the changes in generic types automatically propagate


(* type TemplateGenericType =
    | TSingleType of int
    | TComplexType of Type * (int array)

and TemplateSocketType =
    | TGeneric of TemplateGenericType
    | TStandard of Type

type GenericType =
    | SingleType of ref<SocketType option>
    | ComplexType of Type * ref<SocketType option> array

and SocketType =
    | Generic of GenericType
    | Standard of Type

type UnknownType = obj

let convertfromTemplate (genericTypes:SocketType option ref array) t= 
    match t with
        | TStandard a -> Standard a
        | TGeneric g ->
            match g with
            | TSingleType s -> Generic <| SingleType(genericTypes.[s])
            | TComplexType (t, s) ->
                Generic<| ComplexType(t, s |> Array.map (fun x -> genericTypes.[x]))
    
let rec resolveGenericType (genericType) =
    let resolve socketType =
        match socketType.contents with
        | Standard s -> s
        | Generic g -> resolveGenericType g

    match genericType with
    | SingleType t -> t |> (!) |> getOutputType
    | ComplexType (baseType, subtypes) ->
        let types =
            subtypes |> Array.map ((!) >> getOutputType)

        baseType.MakeGenericType(types)

and getOutputType (socket: SocketType option) =
    match socket with
    | Some socket ->
        match socket with
        | Generic a -> resolveGenericType a
        | Standard a -> a
    | None -> typeof<UnknownType>

type MiddleNodeTemplate(inputTypes, outputTypes, fn, nodeInfo) =
    inherit NodeTemplate(fn)
    member x.InputTypes: TemplateSocketType list = inputTypes
    member x.OutputType: TemplateSocketType = outputTypes
    override x.InputsCount = inputTypes.Length
    member val NodeInfo: NodeInfo = nodeInfo

type MiddleNode(template: MiddleNodeTemplate, typesList) =
    inherit Node(template)

    let last =
        Array.init (template.InputTypes.Length) (fun x -> None)

    let genericTypes: SocketType option ref array =
        template.InputTypes
        |> List.toArray
        |> Array.choose (fun t ->
            match t with
            | TStandard a -> None
            | TGeneric g ->
                match g with
                | TSingleType s -> Some [| s |]
                | TComplexType (_, s) -> Some s)
        |> Array.collect id
        |> Array.distinct
        |> Array.map (fun _ -> ref None)

    member val inputTypes: SocketType array =
        template.InputTypes
        |> List.toArray
        |> Array.map (convertfromTemplate genericTypes)
        


    member val outputSocketType: SocketType = template.OutputType|>convertfromTemplate genericTypes


    member val Last: Node option array = last
    ///Used to register a node as inputting to this socket
    ///Sets any generic types associated with this input to the types coming from the source node
    ///Assumes you've allready cheked that the input and output sockets are compatible
    member x.registerInputNode socketNum (node: MiddleNode) =
        let mySocketType = x.inputTypes.[socketNum]
        let incomingSocketType = node.outputSocketType
        let incomingType = node.outputType

        last.[socketNum] <- Some node

        match mySocketType with
        | Standard _ -> ()
        | Generic a ->
            match a with
            //In the case of a simple type just save what that generic type is so it can be used elsewhere eg: list<'a> ->'a .
            | SingleType typ -> typ := (Some incomingSocketType)
            | ComplexType (_, indexs) ->
                //TODO: i need to ensure the index of the type args is allways what i expect it to be
                
                match incomingSocketType with
                |Standard t->
                    //If the incoming type is non-generic we don't have to worry about propigating type changes through multiple nodes so we cna just make brnad new socketTypes 
                    let args = incomingType.GetGenericArguments()
                    (indexs, args)
                    ||> Array.iter2 (fun index arg -> index:= (Some<| Standard arg))
                |Generic t->
                    match t with 
                    |SingleType s->
                            

                    |ComplexType g
                    (indexs, args)
                    ||> Array.iter2 (fun index arg -> _typesList.[index] <- Some arg)
    ///Used to remove a socket connection
    /// Does things like reset the generic status of the socket
    member x.deRegisterInput socketNum =
        let socket = template.InputTypes.[socketNum]
        last.[socketNum] <- None

        match socket with
        | Standard a -> ()
        | Generic a ->
            match a with
            | SingleType index -> _typesList.[index] <- None
            | ComplexType (_, indexs) ->
                indexs
                |> Array.iter (fun index -> _typesList.[index] <- None)



    override x.outputType =
        template.OutputType |> getOutputType _typesList

    override x.inputType index =
        template.InputTypes.[index]
        |> getOutputType _typesList *)
