module VisProg.Shared.Node.GenericNode

open System

///A Complex type is a type that contains a generic type param, eg: ``list<int>`` represented as (list,[index of type param])
///a SimpleType is just the index of a type like ``int``
type GenericType =
    | SingleType of int
    | ComplexType of (Type * int array)

type SocketType =
    | Generic of GenericType
    | Standard of Type

type UnknownType = obj

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

let getOutputType typesList socket =
    match socket with
    | Generic a -> resolveGenericType typesList a
    | Standard a -> a

type MiddleNodeTemplate(inputTypes, outputTypes, fn, nodeInfo) =
    inherit NodeTemplate(fn)
    member x.InputTypes : SocketType list = inputTypes
    member x.OutputType : SocketType = outputTypes
    override x.InputsCount = inputTypes.Length
    member val NodeInfo: NodeInfo = nodeInfo

type MiddleNode(template: MiddleNodeTemplate, typesList) =
    inherit Node(template)
    let _typesList : Type option array = typesList

    let last =
        Array.init (template.InputTypes.Length) (fun x -> None)

    member val Last: Node option array = last
    ///Used to register a node as inputting to this socket
    ///Sets any generic types associated with this input to the types coming from the source node
    member x.registerInputNode socketNum (node: Node) =
        let inp = template.InputTypes.[socketNum]
        let incomingType = node.outputType
        let args = incomingType.GetGenericArguments()

        last.[socketNum] <- Some node

        match inp with
        | Standard _ -> ()
        | Generic a ->
            match a with
            | SingleType index ->
                if args.Length > 0 then
                    raise (
                        new ArgumentException "Tried to input a complex generic type into a simple genericType input"
                    )

                _typesList.[index] <- Some incomingType
            | ComplexType (_, indexs) ->
                //TODO: i need to ensure the index of the type args is allways what i expect it to be
                (indexs, args)
                ||> Array.iter2 (fun index arg -> _typesList.[index] <- Some arg)
    ///Used to remove a socket connection 
    /// Does things like reset the generic status of the socket
    member x.deRegisterInput socketNum =
        let socket= template.InputTypes.[socketNum]
        last.[socketNum]<-None

        match socket with
        |Standard a-> ()
        |Generic a->
            match a with 
            |SingleType index->
                _typesList.[index]<-None
            |ComplexType (_,indexs)->
                indexs|>Array.iter(fun index->_typesList.[index]<-None)



    override x.outputType =
        template.OutputType |> getOutputType _typesList

    override x.inputType index =
        template.InputTypes.[index]
        |> getOutputType _typesList
