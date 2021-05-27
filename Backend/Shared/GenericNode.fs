module VisProg.Shared.Node.GenericNode

open System

///A Complex type is a type that contains a generic type param, eg: ``list<int>`` represented as (list,[index of type param])
///a SimpleType is just the index of a type like ``int``
type GenericType =
    | SingleType of int
    | ComplexType of (Type * int array)

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

type GenericNodeTemplate(inputTypes, outputTypes, fn, nodeInfo) =
    inherit NodeTemplate(fn)
    member x.InputTypes : GenericType list = inputTypes
    member x.OutputType : GenericType = outputTypes
    override x.InputsCount = inputTypes.Length
    member val NodeInfo: NodeInfo = nodeInfo

type GenericNode(template: GenericNodeTemplate, typesList) =
    inherit Node(template)
    let _typesList : Type option array = typesList

    interface MiddleNode with
        member x.registerInputNode index (node: Node) =
            let inp = template.InputTypes.[index]
            let incomingType = node.outputType
            let args = incomingType.GetGenericArguments()

            match inp with
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


    override x.outputType =
        template.OutputType
        |> resolveGenericType _typesList

    override x.inputType index =
        template.InputTypes.[index]
        |> resolveGenericType _typesList
