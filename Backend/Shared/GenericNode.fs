[<AutoOpen>]
module VisProg.Shared.Node.Mid

open System
open FSharpx.Collections
///A Complex type is a type that contains a generic type param, eg: ``list<int>`` represented as (list,[index of type param])
///a SimpleType is just the index of a type like ``int``
//TODO: test if this properly allows for soemthing like Dictionary<'b,list<'a>>
type GenericType =
    | SingleType of int
    | ComplexType of (Type * SocketType array )

and SocketType =
    | Generic of GenericType
    | Standard of Type

type UnknownType = obj

let rec resolveType (typesList: Type option array) (genericTypes: SocketType) =
    match genericTypes with
    |Standard s->s
    |Generic g->
        match g with
        | SingleType x ->
            typesList.[x]
            |> Option.defaultValue (typeof<UnknownType>)
        | ComplexType (baseType, indexs) ->
            let types =
                indexs
                |> Array.map( resolveType typesList)
            baseType.MakeGenericType(types)


let updateTypesList (typesList:Type option array) mySocketType (incomingType:Type)=
    //We return a new copy to makes sure immutability is maintained
    //This is so we can simulate what a type change wuold do to downsream nodes
    let typesList =typesList.Clone() :?>(Type option array)
    
    let args = incomingType.GetGenericArguments()


    //We already know that the incoming type matches the type of the socket. And so they are structured the same
    //So as we destructure the socket we can destruture the args along with it and assign generics as they come
    let rec dissasembleType (typ:Type)  (mySocket: SocketType) =
        match mySocket with
        |Standard _->()
        |Generic g->
            match g with
            | SingleType index ->
                if args.Length > 0 then
                    raise (
                        new ArgumentException "Tried to input a complex generic type into a simple genericType input"
                    )
                typesList.[index] <- Some incomingType 

            | ComplexType (baseType, sockets) ->
                let args=typ.GetGenericArguments()
                ( args,sockets)||> Array.iter2 dissasembleType

    dissasembleType incomingType mySocketType
    typesList
type MiddleNodeTemplate(inputTypes, outputTypes, fn, nodeInfo) =
    inherit NodeTemplate(fn)
    member x.InputTypes : SocketType list = inputTypes
    member x.OutputType : SocketType = outputTypes
    override x.InputsCount = inputTypes.Length
    member val NodeInfo: NodeInfo = nodeInfo

type MiddleNode(template: MiddleNodeTemplate, typesList) =
    inherit Node(template)
    let mutable _typesList : Type option array = typesList

    let last =
        Array.init (template.InputTypes.Length) (fun x -> None)
    member val Next: MiddleNode list = [] with get, set
    member val template=template
    member val Last: MiddleNode option array = last
    member x.inputs : obj array = [||] //This needs to be a hashmap or map with int as keys each int representing a paricular socket
    member x.getNewTypesList socketNum incomingType=
        let mySocketType = template.InputTypes.[socketNum]
        updateTypesList _typesList mySocketType incomingType
    ///Used to register a node as inputting to this socket
    ///Sets any generic types associated with this input to the types coming from the source node
    member x.registerInputNode socketNum (node: MiddleNode) =

        last.[socketNum] <- Some node
        _typesList<- (x.getNewTypesList socketNum node.outputType)
        
    ///Used to remove a socket connection 
    /// Does things like reset the generic status of the socket
    member x.deRegisterInput socketNum =
        let socket= template.InputTypes.[socketNum]
        last.[socketNum]<-None
        let rec removeGenerics  (mySocket: SocketType) =
            match mySocket with
            |Standard _->()
            |Generic g->
                match g with
                | SingleType index ->typesList.[index] <- None
                | ComplexType (baseType, sockets) ->
                    ( sockets)|> Array.iter removeGenerics
        removeGenerics socket



    override x.outputType =
        template.OutputType |> resolveType _typesList

    override x.inputType index =
        template.InputTypes.[index]
        |> resolveType _typesList
    member x.outputType2 typesList =
        template.OutputType |> resolveType typesList

    member x.inputType2 typesList index  =
        template.InputTypes.[index]
        |> resolveType typesList
    member x.typesList=_typesList
    member x.updateTypesList list= _typesList<-list
