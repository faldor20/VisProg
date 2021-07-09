module VisProg.Shared.Node.Funcs

open System
open Node


let rec compairGenericTypes (a:Type) (b:Type)=
    let destArgs = a.GetGenericArguments()
    let sourceArgs = b.GetGenericArguments()

    let allArgsMatch=
        ((sourceArgs,destArgs)||>Array.zip)
        |> Array.forall (fun (x,y)->
        //We must test if a type is a generictype(a type that takes asome kindof arg such as list<_> or dict<_,_>) and if so recurse
        match x.IsGenericType||y.IsGenericType with
        |true->  areCompatibleTypes x y
        |false-> x=typeof<UnknownType> || y=typeof<UnknownType> || x=y )
    
    let sameBase=a.GetGenericTypeDefinition()=b.GetGenericTypeDefinition()
    sameBase && allArgsMatch
/// Checks if the two types are compatible. That being, either the same (int=int), generic(int='a||list<int>=list<'a>) or assignable(ParentType =ChildType)   
and areCompatibleTypes (source: Type) (dest: Type) =
    //TODO: i need to see if a generic type like list<int> will fit into an input of a genericNode whe the input ype is unknonw and the type returned is list<UnknownType>
    source = dest
    || dest.IsAssignableFrom source
    || hasImplicitConversion source dest
    || dest = typeof<UnknownType>
    || compairGenericTypes source dest


let join inputNum (dest: MiddleNode) (source: Node) =
    let outputType = source.outputType
    let inputType = dest.inputType inputNum

    if areCompatibleTypes outputType inputType then
        dest.registerInputNode inputNum source
        source.Next <- (dest :> Node) :: source.Next
    else
        failwith (sprintf "OH no, the output type of %A does not match the input type of %A " source dest)

let disconnect inputNum (dest: MiddleNode) (source:Node) =

    dest.deRegisterInput inputNum
    source.Next <- source.Next |> List.except [ dest ]
