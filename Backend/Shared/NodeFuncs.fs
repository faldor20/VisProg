module VisProg.Shared.Node.Funcs

open System
open Node


let rec compairGenericTypes (a:Type) (b:Type)=
    printfn "compairing type %A with %A" a b
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

///This should be integrated into checkandUpadteDownStreamNodes
let rec updateDownSteamNodes (dest:MiddleNode)= 
    dest.Next
    |>List.iter(fun nextNode ->
        let socketNum=nextNode.Last|>Array.findIndex(fun x->x|>Option.map ((=)dest)|>Option.defaultValue false )
        nextNode.registerInputNode socketNum dest
    )

///Recursivley checks if any downstream node is rendered incompatible by the chnage to a generic we are making.
///If all nodes checkout the 
///stops recursing when a non-genric input is reached becuase that means the generic is no longer being propigated
let rec checkandUpdateDownStreamNodes (dest:MiddleNode) typesList= 
    let outputType=dest.outputType2 typesList
    let res=
        dest.Next
        |>List.map(fun nextNode ->
            let socketNum=nextNode.Last|>Array.findIndex(fun x->x|>Option.map ((=)dest)|>Option.defaultValue false )
            let socket=nextNode.template.InputTypes[socketNum]
            match socket with
            |Generic _->
                let compatible=areCompatibleTypes outputType  (nextNode.inputType socketNum)
                if compatible then
                    let newTypesList=(nextNode.getNewTypesList socketNum  outputType) 
                    
                    //TODO: make a version of this that actaully saves all the newTypesLists then checks if tehre were anyincompatibilities, then applies all the newTypeslLists  
                    //This breaks tail recursion!
                    checkandUpdateDownStreamNodes nextNode newTypesList
                    //if downStreamCorrect then 
                        //nextNode.updateTypesList newTypesList
                    
                else compatible
            |Standard _->true )
        |>List.contains false
        |>not
    //We now update the typesLists of the Nodes 
    if res then updateDownSteamNodes dest
    res

let join socketIndex (dest: MiddleNode) (source: MiddleNode) =
    let outputType = source.outputType
    let inputType = dest.inputType socketIndex

    let typesList= dest.getNewTypesList socketIndex inputType
    let res=checkandUpdateDownStreamNodes dest typesList
    if not res then failwithf "downstream type not compatible with the change in generics this connection would incur."
    if areCompatibleTypes outputType inputType then
        dest.registerInputNode socketIndex source
        source.Next <- (dest :> MiddleNode) :: source.Next
    else
        failwith (sprintf "OH no, the output type of %A does not match the input type of %A " source dest)
    
let disconnect inputNum (dest: MiddleNode) (source:MiddleNode) =

    dest.deRegisterInput inputNum
    source.Next <- source.Next |> List.except [ dest ]
