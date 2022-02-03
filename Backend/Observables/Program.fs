
module Test
open FSharp
open System
open System.Collections.Generic
open FSharp.Control
open FSharp.Control.Reactive
open VisProg.Shared.Node
open TypeShape
open TypeShape.Core



(* 
let firstObservable a=
    Observable.ofSeq([2;4;6;8])
let second a=
    Observable.ofSeq([3;4;5;7;9])


let FuncRunner (firsts: FirstNode list) (startingNode: StartingNode) =
    let mutable readyInputs = new Dictionary<Guid, (int * obj) list>()


    (* let addReadyInput (node: MiddleNode) (lastNodeID: Guid) lastObs =
        let lastNodes =
            node.Last |> Array.map (fun x -> x.Value)

        let thisInputIndex =
            printfn
                "last id's : %A last ID: %A thisID : %A lastids "
                (lastNodes |> Array.map (fun x -> x.ID))
                lastNodeID
                node.ID


            lastNodes
            |> Array.findIndex (fun x -> x.ID = lastNodeID)

        let thisInput = (thisInputIndex, lastObs)

        let inputList =
            if (readyInputs.ContainsKey node.ID) then
                thisInput :: readyInputs.[node.ID]
            else
                [ thisInput ]

        readyInputs.[node.ID] <- inputList *)

    let rec handleNextNodes (node: MiddleNode) output =
        if node.Next.Length > 0 then
            node.Next
            |> List.collect (buildFunc output node.ID)
        else
            [ output ]

    and buildFunc (input:IObservable<'a>) lastID (node: Node) =
        match node with
        | :? MiddleNode as node ->
            //Single input
            if node.Last.Length = 1 then
                let res = Observable.map boxedCurry node.Template.Fn [ input ]
                handleNextNodes node res
            //MultiInput
            else
                if not (checkNodeHasAllInputs node) then
                    raise
                    <| new System.ArgumentException(
                        sprintf "The node '%A' did not have a node for each of its inputs. inputs %A " node node.Last
                    )
                //We add the last node to the list of nodes that are ready to be input into this node
                addReadyInput node lastID input
                //if this is the first input we know we can just return becuase there must be at least 2
                if not (readyInputs.ContainsKey node.ID) then
                    printfn "first hit on mulitNode."
                    []
                else if readyInputs.[node.ID].Length = node.Template.InputsCount then
                    printfn "muti input node with length %i has all inputs ready" node.Last.Length
                    //This allows us to run a function with an arbitrary number of input params
                    let sorted = readyInputs.[node.ID] |> sortTuple
                    let res = boxedCurry node.Template.Fn sorted

                    handleNextNodes node res
                else
                    printfn "nth hit on mulitNode"
                    []
    //First we create our observable. This migt be something like a filewatcher
    let source : IObservable<obj> =
        boxedCurry startingNode.Template.Fn (startingNode.inputs |> Array.toList) :?> IObservable<obj>
    //Now we run the entire node graph for each output of the obseravable
    
    //First we give the observable result to any nodes that require it
    let res =
        startingNode.Next
        |> List.map (buildFunc source startingNode.ID)
    (* |> Observable.map
        (fun sourceOutput ->
            
            //The we run all the firstnodes(nodes which require no oher nodes as input)
            let res2 =
                firsts
                |> List.collect
                    (fun first ->
                        let output =
                            boxedCurry first.Template.Fn (first.inputs |> Array.toList)

                        first.Next |> List.map (buildFunc output first.ID))
            //This should mean all nodes have finished executing so now return any results(should be mostly just unit)
            res @ res2) *)
    |> Observable.wait
 *)
let  findGenerics (fn: ^T -> ^U) =

    match shapeof< ^T ->  ^U> with
    | Shape.FSharpFunc x ->
        let rec getInputs (fn: IShapeFSharpFunc) inputs ( genericTypes:int)=
            let fnType=fn.Domain.Type
            printfn "is genereic %A" fnType.IsGenericType
            printfn "is genericParamater %A" fnType.IsGenericParameter
            printfn "is genericmethodParam %A" fnType.IsGenericMethodParameter
            printfn "is genericTypeDef %A" fnType.IsGenericTypeDefinition
            let mutable genericTypes=genericTypes
            let newInputs =
                if fnType.IsGenericType then
                    let next=(SocketType.Generic<|GenericType.SingleType genericTypes)::inputs
                    genericTypes<-genericTypes+1
                    next
                else
                    (SocketType.Standard fnType) :: inputs
            
            match fn.CoDomain with
            | Shape.FSharpFunc x -> getInputs x newInputs genericTypes
            | output -> (newInputs, output)
        
        getInputs x [] 0

    
(* let inline createMiddleNodeTemplate< ^T ,^U> (fn: ^T -> ^U) boxer (description: string) (outputName: string) =
    
        //let boxedFunc = anyboxer fn (inputs.Length)
        let boxedFunc = boxer fn
        printfn "type:%A" x
        printfn "inputs %A" inputs
        let (funcName, funcInputs) = DocumentGetter.GetInfo(fn)

        let nodeInfo =
            { Name = funcName
              InputNames = (Array.toList funcInputs)
              Description = (description)
              OutputNames = [ (outputName) ] }
        //TODO it would be good to impliment a multibackward and multi. but for now i'm only allowing single outputs. You can decompose a tuple if thats what you want to do
        //TODO: i must be careful when mkaing an instance of a node to make sure it dosn't end up with a shared refence
        MiddleNodeTemplate( inputs, SocketType.Standard output.Type,boxedFunc, nodeInfo)
    | _ -> failwith "this should be impossible, yoou should never be able to pass anything but a function in as fn"let
 *)
(* let test (a:'a) (b:'b)=
    printfn "%A, %A"a b
    (a,b)
let listTest a b=
    a@b *)
let fn  (p1:'a) (p2:'a) (p3:'b)(p4:list<'b>)=
    p1
let getparams (fn2: 'U-> 'T)=
    let typ= fn2.GetType()
    let dcl=fn2.GetType().DeclaringType
    printfn "dcl %A Name: %A"dcl (fn2.GetType().FullName)
    let meth=dcl.GetMethod(nameof fn2)
    printfn "meth %A" meth
    let parm=meth.GetParameters()
    ()
let id2 ba=
    ba
[<EntryPoint>]
let main args=
    let parms= getparams id2
    //let inf=a.GetType().GetMethod(nameof a)
    let info=fn.GetType().DeclaringType.GetMethod(nameof fn)
    printfn "%A" info
    printfn "%A" (info.GetParameters()|>Array.map(fun x->x.ParameterType))
    let param= info.GetParameters()
    //param[0].ParameterType.generic
(*     let info=a.GetType().DeclaringType.GetMethod(nameof a)
    let params=info.GetParameters()
    info.MakeGenericMethod() *)
    //findGenerics test
    0
