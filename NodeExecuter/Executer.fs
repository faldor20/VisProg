module VisProg.Executer

open VisProg.Shared.Node.Node
open System
open System.Collections.Generic
open FSharp.Control.Reactive
open FSharp.Quotations
open VisProg.BackEnd.Node
(* type InvokeResult =
    | Success of obj
    | ObjectWasNotAFunction of Type

let trydynamicFunction (fn: obj) (args: obj seq) =
    let rec dynamicFunctionInternal (next: obj) (args: obj list) : InvokeResult =
        match args.IsEmpty with
        | false ->
            let fType = next.GetType()

            if FSharpType.IsFunction fType then
                let (head, tail) = (args.Head, args.Tail)

                let methodInfo =
                    fType.GetMethods()
                    |> Seq.filter (fun x -> x.Name = "Invoke" && x.GetParameters().Length = 1)
                    |> Seq.head

                let partalResult = methodInfo.Invoke(next, [| head |])
                dynamicFunctionInternal partalResult tail
            else
                ObjectWasNotAFunction fType
        | true -> Success(next)

    dynamicFunctionInternal fn (args |> List.ofSeq)

let dynamicFunction fn a =
    match trydynamicFunction fn a with
    | Success x -> x
    | ObjectWasNotAFunction x -> failwith "tred to invoke an obect that was not a function %A" x


///Runs a Sequence of nodes. the starting node must have Fn=Iobservable<'a>
let runner (startingNode: Node) =
    let mutable waiting =
        new Dictionary<Guid, (int * IObservable<'h>) list>()

    let input a = (a.Fn :?> (unit -> IObservable<'a>)) ()

    ///This appreach usises an up and down appreoach. we go up till a multi input node then back down untill a start to hook iup evverything beore that node
    /// A simpler approach may be to go till we find a multi input node then just add the previous node to a dictionary with the key as the multi input node
    ///then we start again at another starting node
    /// once all the starting nodes are run out we run throught the nodes stored in the dictionary
    ///repeate untill the dictionary is empty, meaning that all node paths are complete.
    let rec run (last: IObservable<'a>) lastNode (node: Node) : 'j list =
        printfn "running from node %A" node.Fn
        ///This function allwos ust to traverse to a strat and the start running again.
        /// we use this to makse sure all the inputs for multi input functions are hooked up.
        (*  let rec backwards (backNode:Node)=
            if backNode.Last.Length=0 then
                backNode.Next|>List.collect(fun next-> run (input backNode) backNode next )
            else backwards node *)
        match node.Last.Length with
        | 1 ->
            let fn a = unbox (dynamicFunction node.Fn [ a ])
            let obsv = last |> Observable.map (fn)

            let observables =
                node.Next |> List.collect (run obsv node)

            if observables.Length = 0 then
                [ obsv ]
            else
                observables

        | _ ->
            //TODO: i ned to make sure order is maintained here
            if (node.Last
                |> Array.map Option.isSome
                |> Array.contains false
                |> not) then
                let lastNodes =
                    node.Last |> Array.map (fun x -> x.Value)

                if waiting.ContainsKey node.ID then
                    waiting.[node.ID] <-
                        (lastNodes
                         |> Array.findIndex (fun x -> x.ID = lastNode.ID),
                         last)
                        :: waiting.[node.ID]

                    if waiting.[node.ID].Length = node.InputType.Length then
                        printfn "muti input node with length %i " node.Last.Length

                        let mapped =
                            waiting.[node.ID]
                            |> sortTuple
                            |> List.toArray
                            |> Observable.zipArray
                            |> Observable.map
                                (fun args ->
                                    printfn "running joined obserable with args %A" args
                                    (unbox (dynamicFunction node.Fn (args |> Seq.toList))))

                        node.Next |> List.collect (run mapped node)
                    else
                        printfn "nth hit on mulitNode"
                        []
                else
                    printfn "first hit on mulitNode adding to dic"

                    waiting.[node.ID] <-
                        [ (lastNodes
                           |> Array.findIndex (fun x -> x.ID = lastNode.ID),
                           last) ]

                    []
            else
                raise (
                    new System.ArgumentException(
                        sprintf "The node '%A' did not have all its inputs filled. inputs %A " node node.Last
                    )
                )

    printfn "starting running from node%A" startingNode

    startingNode.Next
    |> List.collect (fun next -> run (input startingNode) startingNode next)
    |> List.toArray
    |> Observable.zipArray
    |> Observable.wait
 *)

let sortTuple (a: (int * 'a) list) = a |> List.sortBy fst |> List.map snd

let checkNodeHasAllInputs (node: BoxedNode) =
    node.Last
    |> Array.map Option.isSome
    |> Array.contains false
    |> not


let runner2 (startingNode: FirstNode) =
    let mutable readyInputs =
        new Dictionary<Guid, (int * IObservable<'h>) list>()
    ///Adds the lastNode to the list of nodes that are ready to be input into this node
    ///This is becuase a node with multiple inputs must have all inputs at the ready beore it can cinute and hook up to the next node
    let addReadyInput (node: BoxedNode) (lastNodeID: Guid) lastObs =
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

        readyInputs.[node.ID] <- inputList
    /// wither recurses onwardds or returns a result if this is the final node
    let rec handleNextNodes (node: BoxedNode) osbvNext =
        if node.Next.Length > 0 then
            node.Next |> List.collect (run2 osbvNext node.ID)
        else
            [ osbvNext ]
    ///This appreach usises an up and down appreoach. we go up till a multi input node then back down untill a start to hook iup evverything beore that node
    /// A simpler approach may be to go till we find a multi input node then just add the previous node to a dictionary with the key as the multi input node
    ///then we start again at another starting node
    /// once all the starting nodes are run out we run throught the nodes stored in the dictionary
    ///repeate untill the dictionary is empty, meaning that all node paths are complete.
    and run2 (lastObs: IObservable<obj>) (lastNodeID: Guid) (node: BoxedNode) =
        printfn "running from node with inputs %A and outputs %A" node.Template.InputType node.Template.OutputType
        printfn "next nodes %A last nodes  %A" node.Next node.Last
        //First we check if we are a single or multiple input node
        match node.Last.Length with
        | 1 ->
            let osbvNext =
                lastObs |> Observable.map node.Template.Fn

            handleNextNodes node osbvNext

        | _ ->
            //If a multi-input node doesnt have a node for each input we have to crash out.
            if not (checkNodeHasAllInputs node) then
                raise
                <| new System.ArgumentException(
                    sprintf "The node '%A' did not have a node for each of its inputs. inputs %A " node node.Last
                )
            //We add the last node to the list of nodes that are ready to be input into this node
            addReadyInput node lastNodeID lastObs
            //if this is the first input we know we can just return becuase there must be at least 2
            if not (readyInputs.ContainsKey node.ID) then
                printfn "first hit on mulitNode."
                []
            else if readyInputs.[node.ID].Length = node.Template.InputType.Length then
                printfn "muti input node with length %i has all inputs ready" node.Last.Length
                //This allows us to run a function with an arbitrary number of input params
                let mapped =
                    readyInputs.[node.ID]
                    |> sortTuple
                    |> List.toArray
                    |> Observable.zipArray
                    |> Observable.map (fun args -> boxedCurry node.Template.Fn (args |> Seq.toList))

                handleNextNodes node mapped
            else
                printfn "nth hit on mulitNode"
                []


    printfn "starting running from node%A" startingNode

    startingNode.Next
    |> List.collect
        (fun next ->
            run2
                ((startingNode.Template.Fn(box ("TODO", 1, "need proper args")))
                 |> unbox)
                startingNode.ID
                next)
    |> List.toArray
    |> Observable.zipArray
    |> Observable.wait
