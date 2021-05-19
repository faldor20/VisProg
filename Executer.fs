module VisProg.Shared.Executer

open VisProg.Shared.Node
open System
open System.Collections.Generic
open FSharp.Control.Reactive

type InvokeResult =
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

let sortTuple (a: (int * 'a) list) = a |> List.sortBy fst |> List.map snd

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
