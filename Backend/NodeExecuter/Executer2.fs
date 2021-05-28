module VisProg.Shared.Exec


open System
open System.Collections.Generic
open VisProg.Shared.Node.FuncNode
open VisProg.BackEnd.Node
open FSharp.Control.Reactive

let sortTuple (a: (int * 'a) list) = a |> List.sortBy fst |> List.map snd
(* 
let checkNodeHasAllInputs (node: MiddleNode) =
    node.Inputs
    |> Array.map Option.isSome
    |> Array.contains false
    |> not


(* type FirstFuncNode =
    { id: Guid
      inputs: obj list
      Fn: obj -> obj
      NextNodes: Node list }

type StartingNode =
    { id: Guid
      inputs: obj list
      Fn: obj -> obj
      NextNodes: Node list }
 *)
let FuncRunner (inputs: obj array) (firsts: FirstNode list) (startingNode: StartingNode) =
    let mutable readyInputs = new Dictionary<Guid, (int * obj) list>()


    let addReadyInput (node: MiddleNode) (lastNodeID: Guid) lastObs =
        let lastNodes =
            node.Inputs |> Array.map (fun x -> x.Value)

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

    let rec handleNextNodes (node: MiddleNode) output =
        if node.Next.Length > 0 then
            node.Next
            |> List.collect (buildFunc output node.ID)
        else
            [ output ]

    and buildFunc input lastID (node: MiddleNode) =
        if node.Inputs.Length = 1 then
            let res =
                boxedCurry node.NodeData.Template.Fn [ input ]

            handleNextNodes node res
        else
            if not (checkNodeHasAllInputs node) then
                raise
                <| new System.ArgumentException(
                    sprintf "The node '%A' did not have a node for each of its inputs. inputs %A " node node.Inputs
                )
            //We add the last node to the list of nodes that are ready to be input into this node
            addReadyInput node lastID input
            //if this is the first input we know we can just return becuase there must be at least 2
            if not (readyInputs.ContainsKey node.ID) then
                printfn "first hit on mulitNode."
                []
            else if readyInputs.[node.ID].Length = node.Template.InputTypes.Length then
                printfn "muti input node with length %i has all inputs ready" node.Inputs.Length
                //This allows us to run a function with an arbitrary number of input params
                let sorted = readyInputs.[node.ID] |> sortTuple
                let res = boxedCurry node.Template.Fn sorted

                handleNextNodes node res
            else
                printfn "nth hit on mulitNode"
                []
    //First we create our observable. This migt be something like a filewatcher
    let source : IObservable<obj> =
        boxedCurry startingNode.Fn (startingNode.Inputs |> Array.toList) :?> IObservable<obj>
    //Now we run the entire node graph for each output of the obseravable
    source
    |> Observable.map
        (fun sourceOutput ->
            //First we give the observable result to any nodes that require it
            let res =
                startingNode.NodeData.Next
                |> List.map (buildFunc sourceOutput startingNode.ID)
            //The we run all the firstnodes(nodes which require no oher nodes as input)
            let res2 =
                firsts
                |> List.collect
                    (fun first ->
                        let output =
                            boxedCurry first.Fn (first.Inputs |> Array.toList)

                        first.NodeData.Next
                        |> List.map (buildFunc output first.ID))
            //This should mean all nodes have finished executing so now return any results(should be mostly just unit)
            res @ res2)
    |> Observable.wait
 *)