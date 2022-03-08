module VisProg.Executer

open VisProg.Shared.Node.Node
open System
open System.Collections.Generic
open FSharp.Control.Reactive
open FSharp.Quotations
open VisProg.BackEnd.Node
open VisProg.Shared.Node
open Utils.QuotationHelpers
open FSharp.Quotations.Evaluator
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
*)

let sortTuple (a: (int * 'a) list) = a |> List.sortBy fst |> List.map snd

let checkNodeHasAllInputs (node: MiddleNode) =
    node.Last
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
(* let FuncRunner (inputs: obj array) (firsts: MiddleNode list) (startingNode: MiddleNode) =
    let mutable readyInputs = new Dictionary<Guid, (int * obj) list>()


    let addReadyInput (node: MiddleNode) (lastNodeID: Guid) lastObs =
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

    let rec handleNextNodes (node: MiddleNode) output =
        if node.Next.Length > 0 then
            node.Next
            |> List.collect (buildFunc output node.ID)
        else
            [ output ]

    and buildFunc input lastID (node: Node) =
        match node with
        | :? MiddleNode as node ->
            if node.Last.Length = 1 then
                let res = boxedCurry node.Template.Fn [ input ]
                handleNextNodes node res
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
    source
    |> Observable.map
        (fun sourceOutput ->
            //First we give the observable result to any nodes that require it
            let res =
                startingNode.Next
                |> List.map (buildFunc sourceOutput startingNode.ID)
            //The we run all the firstnodes(nodes which require no oher nodes as input)
            let res2 =
                firsts
                |> List.collect
                    (fun first ->
                        let output =
                            boxedCurry first.Template.Fn (first.inputs |> Array.toList)

                        first.Next |> List.map (buildFunc output first.ID))
            //This should mean all nodes have finished executing so now return any results(should be mostly just unit)
            res @ res2)
    |> Observable.wait
 *)


let runner2 (startingNodes: MiddleNode list) =
    let mutable readyInputs =
        new Dictionary<Guid, (int * IObservable<'h>) list>()
    ///Adds the lastNode to the list of nodes that are ready to be input into this node
    ///This is becuase a node with multiple inputs must have all inputs at the ready beore it can cinute and hook up to the next node
    let addReadyInput (node: MiddleNode) (lastNodeID: Guid) lastObs =
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
    let rec handleNextNodes (node: MiddleNode) osbvNext =

        if node.Next.Length > 0 then
            node.Next |> List.collect (fun x->run2 osbvNext node.ID (x:?>MiddleNode))
        else
            [ osbvNext ]
    ///This appreach usises an up and down appreoach. we go up till a multi input node then back down untill a start to hook iup evverything beore that node
    /// A simpler approach may be to go till we find a multi input node then just add the previous node to a dictionary with the key as the multi input node
    ///then we start again at another starting node
    /// once all the starting nodes are run out we run throught the nodes stored in the dictionary
    ///repeate untill the dictionary is empty, meaning that all node paths are complete.
    and run2 (lastObs: IObservable<obj>) (lastNodeID: Guid) (node: MiddleNode) =
        printfn "running from node with inputs %A and outputs %A" node.Template.InputsCount node.outputType
        printfn "next nodes %A last nodes  %A" node.Next node.Last
        //First we check if we are a single or multiple input node
        printfn"thera are %d previous nodes" node.Last.Length
        match node.Last.Length with
        | 1 ->
            printfn "mapping on function"
            let osbvNext =
                lastObs |> Observable.map( fun x-> printfn "running a function with an observable ";node.Template.Fn [|x|])

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
            (* if not (readyInputs.ContainsKey node.ID) then
                printfn "first hit on mulitNode."
                []
            else *) 
            if readyInputs.[node.ID].Length = node.Template.InputsCount then
                printfn "Multi input node with length %i has all inputs ready" node.Last.Length
                //This allows us to run a function with an arbitrary number of input params
                let mapped =
                    readyInputs.[node.ID]
                    |> sortTuple
                    |> List.toArray
                    |> Observable.zipArray
                    |> Observable.map (fun args ->  node.Template.Fn (args |> Seq.toArray))

                handleNextNodes node mapped
            else
                printfn "nth hit on mulitNode"
                []


    printfn "starting running from node%A" startingNodes
    let obsv=
        startingNodes
        |>List.collect( fun startNode->
            startNode.Next
            |> List.collect
                (fun next ->
                    let ob = startNode.Template.Fn(startNode.inputs)
                    let iob=ob|>unbox
                    run2
                        iob
                        startNode.ID
                        (next:?>MiddleNode))
        )
        
        |> List.toArray
        |> Observable.mergeArray
    printfn "!!!Build observable graph. wating for execution to complete"
    obsv|>Observable.wait


    
    

(* let rec createExpression (last:Expr) (node:MinNode)=
    
    //we would tuple the sockets together becuase tuples are used to combine when Observable.zip
    let types=[|(typedefof<(obj *obj) >).MakeGenericType(node.inType);node.outType|]
    let map=Omap.MakeGenericMethod(types)
    let nodeFn=node.methodInfo.MakeGenericMethod([|node.inType[0];node.inType[1];node.outType|]) 
    Expr.Call(map,[last;Expr.Call(nodeFn,[])]) *)


let runner3 (startingNodes: MiddleNode list) =
    let Omap=(funInfo <@Observable.map@>)
    let Omap=Omap.DeclaringType.GetMethod(Omap.Name)
    
    let mutable readyInputs =
        new Dictionary<Guid, (int * Expr) list>()
    ///Adds the lastNode to the list of nodes that are ready to be input into this node
    ///This is becuase a node with multiple inputs must have all inputs at the ready beore it can cinute and hook up to the next node
    let addReadyInput (node: MiddleNode) (lastNodeID: Guid) lastObs =
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
    let rec handleNextNodes (node: MiddleNode) osbvNext =

        if node.Next.Length > 0 then
            node.Next |> List.collect (fun x->run3 osbvNext node.ID (x:?>MiddleNode))
        else
            [ osbvNext ]
    ///This appreach usises an up and down appreoach. we go up till a multi input node then back down untill a start to hook iup evverything beore that node
    /// A simpler approach may be to go till we find a multi input node then just add the previous node to a dictionary with the key as the multi input node
    ///then we start again at another starting node
    /// once all the starting nodes are run out we run throught the nodes stored in the dictionary
    ///repeate untill the dictionary is empty, meaning that all node paths are complete.
    and run3 (lastObs: Expr) (lastNodeID: Guid) (node: MiddleNode) =
        printfn "running from node with inputs %A and outputs %A" node.Template.InputsCount node.outputType
        printfn "next nodes %A last nodes  %A" node.Next node.Last
        //First we check if we are a single or multiple input node
        printfn"there are %d previous nodes" node.Last.Length
        let nodeGenericTypes=node.typesList|>Array.map(fun x->x.Value)
        let nodeMethod=node.generateMethod()

        let nodeInputs=nodeMethod.GetParameters()|>Array.map(fun x->x.ParameterType)
        let nodeoutPut=node.outputType
        let safeout=
            if node.outputType = typeof<System.Void> then
                [| typeof<unit> |]
            else [|node.outputType|] 
             
        match node.Last.Length with
        | 1 ->
            printfn "mapping on function"
            let oMapType= Omap.MakeGenericMethod(Array.append nodeInputs safeout)
            let osbvNext =Expr.Call(oMapType,[makeLambda nodeMethod ;lastObs])
                //lastObs |> Observable.map( fun x-> printfn "running a function with an observable ";node.Template.Fn [|x|])

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
            (* if not (readyInputs.ContainsKey node.ID) then
                printfn "first hit on mulitNode."
                []
            else *) 
            if readyInputs.[node.ID].Length = node.Template.InputsCount then
                printfn "Multi input node with length %i has all inputs ready" node.Last.Length
                //This allows us to run a function with an arbitrary number of input params
                //collect and order our incoming Observables
                let mapped =
                    readyInputs.[node.ID]
                    |> sortTuple
                //Make a tuple that will hold the outputs of the incoming Observables
                let tupleType= Utils.makeTupleType nodeInputs
                let zipper=Utils.getZipper nodeInputs
                //Make a zip expression them all together
                let zipped=Expr.Call(zipper,mapped)
                let omap=Omap
                let oMapType= Omap.MakeGenericMethod(Array.append [|tupleType|]safeout)
                //we use arrApl
                let obs=Expr.Call(oMapType,[tupleLambda nodeMethod tupleType; zipped] )
                    (* |> Observable.zipArray
                    |> Observable.map (fun args ->  node.Template.Fn (args |> Seq.toArray)) *)

                handleNextNodes node obs
            else
                printfn "nth hit on mulitNode"
                []


    printfn "starting running from node%A" startingNodes
    let timer=Diagnostics.Stopwatch.StartNew()
    let obsv=
        let graph=
            startingNodes
            |>List.collect( fun startNode->
                startNode.Next
                |> List.collect
                    (fun next ->
                    //    let ob = startNode.Template.Fn(startNode.inputs)
                        let meth=startNode.generateMethod()
                        let iob=Expr.Call(meth,[Expr.Value(startNode.inputs[0],meth.GetParameters().[0].ParameterType)])
                        run3
                            iob
                            startNode.ID
                            (next:?>MiddleNode))
                )
        let took=timer.Elapsed
        printfn "making graph took %A ms" took.TotalMilliseconds
        graph
        |>List.map(fun x->x|>QuotationEvaluator.EvaluateUntyped:?>IObservable<unit>)
        |> List.toArray
        |> Observable.mergeArray
    timer.Stop()
    printfn "!!!Build observable graph in %f ms. Waiting for execution" timer.Elapsed.TotalMilliseconds
    timer.Restart()
    obsv|>Observable.wait
    timer.Stop()
    printfn "!!!Execution finshed in %f" timer.Elapsed.TotalMilliseconds
 







