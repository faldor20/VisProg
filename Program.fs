// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open TypeShape.Core
open FSharp.Control.Reactive
open System.Collections.Generic
//type Node=
//|Single of SingleNode
//|MultiForward of NodeBase<Type list,Node list ,Node >
//|MultiBackward of NodeBase<Type list,Node , Node List >
//|Multi of NodeBase<Type list ,Node list , Node list >
//|End of NodeBase<Type,bool,Node>
//|Start of NodeBase<Type,Node,bool >
//and SingleNode=
//    {
//    Fn:obj
//    InputType:Type
//    OutputType:Type
//    next:Node
//    Last:Node 
//    }
//and MultiForward=
//    {
//    Fn:obj
//    InputType:Type
//    OutputType:Type
//    next:Node List
//    Last:Node 
//    }
//and MultiBackward=
//    {
//    Fn:obj
//    InputType:Type List
//    OutputType:Type
//    next:Node
//    Last:Node List 
//    }
//and Multi=
//    {
//    Fn:obj
//    InputType:Type List
//    OutputType:Type
//    next:Node List
//    Last:Node List
//    }
//and End=
//    {
//    Fn:obj
//    InputType:Type List
//    Last:Node List
//    }
//and Start=
//    {
//    Fn:obj
//    OutputType:Type
//    next:Node List
//    }
type Node ={
    Fn:obj
    InputType:Type list
    OutputType:Type
    mutable Next: Node list 
    Last:Node option array 
}
let createNode<'T > (a:'T)  =
    match shapeof<'T> with
    |Shape.FSharpFunc x->
        let rec getInputs (fn:IShapeFSharpFunc) inputs=
            let newInputs=fn.Domain.Type::inputs
            match fn.CoDomain with
            | Shape.FSharpFunc x->
                    getInputs x newInputs
            |output->(newInputs,output)
        let inputs,output=getInputs x []
        printfn "type:%A"x
        printfn "inputs %A"inputs
        //TODO it would be good to impliment a multibackward and multi. but for now i'm only allowing single outputs. You can decompose a tuple if thats what you want to do
        {Fn=a;InputType=inputs;OutputType=output.Type;Next=[];Last=Array.create inputs.Length None}
let start a ()=a
let createFirstNode<'a> (a:IObservable<obj>)=
    {Fn=start a;InputType=[];OutputType=typeof<'a>;Next=[];Last=Array.empty;}

//NOTE: it is very important that we not use the copy then update aproach of records. this will cause us to loose our refernce
let join inputNum  (dest:Node) (source:Node) =
    if source.OutputType.IsAssignableTo(dest.InputType.[inputNum]) then
        dest.Last.[inputNum]<-Some source
        source.Next<-dest::source.Next 
    else failwith (sprintf"OH no, the output type of %A does not match the input type of %A " source dest)
let disconnect inputNum  dest source =
    dest.Last.[inputNum]<-None
    source.Next<- source.Next|>List.except [dest]

let add a b =
    a+b
let multiply a b=
    a*b
let plus1 a=
    a+1
let cos (a:Int64)=
    a|>float|>Math.Cos|>int
let ender=createNode (printfn "from the observable%A") 
let middle = createNode cos

let starter= createFirstNode<Int64> ( (Observable.timerSpanPeriod(TimeSpan.FromSeconds(2.0),TimeSpan.FromSeconds(1.0)))|>Observable.map(fun x->x:>obj))  
starter|>join 0 middle 
middle|>join 0 ender

let rec curry (f:obj) a=
    match a with 
    |[]->
        f
    |head::tail->
        let fn=f:?> ('a->'b)
        curry (fn head) tail
let sortTuple (a :( int*'a )list)=
    a
    |>List.sortBy fst
    |>List.map snd

type ScheduleStatus<'T>=
    |Waiting of (Node*IObservable<'T> list)
    |Scehduled of IObservable<'T>
let runner (startingNode:Node)=
    let mutable waiting= new Dictionary<Node,(int*IObservable<'h>)list>()
    let input a= (a.Fn:?>(unit->IObservable<'a>))()

    ///This appreach usises an up and down appreoach. we go up till a multi input node then back down untill a start to hook iup evverything beore that node
    /// A simpler approach may be to go till we find a multi input node then just add the previous node to a dictionary with the key as the multi input node
    ///then we start again at another starting node
    /// once all the starting nodes are run out we run throught the nodes stored in the dictionary
    ///repeate untill the dictionary is empty, meaning that all node paths are complete.
    let rec run (last:IObservable<'a>) lastNode (node:Node):'j list =
        printfn "running from node %A"node
        ///This function allwos ust to traverse to a strat and the start running again.
        /// we use this to makse sure all the inputs for multi input functions are hooked up.
        let rec backwards (backNode:Node)=
            if backNode.Last.Length=0 then
                backNode.Next|>List.collect(fun next-> run (input backNode) backNode next ) 
            else backwards node
        match node.Last.Length with
        |1->
            let obsv=last|>Observable.map (unbox node.Fn)
            node.Next
            |>List.collect (run obsv node )
            
        |_ -> 
            //TODO: i ned to make sure order is maintained here
            if (Array.TrueForAll(node.Last,(fun x->true))) then 
                let lastNodes= node.Last|>Array.map(fun x->x.Value)
                
                if waiting.ContainsKey node then 
                    if waiting.[node].Length = node.InputType.Length-1 then
                        let mapped=
                            waiting.[node]
                            |>sortTuple
                            |>List.toArray
                            |>Observable.zipArray
                            |>Observable.map (fun args->(curry node.Fn (args|>Seq.toList)))
                        node.Next|>List.collect(run mapped node)
                    else 
                        waiting.[node]<-(lastNodes|>Array.findIndex(fun x->x=lastNode),last)::waiting.[node]
                        []
                else 
                    waiting.[node]<-(lastNodes|>Array.findIndex(fun x->x=lastNode),last)::waiting.[node]
                    lastNodes|>Array.toList|>List.collect(backwards)
            else 
                raise (new System.ArgumentException(sprintf "The node '%A' did not have all its inputs filled. inputs %A " node node.Last))
                
    printfn "starting running from node%A" startingNode
    startingNode.Next|>List.collect(fun next-> run (input startingNode) startingNode next )
    |>List.iter(fun x->x|>Observable.wait)

            
            
            


// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    

    let a (b:int)=b
    match shapeof<bool->int->float> with
    //the domain is the input type, the co-domain is the output type
    // i can use this to generate my node info
    |Shape.FSharpFunc x -> 
        printfn "domain:%A" x.Domain
        printfn "codomain:%A" x.CoDomain
    runner starter
    //as we can see her the domain is allways the first input type and the codomain is the rest. This is becase of currying.
    // this means that a simple way of implimenting multi-input nodes is to just nest two nodes inside one another
    //the nodes are allways doing a kind of currying.
    //i could display it as one multi-node but internally i could just use a kind of curry-node. These nodes could maybenot passs through the observable
    //stuff and be handled a little differnently
    0 // return an integer exit code
