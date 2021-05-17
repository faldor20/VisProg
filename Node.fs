
module VisProg.Node
open System
open FSharp.Reflection
open TypeShape.Core
let hasImplicitConversion  (source:Type) (destination:Type) =

    let sourceCode = Type.GetTypeCode( source );
    let destinationCode = Type.GetTypeCode( destination );
    match( sourceCode ) with
    | TypeCode.SByte->
            match( destinationCode )with
            | TypeCode.Int16 | TypeCode.Int32 | TypeCode.Int64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Byte->
            match( destinationCode )with
            | TypeCode.Int16 | TypeCode.UInt16 | TypeCode.Int32 | TypeCode.UInt32 | TypeCode.Int64 | TypeCode.UInt64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Int16->
            match( destinationCode )with
            | TypeCode.Int32 | TypeCode.Int64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->      
                true
            |_->
                false
    | TypeCode.UInt16->
            match( destinationCode )with
            | TypeCode.Int32 | TypeCode.UInt32 | TypeCode.Int64 | TypeCode.UInt64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Int32->
            match( destinationCode )with
            | TypeCode.Int64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal -> 
                true
            |_->
                false
    | TypeCode.UInt32->
            match( destinationCode )with
            | TypeCode.UInt32| TypeCode.UInt64| TypeCode.Single| TypeCode.Double| TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Int64->false
    | TypeCode.UInt64->
            match( destinationCode )with
            | TypeCode.Single| TypeCode.Double| TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Char->
            match( destinationCode ) with
            | TypeCode.UInt16 | TypeCode.Int32 | TypeCode.UInt32 | TypeCode.Int64 | TypeCode.UInt64 | TypeCode.Single | TypeCode.Double | TypeCode.Decimal->
                true
            |_->
                false
    | TypeCode.Single->
            ( destinationCode.Equals( TypeCode.Double) )
    |_-> false

type InvokeResult = 
    | Success of obj
    | ObjectWasNotAFunction of Type

let trydynamicFunction (fn:obj) (args:obj seq) =
    let rec dynamicFunctionInternal (next:obj) (args:obj list) : InvokeResult =
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
            else ObjectWasNotAFunction fType
        | true ->
            Success(next)
    dynamicFunctionInternal fn (args |> List.ofSeq )
let dynamicFunction fn a=
    match trydynamicFunction fn a with
    |Success x->x
    |ObjectWasNotAFunction x->failwith "tred to invoke an obect that was not a function %A"x
let a= typeof<FSharpFunc<int,int>>
let genType=a.GetGenericTypeDefinition()
(* type Node ={
    ID:Guid
    Fn:obj
    InputType:Type list
    OutputType:Type
    mutable Next: Node list 
    Last:Node option array 
} *)

//using inheritance we seem to be able to get a combination of genric and type safe operations.
type Node (Fn,inputTypes,outputTypes,Next,last)=
    member x.ID:Guid=Guid.NewGuid()
    member x.Fn:obj=Fn
    member x.InputType:Type list =inputTypes
    member x.OutputType:Type=outputTypes
    member val Next: Node list =Next with get,set
    member x.Last:Node option array=last 

type Node<'T> (Fn,inputTypes,outputTypes)=
    inherit Node(Fn,inputTypes,outputTypes,[],(Array.init inputTypes.Length (fun x->None)))
    member x.Fn:'T=Fn
type FirstNode<'T> (Fn:IObservable<'T>)=
    inherit Node<IObservable<'T>>(Fn,[],typeof<'T>)
    member x.Fn:IObservable<'T>=Fn
type Node<'U,'V> (Fn:'U->'V,inputTypes,outputTypes)=
    inherit Node<'U->'V>(Fn,inputTypes,outputTypes)
    member x.Fn:'U->'V=Fn

type Node<'T,'U,'V> (Fn:'T->'U->'V,inputTypes,outputTypes)=
    inherit Node<'T,'U->'V>(Fn,inputTypes,outputTypes)
    member x.Fn:'T->'U->'V=Fn
   
        
(* 
let ac = Node<int,int> ((fun x->x+1),[],typeof<int>)
let ag = Node<'a,'b> ((fun x->x+1.0),[],typeof<int>)
let a:Node list=[ac;ag]
let args= (1,2)
let res:list<obj>=
    a|>List.map(fun x->
        match x with
        | :? Node<_,_,_> as triFunc->
             args||>triFunc.Fn
        |_->failwith "dang"
    ) *)
let createNode (a:'T->'U)  :Node<'T,'U>=
    match shapeof<'T->'U> with
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
        Node<'T,'U>(a,inputs,output.Type)
    |a->failwith  (sprintf"tried to create node with object :%A type of %A" a (a.GetType()))
let createFirstNode<'T> (a:IObservable<'T>)= 
    FirstNode(a)

let start a ()=a

//NOTE: it is very important that we not use the copy then update aproach of records. this will cause us to loose our refernce
let join inputNum  (dest:Node) (source:Node) =
    if source.OutputType.IsAssignableTo(dest.InputType.[inputNum]) || hasImplicitConversion source.OutputType dest.InputType.[inputNum] then
        dest.Last.[inputNum]<-Some source
        source.Next<-dest::source.Next
    else failwith (sprintf"OH no, the output type of %A does not match the input type of %A " source dest)

let disconnect inputNum  (dest:Node) (source:Node) =
    dest.Last.[inputNum]<-None
    source.Next<- source.Next|>List.except [dest]