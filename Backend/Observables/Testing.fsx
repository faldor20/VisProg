#r "nuget:Fsharp.Control.Reactive"
#r "nuget:FSharp.Quotations.Evaluator"
#load @"c:\Users\Eli\Documents\programming\FSharp\VisProg\Backend\Observables\Common.fs"
#load @"c:\Users\Eli\Documents\programming\FSharp\VisProg\Backend\Observables\Experiments.fs"

open FSharp.Control.Reactive
open System
open FSharp.Quotations
open Observables.Common
open FSharp.Quotations.Evaluator
open System.Reflection
open System.Reactive
open System.Reactive.Linq
open System.Reactive.Concurrency
open System.Reactive.Disposables

module Quotation=
    let inline maker< ^A> (a:IObservable< ^A >) = <@@ a @@>
    let inline mapper fn o= <@@Observable.map %%fn %%o@@>
    let inline zipper o1 o2= <@@Observable.zip %%o1 %%o2 @@>
    let createZip =printfn"%A" <@@Observable.zip@@>
    let add1 a=
        a+1
    let a= Observable.zip
module QuotationExecuter=
    open Quotation

    type  MinNode={
        nextnode:MinNode
        methodInfo:Reflection.MethodInfo
        inType:Type array
        outType:Type
    }
    //It may be possible to use Expr.call and makegeneric method info to construct correctly typed methods fall the required calls.
    //could be as easy as calling inputType on each of the sockets and passssing them inot the makegerneicmethodInfo method

    let rec createExpression (last:Expr) (node:MinNode)=
        let Obs= Observable.map.GetType().DeclaringType
        let Omap=Obs.GetMethod(Observable.map.GetType().Name)
        //we would tuple the sockets together becuase tuples are used to combine when Observable.zip
        let types=[|(typedefof<(obj *obj) >).MakeGenericType(node.inType);node.outType|]
        let map=Omap.MakeGenericMethod(types)
        let nodeFn=node.methodInfo.MakeGenericMethod([|node.inType[0];node.inType[1];node.outType|]) 
        Expr.Call(map,[last;Expr.Call(nodeFn,[])])
    ///Important note: Any call that takes type unit, does not need any args at all
    let assemble meths=
        let rec caller (methods:MethodInfo list) =
            match methods with
            |[head]-> 
                printfn "adding final %A"head
                Expr.Call(head,[ ])
            |head::tail -> 
                printfn "adding head %A"head
                Expr.Call(head,[caller tail])    
        meths|>List.rev|>caller
    //==========================================================
    //     This Test Shows That it is possible to create Exppressions from generic methods with applied parameters
    //     then evaluate those methods.
    //      This is exactly what we need to generate the observables we want
    //      It could even be used to create non-observbable evaluators             
    //==========================================================
    
    let test()=
        let intT=typeof<int>
        let intLT=typeof<int list>
        let gen10M=MI.get(get10)
        let add1M= MI.get add1
        let listmeth=MI.get makeList
        let prM= MI.get printr
        let lsCM=MI.get Listcopy

        let lsCM=lsCM.MakeGenericMethod([|intT;intT|])
        let lsM=listmeth.MakeGenericMethod([|intT|])
        let prM=prM.MakeGenericMethod([|intLT|])
        
        let assembled=assemble [gen10M;add1M;lsM; lsCM;prM]

        assembled|>QuotationEvaluator.EvaluateUntyped
    
    let foldr a b=
        a+b
    let a=
        <@ [1]|>List.fold foldr 0 @>
    printfn "%A"a
    let makeLambda (methInfo:MethodInfo)=
        //build a lambda for each incoming paramatere then call the function using those variables fomr teh lambda
        let rec inner (args:Type list) (vars:Expr list)=
            match args with
            |head::tail->
                let xV=Var("x",head)
                let x=Expr.Var(xV)
                Expr.Lambda(xV,inner tail (x::vars)) 
            |[]->Expr.Call(methInfo,  vars|>List.rev )    
        let args=methInfo.GetParameters()|>Array.map(fun x->x.ParameterType)|>Array.toList
        inner args []
    //test()
    let testMap()=
        let intT=typeof<int>
        let intLT=typeof<int list>
        let gen10M=MI.get(get10)
        let add1M= MI.get add1
        let listmeth=MI.get makeList
        let prM= MI.get printr
        let lsCM=MI.get Listcopy
        let lsfM=MI.get List.fold
        let foldrM=MI.get foldr

        let lsfM=lsfM.MakeGenericMethod([|intT;intT|])
        let lsCM=lsCM.MakeGenericMethod([|intT;intT|])
        let lsM=listmeth.MakeGenericMethod([|intT|])
        let prM=prM.MakeGenericMethod([|intLT|])
        
        let assembled=assemble [gen10M;add1M;lsM; lsCM]
        
        printfn "made adder"
        let final=Expr.Call(lsfM,[makeLambda foldrM;Expr.Value(0);assembled])
        final|>QuotationEvaluator.EvaluateUntyped 

    printfn "%A "<| testMap()
    let aa=(1,1)
    printfn "%A" (aa.GetType())
    //What should our expression look like?

    let exp= <@ (fun ((x,y):(int*int))->foldr x y)@>
    printfn "Array applyer:\n %A" exp
    let testArray=
        let intT=typeof<int>
        let intLT=typeof<int list>
        let gen10M=MI.get(get10)
        let add1M= MI.get add1
        let listmeth=MI.get makeList
        let prM= MI.get printr
        let lsCM=MI.get Listcopy
        let lsfM=MI.get List.fold
        let foldrM=MI.get foldr

        let lsfM=lsfM.MakeGenericMethod([|intT;intT|])
        let lsCM=lsCM.MakeGenericMethod([|intT;intT|])
        let lsM=listmeth.MakeGenericMethod([|intT|])
        let prM=prM.MakeGenericMethod([|intLT|])
        
        let assembled=assemble [gen10M;add1M;lsM; lsCM]
        
        printfn "made adder"
        let final=Expr.Call(lsfM,[makeLambda foldrM;Expr.Value(0);assembled])
        final|>QuotationEvaluator.EvaluateUntyped 