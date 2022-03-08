module Experiments
open FSharp.Control.Reactive
open System
open FSharp.Quotations

module Quotation=
    let inline maker< ^A> (a:IObservable< ^A >) = <@@ a @@>
    let inline mapper fn o= <@@Observable.map %%fn %%o@@>
    let inline zipper o1 o2= <@@Observable.zip %%o1 %%o2 @@>
    let createZip =printfn"%A" <@@Observable.zip@@>
    let add1 a=
        a+1
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

(*     let rec createExpression (last:Expr) (node:MinNode)=
        let Obs= Observable.map.GetType().DeclaringType
        let Omap=Obs.GetMethod(Observable.map.GetType().Name)
        //we would tuple the sockets together becuase tuples are used to combine when Observable.zip
        let map=Omap.MakeGenericMethod([|node.inType 0,node.intype 1;node.OutType|])
        let nodeFn=node.methodInfo.MakeGenericMethod([|node.inType[0];node.inType [1];node.outType|]) 
        Expr.Call(map,[last;Expr.Call(nodeFn,[])]) *)