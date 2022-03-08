module VisProg.Utils
open FSharp.Quotations
open System.Reflection
open System
open TypeShape.Core
open System.Reactive
open System.Reactive.Linq
open System.Reactive.Concurrency
open System.Reactive.Disposables
open FSharp.Control.Reactive
let zip2 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> )  : IObservable<'Source1 * 'Source2 > =
    Observable.Zip( first, second, fun a b -> a,b)
let zip3 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> ) ( third:IObservable<'Source3> ) : IObservable<'Source1 * 'Source2 * 'Source3> =
    Observable.Zip( first, second, third, fun a b c -> a,b,c)
let zip4 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> ) ( third:IObservable<'Source3> ) ( fourth:IObservable<'Source4> )  : IObservable<'Source1 * 'Source2 * 'Source3*'Source4> =
    Observable.Zip( first, second, third,fourth, fun a b c d -> a,b,c ,d)
let zip5 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> ) ( third:IObservable<'Source3> ) ( fourth:IObservable<'Source4> ) ( fifth:IObservable<'Source5> )  : IObservable<'Source1 * 'Source2 * 'Source3*'Source4*'Source5> =
    Observable.Zip( first, second, third,fourth,fifth, fun a b c d e -> a,b,c ,d,e)
let zip6 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> ) ( third:IObservable<'Source3> ) ( fourth:IObservable<'Source4> ) ( fifth:IObservable<'Source5> ) ( sixth:IObservable<'Source6> )  : IObservable<'Source1 * 'Source2 * 'Source3*'Source4*'Source5*'Source6> =
    Observable.Zip( first, second, third,fourth,fifth,sixth, fun a b c d e f-> a,b,c ,d,e,f)
let zip7 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> ) ( third:IObservable<'Source3> ) ( fourth:IObservable<'Source4> ) ( fifth:IObservable<'Source5> ) ( sixth:IObservable<'Source6> ) ( seventh:IObservable<'Source7> ) : IObservable<'Source1 * 'Source2 * 'Source3*'Source4*'Source5*'Source6*'Source7> =
    Observable.Zip( first, second, third,fourth,fifth,sixth,seventh, fun a b c d e f g-> a,b,c ,d,e,f,g)
let zip8 ( first:IObservable<'Source1> ) ( second:IObservable<'Source2> ) ( third:IObservable<'Source3> ) ( fourth:IObservable<'Source4> ) ( fifth:IObservable<'Source5> ) ( sixth:IObservable<'Source6> )( seventh:IObservable<'Source7> )( eighth:IObservable<'Source8> )  : IObservable<'Source1 * 'Source2 * 'Source3*'Source4*'Source5*'Source6*'Source7*'Source8> =
    Observable.Zip( first, second, third,fourth,fifth,sixth,seventh,eighth, fun a b c d e f g h-> a,b,c,d,e,f,g,h)


let tup2T=typedefof<int*int>
let tup3T=typedefof<int*int*int>
let tup4T=typedefof<int*int*int*int>
let tup5T=typedefof<int*int*int*int*int>
let tup6T=typedefof<int*int*int*int*int*int>
let tup7T=typedefof<int*int*int*int*int*int*int>
let tup8T=typedefof<int*int*int*int*int*int*int*int>
let makeTupleType (paramArray:Type array)=
    let tupleBaseType=
        match paramArray.Length with
        |2->tup2T
        |3->tup3T
        |4->tup4T
        |5->tup5T
        |6->tup6T
        |7->tup7T
        |8->tup8T
        |_->failwithf "unsupported number of function args"
    tupleBaseType.MakeGenericType(paramArray)
let getZipper( paramArray:Type array)=
    let method=
        match paramArray.Length with
        |2->zip2.GetType().DeclaringType.GetMethod(nameof zip2)
        |3->zip3.GetType().DeclaringType.GetMethod(nameof zip3)
        |4->zip4.GetType().DeclaringType.GetMethod(nameof zip4)
        |5->zip5.GetType().DeclaringType.GetMethod(nameof zip5)
        |6->zip6.GetType().DeclaringType.GetMethod(nameof zip6)
        |7->zip7.GetType().DeclaringType.GetMethod(nameof zip7)
        |8->zip8.GetType().DeclaringType.GetMethod(nameof zip8)
    method.MakeGenericMethod(paramArray)

module QuotationHelpers=
    ///Converts a methodInfo into a lambda as opposed to a call. 
    ///Used when you want to say pass a funtion into List.map
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
    ///Assembles a sequence of functions together.
    /// Assumes each function's output matches the next function's input
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
    ///Creates a lambda that takes a tuple and then desctructures it
    ///and feeds it the provided method .
    let tupleLambda (methodInfo:MethodInfo) tupleType  =
        let args=methodInfo.GetParameters()
        
        let xV=Var("x",tupleType)
        let x=Expr.Var(xV)
        let exprArgs=args|>Array.mapi(fun i _-> Expr.TupleGet(x,i))|>Array.toList

        Expr.Lambda(xV,Expr.Call(methodInfo,exprArgs))
    