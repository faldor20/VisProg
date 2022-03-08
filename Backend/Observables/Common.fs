module Observables.Common
open FSharp.Control.Reactive
open FSharp.Quotations

let getparams (fn2: 'U-> 'T)=

    let dcl=fn2.GetType().DeclaringType
    printfn "dcl %A Name: %A"dcl (fn2.GetType().FullName)
    let meth=dcl.GetMethod(nameof fn2)
    printfn "meth %A" meth
    let parm=meth.GetParameters()
    ()
let rec funInfo = function
| Patterns.Call(None, methodInfo, _) -> methodInfo
| Patterns.Lambda(_, expr) -> funInfo expr
| _ -> failwith "Unexpected input"
type MI=
    static member get([<ReflectedDefinition>] fn: Expr<'T>) =
        
        let methodInfo=(funInfo fn)
        let goodMethodInfo=methodInfo.DeclaringType.GetMethod(methodInfo.Name)
        goodMethodInfo

//==========================================================
//                  TestFuncs
//==========================================================


let fn  (p1:'a) (p2:'a) (p3:'b)(p4:list<'b>) (p5: int list)=
    p1
let id2 (ba:'a) (bb:'b)=
    ba
let add a b=
    a+b
let add1 a b=
    a+b
let makeList a=
    [a]
let Listcopy (a: 'a list):'b list =
    let boxed=a|>box
    let unboxed:'b list=boxed|>unbox
    unboxed@unboxed
let get10()=
    10
let makeN numbers=
    numbers|>Observable.ofSeq
let printr a=
    printfn "result %A" a

let tupleize a (b,c)=
    a b c
