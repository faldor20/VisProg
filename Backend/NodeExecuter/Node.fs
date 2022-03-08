module VisProg.BackEnd.Node

open System
open FSharp.Reflection
open TypeShape.Core
open TypeShape
open VisProg.Shared.Node
open FSharp.Quotations
open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Evaluator
open FSharp.Linq.RuntimeHelpers
open TypeShape
open System.Collections.Concurrent
open System.Linq.Expressions

let (?) (this : 'Source) (prop : string) : 'Result =
    let p = this.GetType().GetProperty(prop)
    p.GetValue(this, null) :?> 'Result
module EfficientDelegate =


    type VoidType = unit

    type DelegatePair( del:Delegate,  argumentCount:int,  hasReturnValue:bool)=
        member x.Delegate=del
        member x.argumentCount=argumentCount
        member x.HasReturnValue=hasReturnValue

    let CreateDelegate( method:Reflection.MethodInfo)=
        
        let argTypes= 
            [| method.ReturnType |]
            |>Array.append
                (method.GetParameters()
                |>Array.map(fun p->p.ParameterType)   ) 
            
        printfn "type args %A" argTypes

        let newDelType = Expression.GetDelegateType(argTypes);

        let newDel = Delegate.CreateDelegate(newDelType, method);

 

        DelegatePair(newDel, argTypes.Length - 1, method.ReturnType <> typeof<VoidType>)

    

 

    let TooManyArgsMessage = "Invokes for more than 10 args are not yet implemented"

 


 

    let delegateMap = new ConcurrentDictionary<Tuple<string, string>, DelegatePair>();

    let EfficientInvoke( methodInfo:Reflection.MethodInfo,  args:obj[]):obj=
        let key = (methodInfo.Name,methodInfo.DeclaringType.Name)
        let delPair = delegateMap.GetOrAdd(key, CreateDelegate methodInfo);
        if (delPair.HasReturnValue) then
            match (delPair.argumentCount) with
            |0-> delPair?Delegate()
            |1-> delPair?Delegate(args[0])
            |2-> delPair.Delegate.DynamicInvoke(args)          
            |3-> delPair?Delegate(args[0], args[1], args[2])
            |4-> delPair?Delegate(args[0], args[1], args[2], args[3])
            |5-> delPair?Delegate(args[0], args[1], args[2], args[3], args[4])
            |6-> delPair?Delegate(args[0], args[1], args[2], args[3], args[4], args[5])
            |7-> delPair?Delegate(args[0], args[1], args[2], args[3], args[4], args[5], args[6])
            |8-> delPair?Delegate(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7])
            |9-> delPair?Delegate(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8])
            |10-> delPair?Delegate(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9])
            |_-> raise <|NotImplementedException(TooManyArgsMessage)
        else ()

type CompiledExpr<'T> = Environment -> 'T

and Environment private (index: Map<Var, obj ref>) =
    new() = new Environment(Map.empty)

    member __.NewVar(v: Var, value: obj) =
        new Environment(Map.add v (ref value) index)

    member __.GetVar(v: Var) = index.[v].Value
    member __.UpdateVar(v: Var, value: obj) = index.[v] := value


let rec meaning<'T> (expr: Expr<'T>) : CompiledExpr<'T> =
    let EQ (f: CompiledExpr<'a>) = unbox<CompiledExpr<'T>> f
    let cast (e: Expr) = Expr.Cast<_> e

    match expr with
    | Value (:? 'T as t, _) -> fun _ -> t
    | Var var -> fun env -> env.GetVar var :?> 'T
    | Application (func, arg) ->
        let argShape = TypeShape.Create arg.Type

        argShape.Accept
            { new ITypeVisitor<CompiledExpr<'T>> with
                member __.Visit<'Arg>() =
                    let cfunc = meaning<'Arg -> 'T> (cast func)
                    let carg = meaning<'Arg> (cast arg)
                    EQ(fun env -> (cfunc env) (carg env)) }

    | Lambda (var, body) ->
        match shapeof<'T> with
        | Shape.FSharpFunc s ->
            s.Accept
                { new IFSharpFuncVisitor<CompiledExpr<'T>> with
                    member __.Visit<'Dom, 'Cod>() =
                        let cbody = meaning<'Cod> (cast body)

                        let c =
                            EQ(fun env (v: 'Dom) -> let env' = env.NewVar(var, v) in cbody env')

                        c }

        | _ -> failwith "internal error"

    | Let (var, bind, cont) ->
        let vShape = TypeShape.Create var.Type

        vShape.Accept
            { new ITypeVisitor<CompiledExpr<'T>> with
                member __.Visit<'Var>() =
                    let cbind = meaning<'Var> (cast bind)
                    let ccont = meaning<'T> (cast cont)

                    fun env ->
                        let v = cbind env
                        let env' = env.NewVar(var, v)
                        ccont env'
                    |> EQ }

    | IfThenElse (cond, left, right) ->
        let ccond = meaning<bool> (cast cond)
        let cleft = meaning<'T> (cast left)
        let cright = meaning<'T> (cast right)

        fun env ->
            if ccond env then
                cleft env
            else
                cright env

    | Sequential (left, right) when left.Type = typeof<unit> ->
        let cleft = meaning<unit> (cast left)
        let cright = meaning<'T> (cast right)

        fun env ->
            cleft env
            cright env

    | SpecificCall <@ (+) @> (None, _, [ left; right ]) when typeof<'T> = typeof<int> ->
        let cleft = meaning<int> (cast left)
        let cright = meaning<int> (cast right)
        EQ(fun env -> cleft env + cright env)

    | SpecificCall <@ (-) @> (None, _, [ left; right ]) when typeof<'T> = typeof<int> ->
        let cleft = meaning<int> (cast left)
        let cright = meaning<int> (cast right)
        EQ(fun env -> cleft env - cright env)

    | SpecificCall <@ (*) @> (None, _, [ left; right ]) when typeof<'T> = typeof<int> ->
        let cleft = meaning<int> (cast left)
        let cright = meaning<int> (cast right)
        EQ(fun env -> cleft env * cright env)

    | SpecificCall <@ (=) @> (None, _, [ left; right ]) ->
        match TypeShape.Create left.Type with
        | Shape.Equality s ->
            s.Accept
                { new IEqualityVisitor<CompiledExpr<'T>> with
                    member __.Visit<'a when 'a: equality>() =
                        let cleft = meaning<'a> (cast left)
                        let cright = meaning<'a> (cast right)
                        EQ(fun env -> cleft env = cright env) }

        | _ -> failwith "internal error"

    | SpecificCall <@ not @> (None, _, [ pred ]) ->
        let cleft = meaning<bool> (cast pred)
        EQ(not << cleft)

    | LetRecursive ([ (fvar, body) ], cont) ->
        let fshape = TypeShape.Create fvar.Type

        fshape.Accept
            { new ITypeVisitor<CompiledExpr<'T>> with
                member __.Visit<'Func>() =
                    let cbody = meaning<'Func> (cast body)
                    let ccont = meaning<'T> (cast cont)

                    EQ
                        (fun env ->
                            let env' = env.NewVar(fvar, null)
                            env'.UpdateVar(fvar, cbody env')
                            ccont env') }

    | _ -> failwithf "Unsupported expression %A" expr


let compile (e: Expr<'T>) : unit -> 'T =
    let c = meaning e
    fun () -> c (Environment())

let run (e: Expr<'T>) : 'T = compile e ()


let rec funInfo = function
| Patterns.Call(None, methodInfo, _) -> methodInfo
| Patterns.Lambda(_, expr) -> funInfo expr
| _ -> failwith "Unexpected input"


let inline boxHelper (fn: ^a -> ^b) above (x: obj) : obj = x |> (unbox >> fn >> above >> box)

let inline boxFn (fn: ^a -> ^b) (x: obj) =
    let coDomain = fn (unbox x)
    coDomain |> box


let inline boxFn2 fn : (obj -> obj) = boxHelper fn boxFn
let inline boxFn2' fn : obj -> obj = unbox >> fn >> boxFn >> box
let inline boxFn3 fn = boxHelper fn boxFn2
let inline boxFn4 fn = boxHelper fn boxFn3
let inline boxFn5 fn = boxHelper fn boxFn4
let inline uncurry2 fn a b = fn (a, b)
let inline uncurry3 fn a b c = fn (a, b, c)
let inline uncurry4 fn a b c d = fn (a, b, c, d)
let inline uncurry5 fn a b c d e = fn (a, b, c, d, e)
let inline uncurry6 fn a b c d e f = fn (a, b, c, d, e, f)

let inline anyUncurryAndBoxFn (fn: ^T -> ^U) argNum : obj -> obj =
    let a = box fn
    let funcList = [ boxFn (boxFn2) ]

    match argNum with
    | 2 -> unbox (funcList.[0] a)

    | _ ->
        raise (
            new ArgumentException(
                sprintf
                    "tried to do afunctionboxing on a function taht ahd to many input args only functions with up to 5 args ar currently supported yours had %i"
                    argNum
            )
        )
(*
let inline anyboxer< ^T, ^U> (fn: ^T-> ^U) argNum : obj -> obj =

    let a=fn.GetType().GetGenericTypeDefinition()
*)


(* type Funct< ^a, ^b, ^c> =
    | F1 of (^a -> ^b)
    | F2 of (^a -> ^b -> ^c)

let inline anyboxer (fn: Funct< ^a, ^b, ^c >) =
    match fn with
    | F1 a -> boxFn a
    | F2 a -> boxFn2 a *)

let getFuncTypes (fn: ^a -> ^b) =
    let input = typedefof< ^a>
    let output = typedefof< ^b>
    (input, output)

(* let inline createBoxedNodeTemplate (fn: ^T -> ^U) boxer (description: string) (outputName: string) =
    match shapeof< ^T ->  ^U> with
    | Shape.FSharpFunc x ->
        let rec getInputs (fn: IShapeFSharpFunc) inputs =
            let newInputs = fn.Domain.Type :: inputs

            match fn.CoDomain with
            | Shape.FSharpFunc x -> getInputs x newInputs
            | output -> (newInputs, output)

        let inputs, output = getInputs x []
        //let boxedFunc = anyboxer fn (inputs.Length)
        let boxedFunc = boxer fn
        printfn "type:%A" x
        printfn "inputs %A" inputs
        let (funcName, funcInputs) = DocumentGetter.GetInfo(fn)

        let nodeInfo =
            { Name = funcName
              InputNames = (Array.toList funcInputs)
              Description = (description)
              OutputNames = [ (outputName) ] }
        //TODO it would be good to impliment a multibackward and multi. but for now i'm only allowing single outputs. You can decompose a tuple if thats what you want to do
        //TODO: i must be careful when mkaing an instance of a node to make sure it dosn't end up with a shared refence
        NonGenericNodeTemplate(boxedFunc, inputs, output.Type, nodeInfo)
    | _ -> failwith "this should be impossible, yoou should never be able to pass anything but a function in as fn"
     *)

let getSocketTypes ( methodInfo:Reflection.MethodInfo ) isFirst =
    //TODO: I Call these gneric type iterators a number of times, it may be possible to do it less times overall
    let rec extractGenerics  generics (typ:Type)=
        printfn "getting the generics type %A" typ
        let args= typ.GenericTypeArguments
        if  not (typ.IsGenericType||typ.IsGenericParameter) then 
            printfn "received a non-generic type%A" generics
            generics
        else if args.Length=0 then typ::generics
        else args|>Array.toList|>List.collect (extractGenerics generics)
    //beucase we don't know if types with params have generics we have to iterate intoa  type first to find out if it contains generics and then 
    //iterate again to consttruct the generic type, or just use a non-generic type
    let hasGenerics (typ:Type)=
        let rec intern (typ:Type)=
            let args= typ.GenericTypeArguments
            if not (typ.IsGenericType||typ.IsGenericParameter) then 
                [|false|]
            else if args.Length=0 then[|true|]
            else args|>Array.collect(intern )
        let generic=(intern typ )|>Array.contains true
        if not generic then printfn "type %A has no generics" typ
        generic

    let convertSocket  (mapping:Map<string,int>) (typ:Type)=
        let rec convertGeneric (mapping:Map<string,int>) (typ:Type)=
            if not (typ.IsGenericType||typ.IsGenericParameter) then  
                SocketType.Standard typ
            else if typ.GenericTypeArguments.Length>0 then
                //TODO! Not tail recursive. Shouldn't need to be but worth noting.
                SocketType.Generic (GenericType.ComplexType (typ.GetGenericTypeDefinition(),typ.GenericTypeArguments|>Array.map (convertGeneric mapping)))
            else 
                SocketType.Generic(GenericType.SingleType( mapping|>Map.find typ.Name))
        //If the type has no generics at all we can just return it as a standard type
        if not (typ.IsGenericParameter||typ.IsGenericType)||not (typ|>hasGenerics) then  
                SocketType.Standard typ
        else convertGeneric mapping typ
    let param=methodInfo.GetParameters()
    printfn "Got param info for function"
    let types=param|>Array.map(fun x ->x.ParameterType)
    printfn "Got param types  for function"

    let genericsList= 
        types
        |>Array.append [|methodInfo.ReturnType|] //Don't forget to add in the return type just incase it is some other generic
        |>Array.filter(fun x->x.IsGenericType||x.IsGenericParameter)  
        |>Array.collect (extractGenerics []>>List.toArray)
    //I'm really not a fan f using the type name sting here, but we ``Type`` does not impliment comparison,
    //so we have to use the name as a proxy
    let typeMapping= 
        genericsList
        |>Array.indexed
        |>Array.map(fun (i,x)->(x.Name,i))
        |>Array.distinctBy(fun (x,i)-> x)
        //|>Array.groupBy(fun (x,i)->x)
        //|>Array.map (fun (i,group)->i,group|>Array.map(fun (x,i)-> i))
        |>Map.ofArray
    printfn"Made generics list and type mappings %A" typeMapping
    //get a list of all unique generics
    //generate an array whetehr the index is the param number and 
    //the content is the gnerics index
    let socketTypes=
        types|>Array.map (convertSocket typeMapping)
    let outputType=
            let returnType=methodInfo.ReturnType
            if isFirst then
            //TODO: no longer necissary beucase we already know the first func returns iobservable
                if returnType.GetGenericTypeDefinition() = typedefof<IObservable<_>> then
                    let ret=methodInfo.ReturnType.GetGenericArguments()[0]
                    printfn "got starting node that returns %A" ret
                    ret|>convertSocket typeMapping
                    
                else raise<| ArgumentException $"tried to create a a starting node without returning IOBservable. returning: {returnType}"
            else methodInfo.ReturnType|>convertSocket typeMapping

    
    socketTypes,outputType
let getNodeInfo (methodInfo:Reflection.MethodInfo) description outName=
    let funcName,funcInputs=methodInfo.Name,methodInfo.GetParameters()|>Array.map(fun x->x.Name)
    {   
        Name = funcName
        InputNames = (Array.toList funcInputs)
        Description = (description)
        OutputNames = [ (outName) ] 
    }   

let inline applyer< ^T, ^U > (fn: ^T -> ^U) (args: obj array)=
    match args.Length with
    |1-> ((fn|>unbox) (args[0]|>unbox)) |>box
    |2-> ((fn|>unbox) (args[0]|>unbox) (args[1]|>unbox))|>box
    |3-> ((fn|>unbox) (args[0]|>unbox) (args[1]|>unbox) (args[2]|>unbox))|>box
    |4-> ((fn|>unbox) (args[0]|>unbox) (args[1]|>unbox) (args[2]|>unbox) (args[4]|>unbox))|>box
    
type DocumentGetter =
    static member GetInfo([<ReflectedDefinition>] x: Expr<_ -> _>) =
        match x with
        | DerivedPatterns.Lambdas (_, Patterns.Call (_, methodInfo, _)) ->
            
            (methodInfo)
        | ValueWithName (rest,t,name)->
            printfn "valuewithName %A, type: %A" rest (rest.GetType())
            raise (ArgumentException("Not given a function"))
        |e->
            printfn "getting info un usnuported type of %A  Type: %A" e e.Type
            raise (ArgumentException("Not Given a function"))
    static member CreateMiddleNodeTemplate([<ReflectedDefinition>]fn: Expr<'T>, description: string, outputName: string, ?isFirst:bool) =
        printfn "Getting method Info for func %A" fn
        //for some reason this version of the method info doesn't include proper generic types
        let badMethodInfo=(funInfo fn)
        let methodInfo=badMethodInfo.DeclaringType.GetMethod(badMethodInfo.Name)

        let socketTypes,outputType=getSocketTypes methodInfo false
        let nodeInfo= getNodeInfo (methodInfo:Reflection.MethodInfo) description outputName
        let paramTypes=methodInfo.GetParameters()|>Array.map(fun x->x.ParameterType)
        //let fn2= fn|>QuotationEvaluator.Evaluate|>boxer
        (* let fn= (fun x->
            EfficientDelegate.EfficientInvoke(methodInfo,x)) *)
        //let boxedFunc = boxer fn
        
        //TODO it would be good to impliment a multibackward and multi. but for now i'm only allowing single outputs. You can decompose a tuple if thats what you want to do
        //TODO: i must be careful when mkaing an instance of a node to make sure it dosn't end up with a shared refence
        MiddleNodeTemplate( socketTypes|>Array.toList,outputType,(fun x->failwithf "NOOOOOOO!!!!";x),methodInfo, nodeInfo)
    static member CreateFirstNodeTemplate([<ReflectedDefinition>]fn: Expr<'T->IObservable<'U>>,  description: string, outputName: string) =
        printfn "Getting method Info for func %A" fn
        let badMethodInfo=(funInfo fn)
        let methodInfo=badMethodInfo.DeclaringType.GetMethod(badMethodInfo.Name)

        let socketTypes,outputType=getSocketTypes methodInfo true
        let nodeInfo= getNodeInfo (methodInfo:Reflection.MethodInfo) description outputName
        //let fn2= fn|>QuotationEvaluator.Evaluate|>boxer
    (*     let fn= 
            (fun x->)//>> unbox >> (Observable.map box) >> box *)
        
        MiddleNodeTemplate( socketTypes|>Array.toList,outputType,(fun x->failwithf "NOOOOOOO!!!!";x),methodInfo, nodeInfo)
        //| _ -> failwith "this should be impossible, yoou should never be able to pass anything but a function in as fn"

(* let inline objectifyTuple (tuple: ^U) =
    let boxed = (box tuple)

    match (boxed) with
    | :? (^a * ^b) as t ->
        let a, b = t
        box (box a, box b)
    | :? (^a * ^b * ^c) as t ->
        let a, b, c = t
        box (box a, box b, box c)
    | :? (^a * ^b * ^c * ^d) as t ->
        let a, b, c, d = t
        box (box a, box b, box c, box d)
    | _ -> failwith "non implmented tuple length"
let inline deobjectifyTuple (tuple: obj) =
    let boxed = (box tuple)

    match (boxed) with
    | :? (^a * ^b) as t ->
        let a, b = t
        box (box a, box b)
    | :? (^a * ^b * ^c) as t ->
        let a, b, c = t
        box (box a, box b, box c)
    | :? (^a * ^b * ^c * ^d) as t ->
        let a, b, c, d = t
        box (box a, box b, box c, box d)
    | _ -> failwith "non implmented tuple length"
let inline convertTupleFunc (tuple: ^U) =
    match shapeof< ^U> with
    | Shape.Tuple s ->
        let types =
            s.Elements |> Array.map (fun x -> x.Member.Type)

        let out = objectifyTuple tuple
        (out, types)
    | _ -> failwith "Given  '%A' that was not a tuple" tuple *)
//let tuple=

let inline getTupleTypes (tupleType: Type) =
    match TypeShape.Create tupleType with
    | Shape.Tuple s ->
        let types =
            s.Elements |> Array.map (fun x -> x.Member.Type)

        (types)
    | _ -> failwith "not a tuple"

///``fn`` is a tuple ->IObservable< ^T >
let inline createFirstNodeTemplate< ^T, ^U> boxer (fn: ^U -> IObservable< ^T >)  name description outputName =
    let nodeInfo =
        { Name = name
          Description = description
          OutputNames = [ outputName ]
          InputNames = [ "Start" ] }

    let inputs =[typeof< ^U>] 

    let output = typeof< ^T>
    let boxedfn =(fun x-> fn(unbox x)|>Observable.map(fun x->box x)) 
    //TODO it would be good to impliment a multibackward and multi. but for now i'm only allowing single outputs. You can decompose a tuple if thats what you want to do
    //TODO: i must be careful when mkaing an instance of a node to make sure it dosn't end up with a shared refence
    FirstNodeTemplate(boxedfn >> unbox >> (Observable.map box) >> box, inputs, output, nodeInfo)

let rec boxedCurry (fn: obj -> obj) (args: obj list) =
    match args with
    | [ arg ] -> fn arg
    | arg :: rest -> boxedCurry ((fn arg) :?> obj -> obj) rest
    | _ -> raise (new ArgumentException "Received an empty arg array. Cannot run function with no arguments")
