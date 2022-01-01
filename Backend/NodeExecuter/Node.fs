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
open TypeShape

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

type DocumentGetter =
    static member GetInfo([<ReflectedDefinition>] x: Expr<_ -> _>) =
        match x with
        | DerivedPatterns.Lambdas (_, Patterns.Call (_, methodInfo, _)) ->
            let paramaters =
                methodInfo.GetParameters()
                |> Array.map (fun x -> x.Name)

            let name = methodInfo.Name
            (name, paramaters)
        | ValueWithName (rest,t,name)->
            printfn "valuewithName %A, type: %A" rest (rest.GetType())
            (name,[|"params"|])
        |e->
            printfn "getting info un usnuported type of %A  Type: %A" e e.Type
            ("",[|""|])


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

let inline createBoxedNodeTemplate (fn: ^T -> ^U) boxer (description: string) (outputName: string) =
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
let inline createMiddleNodeTemplate< ^T ,^U> (fn: ^T -> ^U) boxer (description: string) (outputName: string) =
    match shapeof< ^T ->  ^U> with
    | Shape.FSharpFunc x ->
        let rec getInputs (fn: IShapeFSharpFunc) inputs =
            let fnType=fn.Domain.Type
            printfn "is genereic %A" fnType.IsGenericType
            printfn "is genericParamater %A" fnType.IsGenericParameter
            printfn "is genericmethodParam %A" fnType.IsGenericMethodParameter
            printfn "is genericTypeDef %A" fnType.IsGenericTypeDefinition
            let newInputs =( SocketType.Standard fnType) :: inputs
            
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
        MiddleNodeTemplate( inputs, SocketType.Standard output.Type,boxedFunc, nodeInfo)
    | _ -> failwith "this should be impossible, yoou should never be able to pass anything but a function in as fn"

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
