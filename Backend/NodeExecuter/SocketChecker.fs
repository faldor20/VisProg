module VisProg.SocketChecker

open VisProg.Shared.Node.Node
open System
open System.Collections.Generic
open FSharp.Control.Reactive
open FSharp.Quotations
open VisProg.BackEnd.Node
open VisProg.Shared.Node
open VisProg.Shared.Node.Mid
open VisProg.Shared.Node.Funcs
(* 
let getNonGenericTypes (types:SocketType list)=
    types|>List.choose(fun x-> match x with |Standard y-> Some y|Generic(y,_)->Some y )

let getAllSocketTypes (nodes:NodeTemplate list)=
    let socketTypes=
        nodes|>List.map(fun x->
            match x with
            | :? NonGenericNodeTemplate  as y->y.InputType,y.OutputType
            | :? MiddleNodeTemplate as y-> y.InputTypes,y.OutputType
        )
    areCompatibleTypes  *)