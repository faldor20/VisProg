
module Tests.Node.areCompatibleTypes

open VisProg.Shared.Node
open VisProg.BackEnd.Node
open Swensen.Unquote
open Xunit
open System
open VisProg.BackEnd.Node
open VisProg.Shared.Node
[<Fact>]
let ``Unknown Type works for any type``()=
    let a= typeof<int list >
    let b= typeof<UnknownType list>
    let compatible=Funcs.areCompatibleTypes a b
    Assert.True compatible

[<Fact>]
let ``Unknown Type works for tuple inside list``()=
    let a= typeof<int*float list >
    let b= typeof<int*UnknownType list>
    
    let compatible=Funcs.areCompatibleTypes a b
    Assert.True compatible

[<Fact>]
let ``Child Type is compatible with parent``()=
    let parent= typeof<Node >
    let child= typeof<MiddleNode>
    
    let compatible=Funcs.areCompatibleTypes child parent
    Assert.True compatible

[<Fact>]
let ``Unknown Type works for tuple``()=
    let a= typeof<int*float >
    let b= typeof<int*UnknownType>
    
    let compatible=Funcs.areCompatibleTypes a b
    Assert.True compatible
[<Fact>]
let ``Unknown Type doesn't work when rest doesn't match``()=
    let a= typeof<string*float  >
    let b= typeof<int*UnknownType>
    let compatible=Funcs.areCompatibleTypes a b
    Assert.False compatible

//===Testing Nodes===
[<Fact>]
let NestedUnkownTypes()=
    let a= typeof<'a list  >
    let b= typeof<int*UnknownType>
    let compatible=Funcs.areCompatibleTypes a b
    Assert.False compatible