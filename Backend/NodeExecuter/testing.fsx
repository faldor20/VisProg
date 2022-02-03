#r "./bin/Debug/net6.0/Executer.dll"
#r "./bin/Debug/net6.0/Node.dll"
#r "./bin/Debug/net6.0/TypeShape.dll"
open VisProg.BackEnd.Node
open VisProg.Executer
open VisProg.Shared.Node
open VisProg.Shared.Node.Mid
let a= typeof<List<_>>
printfn "gen type? %b"a.IsGenericType
printfn "gen type def? %b"a.IsGenericTypeDefinition
let b= typedefof<List<_>>
printfn "gen type? %b" b.IsGenericType
printfn "gen type def? %b" b.IsGenericTypeDefinition

let prnt (a:'a) = 
    printfn "%A"a
    1
let multi (a) b = 
    a*b
let div (a) b = 
    a/b
let div2 (a)  = 
    a/2

let prntNode= VisProg.BackEnd.Node.createMiddleNodeTemplate prnt boxFn "prints anything" "outputs an int 1."
let multiNode= VisProg.BackEnd.Node.createMiddleNodeTemplate multi boxFn2 "multiplies" "outputs a*b"
let divNode= VisProg.BackEnd.Node.createMiddleNodeTemplate div boxFn2 "divides" "output a/b"
let div2Node= VisProg.BackEnd.Node.createMiddleNodeTemplate div2 boxFn "diveds a /2" "a/2"
let printdata (c:MiddleNodeTemplate)=
    printfn "=====start===="
    printfn "NodeInp:%A"c.InputsCount
    printfn "NodeInptype:%A"c.InputTypes
    printfn "NodeInfo:%A"c.NodeInfo
    printfn "NodeOutput:%A"c.OutputType
    printfn "=====end===="
[prntNode;multiNode;divNode;div2Node]|>List.iter printdata

let prnt = MiddleNode(prntNode)