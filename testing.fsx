let a= typeof<List<_>>
printfn "gen type? %b"a.IsGenericType
printfn "gen type def? %b"a.IsGenericTypeDefinition
let b= typedefof<List<_>>
printfn "gen type? %b" b.IsGenericType
printfn "gen type def? %b" b.IsGenericTypeDefinition