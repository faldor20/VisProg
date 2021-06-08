open System
///Allows us to see the state of generic types
type Unknown = obj
let a= typeof<int*float list >
let b= typeof<int*Unknown list>
a.IsGenericType|>printfn "generic? %A"
typeof<obj>.IsGenericType|>printfn "obj generic? %A"
a.IsGenericTypeDefinition  |>printfn "genericdef? %A"
let gen=b.GetGenericTypeDefinition()
gen|>printfn "type %A"
let getgenArgs=b.GetGenericArguments()

getgenArgs|>printfn "get gen arggs %A"
let genArgs=b.GenericTypeArguments
genArgs|>printfn "genargs %A"

type Parent()=
    member x.a=10
type Child()=
    inherit Parent()
    member x.b = 20
let p=typeof<Parent>
let c= typeof<Child>
p.IsAssignableFrom c