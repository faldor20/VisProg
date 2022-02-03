#r "nuget: TypeShape, 9.0.0"

open TypeShape
open System

let boxFn fn x = (fn (unbox x)) |> box
let boxFn2 fn  = 
let inline plus a b = a + b
let z = plus 1 2
let x = plus 1.1 2.2
let evalTest z = 
    boxFn2 plus

