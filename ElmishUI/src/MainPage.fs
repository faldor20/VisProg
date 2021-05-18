module App.MainPage
open System
open Elmish
open Elmish.React
open VisProg
open Feliz

type Model=
    {VisProgEditor:Editor.EditorModel}
type Msg =
| Increment
| Decrement

let init() : Model = {VisProgEditor=Editor.init()}
let update (msg:Msg) (model:Model) =
    match msg with
    |Increment->model
// VIEW (rendered with React)

let view (model:Model) dispatch =
    Html.div[

    ]


let startup()=
    Program.mkSimple init update view
    |> Program.withReactSynchronous "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run
