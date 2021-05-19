module App.MainPage
open System
open Elmish
open Elmish.React
open VisProg.UI.Editor
open VisProg.UI
open Feliz

type Model=
    {VisProgEditor:Editor.EditorModel}
type Msg =
| Increment
| Decrement

let init() : Model = 
    let normal={VisProgEditor=Editor.init()}
    {normal with VisProgEditor= {normal.VisProgEditor with VisNodes=(Editor.testNode())::normal.VisProgEditor.VisNodes}}
let update (msg:Msg) (model:Model) =
    match msg with
    |Increment->model
// VIEW (rendered with React)

let view (model:Model) dispatch =
    Html.div[
        prop.text "hi"
        prop.children[
            Html.h1 "Main"
            Editor.render(model.VisProgEditor)
        ]
    ]


let startup()=
    Program.mkSimple init update view
    |> Program.withReactSynchronous "feliz-app"
    |> Program.withConsoleTrace
    |> Program.run
