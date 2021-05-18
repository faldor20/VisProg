module VisProg.Editor
open System
open VisProg.VisNode
open Feliz
type EditorModel=
    {
    VisNodes:VisNode list
    SelectedType:Type option
    }

let init()=
    {VisNodes=[];SelectedType=None}

let testNode()=
    let plus20 = (fun x->x+20)
    let plus20Node=Node.createNode plus20
    {Node=plus20Node;Name="add20";inputNames=["x"];Description="dds 20 to 'x'";Postition=(100,200);outputNames=["x+20"]}

let render model=
    Html.div[
        prop.children (model.VisNodes|>List.map(fun x->
        VisNode.renderNode x model.SelectedType
        ))
    
    ]