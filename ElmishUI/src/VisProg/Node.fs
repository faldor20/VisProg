namespace VisProg.UI


module VisNode =
    open System
    open VisProg.Shared.Node
    open Feliz

    type VisNode =
        { Node: Node
          Name: string
          Description: String
          Postition: (int * int)
          inputNames: string list
          outputNames: string list }

    let getIDString visNode =
        let id = visNode.Node.ID

        Convert
            .ToBase64String(id.ToByteArray())
            .TrimEnd '='

    let renderPoint name (id: string) socketType classExtra =
        //TODO:Use the type to set color
        //TODO
        Html.li [ prop.className ("socket " + classExtra)
                  prop.text (sprintf "%s |Type: %A " name socketType)
                  prop.id id ]

    let getSocketClass thisType (selectedType: Type option) isInput =
        let io = if isInput then "input" else "output"

        let state =
            selectedType
            |> Option.map
                (fun x ->
                    if (x = thisType) then
                        "matching"
                    else
                        "disabled")
            |> Option.defaultValue ""

        String.Concat [ io; state ]


    let renderSockets names sockets selectedType (idPrefix: string) isInput =
        Html.ul [ prop.className "socketList"
                  prop.children (
                      (names, sockets)
                      ||> List.mapi2
                              (fun i name socketType ->
                                  let render =
                                      renderPoint name (idPrefix + i.ToString()) socketType

                                  let className =
                                      getSocketClass socketType selectedType isInput

                                  render className)
                  ) ]

    let renderInputs visNode selectedType =
        let id = (getIDString visNode)
        renderSockets visNode.inputNames visNode.Node.InputType selectedType id true

    let renderOutputs visNode selectedType =
        let id = (getIDString visNode)
        renderSockets visNode.outputNames [ visNode.Node.OutputType ] selectedType id false

    let renderNode (visNode: VisNode) selectedType =
        Html.div [ prop.className "node"
                   prop.style [ style.position.absolute
                                style.left (visNode.Postition |> fst)
                                style.bottom (visNode.Postition |> snd) ]
                   prop.children [ Html.h1 visNode.Name
                                   renderInputs visNode selectedType
                                   Html.p visNode.Description
                                   renderOutputs visNode selectedType ] ]







module Connection =

    open Feliz
    open System

    let renderPoint thisType classExtra =
        //TODO:Use the type to set color
        //TODO
        Html.div [ prop.className ("circle" + classExtra) ]

    let connection (selectedType: Type option) thisType isInput =
        match selectedType with
        | Some sel ->
            if sel = thisType && isInput then
                renderPoint thisType "matching"
            else
                renderPoint thisType "disabled"
        | None -> renderPoint thisType ""

    let renderConnection fromnode toNode inputnum =

        Html.div [ Svg.line [ svg.points [ (10, 10); (20, 20) ] ] ]


module NodeTest =
    open Feliz
    open VisNode
    open System

    type model =
        { SelectedType: Type option
          Nodes: VisNode list }

    let c = 1
