module Elmish.Boxes

open System
open Fable.React.Helpers
open Fable.React.Props
open Fable.React.Standard

[<AutoOpen>]
module Domain =


  [<NoComparison>]
  type Point =
    { x: float
      y: float }
    static member (+) (p1, p2) = { x = p1.x + p2.x; y = p1.y + p2.y }
    static member (+) (p, scalar) = { x = p.x + scalar; y = p.y + scalar }
    static member (-) (p1, p2) = { x = p1.x - p2.x; y = p1.y - p2.y }
    static member (-) (p, scalar) = { x = p.x - scalar; y = p.y - scalar }
    static member (*) (p, scalar) = { x = p.x * scalar; y = p.y * scalar }
    static member (*) (p1, p2) = { x = p1.x * p2.x; y = p1.y * p2.y }
    static member (/) (p1, p2) = { x = p1.x / p2.x; y = p1.y / p2.y }
    static member (/) (p, scalar) = { x = p.x / scalar; y = p.y / scalar }

  type BoxId = Guid

  /// Anchor relative to box upper left
  type BoxRelativeAnchor = Point


  type Box =
    { id: BoxId
      upperLeft: Point
      size: float
      color: string }


  module Box =

    let create (center: Point) size color =
      { id = Guid.NewGuid()
        upperLeft = center - (size / 2.)
        size = size
        color = color }

    let moveBy dPos box =
      { box with upperLeft = box.upperLeft + dPos }

    let resizeBy factor (anchor: Point) box =
      let relAnchor = (anchor - box.upperLeft) / box.size
      let resizedBox = { box with size = box.size * factor }
      let relAnchorResized = (anchor - box.upperLeft) / resizedBox.size
      let dPos = (relAnchorResized - relAnchor) * resizedBox.size
      { resizedBox with upperLeft = box.upperLeft + dPos }


  type BoxHierarchy =
    { boxes: Map<BoxId, Box>
      childToParent: Map<BoxId, BoxId>  }


  module BoxHierarchy =

    let childIdsOf parentId hierarchy =
      hierarchy.childToParent
      |> Map.filter (fun _ pid -> pid = parentId)
      |> Map.toList
      |> List.map fst

    let childrenOf parentId hierarchy =
      hierarchy
      |> childIdsOf parentId
      |> List.choose hierarchy.boxes.TryFind

    let rec withDescendantIds hierarchy parentId =
      parentId
      :: ( hierarchy
           |> childIdsOf parentId
           |> List.collect (withDescendantIds hierarchy) )

    let topLevelBoxes hierarchy =
      hierarchy.boxes
      |> Map.filter (fun boxId _ -> not <| hierarchy.childToParent.ContainsKey boxId)
      |> Map.toList
      |> List.map snd

    let addBox box hierarchy =
      { hierarchy with boxes = hierarchy.boxes.Add(box.id, box) }

    let removeWithDescendants boxId hierarchy =
      let toRemove = boxId |> withDescendantIds hierarchy |> Set.ofList
      { hierarchy with
          boxes =
            hierarchy.boxes
            |> Map.filter (fun bid _ -> not <| toRemove.Contains bid)
          childToParent =
            hierarchy.childToParent
            |> Map.filter (fun childId parentId ->
                not <| toRemove.Contains childId
                && not <| toRemove.Contains parentId
            )
      }

    let setTopLevel boxId hierarchy =
      { hierarchy with childToParent = hierarchy.childToParent |> Map.remove boxId }

    let move boxId newPos relativeAnchor hierarchy =
      match hierarchy.boxes.TryFind boxId with
      | None -> hierarchy
      | Some box ->
          let idsToMove = box.id |> withDescendantIds hierarchy |> Set.ofList
          let dPos = newPos - box.upperLeft - relativeAnchor
          let updatedBoxes =
            hierarchy.boxes
            |> Map.map (fun _ box ->
                if idsToMove.Contains box.id then
                  box |> Box.moveBy dPos
                else box
            )
          { hierarchy  with boxes = updatedBoxes }

    let setRelationship childId parentId hierarchy =
      { hierarchy with childToParent = hierarchy.childToParent.Add(childId, parentId) }

    let resize boxId factor absoluteAnchor hierarchy =
      let idsToResize = boxId |> withDescendantIds hierarchy |> Set.ofList
      { hierarchy with
          boxes =
            hierarchy.boxes
            |> Map.map (fun _ box ->
                if idsToResize.Contains box.id then
                  box |> Box.resizeBy factor absoluteAnchor
                else box
            )
      }


[<AutoOpen>]
module MvuModel =

  type Model =
    { hierarchy: BoxHierarchy
      mousePos: Point
      dragging: (BoxId * BoxRelativeAnchor) option
      undo: Model list
      redo: Model list }

  let init () =
    let id1 = Guid.NewGuid()
    let id2 = Guid.NewGuid()
    let id3 = Guid.NewGuid()
    let id4 = Guid.NewGuid()
    { hierarchy =
        { boxes =
            [
              {id = id1; upperLeft = { x = 20.; y = 20. }; size = 200.; color = "red"}
              {id = id2; upperLeft = { x = 100.; y = 100. }; size = 50.; color = "green"}
              {id = id3; upperLeft = { x = 400.; y = 400. }; size = 200.; color = "blue"}
              {id = id4; upperLeft = { x = 500.; y = 500. }; size = 80.; color = "yellow"}
            ]
            |> List.map (fun b -> b.id, b)
            |> Map.ofList
          childToParent = Map.empty.Add(id2, id1).Add(id4, id3) }
      mousePos = { x = 0.; y = 0. }
      dragging = None
      undo = []
      redo = [] }

  let isDragging boxId model =
    model.dragging
    |> Option.map (fun (bid, _) -> bid = boxId)
    |> Option.defaultValue false

  let drop childId parentId model =
    { model with
        dragging = None
        hierarchy = model.hierarchy |> BoxHierarchy.setRelationship childId parentId }

  let rand = Random()

  let addRandomBox center model =
    let size = rand.Next(40, 200) |> float
    let color = String.Format("#{0:X6}", rand.Next(0x1000000))
    let box = Box.create center size color
    { model with hierarchy = model.hierarchy |> BoxHierarchy.addBox box }


[<AutoOpen>]
module MvuUpdate =

  type Msg =
    | PickUp of BoxId * BoxRelativeAnchor
    | DropOnBox of BoxId
    | DropOnEmpty
    | CreateNew of Point
    | Delete of BoxId
    | Grow of BoxId
    | Shrink of BoxId
    | MouseMove of Point
    | Undo
    | Redo


  let update msg model =
    match msg with

    | PickUp (boxId, relativeAnchor) ->
        { model with
            dragging = Some (boxId, relativeAnchor)
            hierarchy = model.hierarchy |> BoxHierarchy.setTopLevel boxId
            undo = model :: model.undo
            redo = [] }

    | DropOnBox parentId ->
        model.dragging
        |> Option.map (fun (childId, _) -> drop childId parentId model)
        |> Option.defaultValue model

    | DropOnEmpty -> { model with dragging = None }

    | CreateNew center ->
        { model with
            undo = model :: model.undo
            redo = [] }
        |> addRandomBox center

    | Delete boxId ->
        { model with
            hierarchy = model.hierarchy |> BoxHierarchy.removeWithDescendants boxId
            undo = model :: model.undo
            redo = [] }

    | Grow boxId ->
        { model with
            hierarchy = model.hierarchy |> BoxHierarchy.resize boxId 1.1 model.mousePos
            undo = model :: model.undo
            redo = [] }

    | Shrink boxId ->
        { model with
            hierarchy = model.hierarchy |> BoxHierarchy.resize boxId (1./1.1) model.mousePos
            undo = model :: model.undo
            redo = [] }

    | MouseMove newPos ->
        { model with
            mousePos = newPos
            hierarchy =
              match model.dragging with
              | None -> model.hierarchy
              | Some (boxId, anchor) -> model.hierarchy |> BoxHierarchy.move boxId newPos anchor
        }

    | Undo ->
        match model.undo with
        | [] -> model
        | head :: tail -> { head with undo = tail; redo = model :: model.redo }

    | Redo ->
        match model.redo with
        | [] -> model
        | head :: tail -> { head with redo = tail; undo = model :: model.undo }


[<AutoOpen>]
module MvuView =

  let rec boxWithChildren model dispatch origin box =
    let dragging = isDragging box.id model
    div [
      Key (string box.id)
      Class "box"
      OnMouseDown (fun ev ->
        ev.preventDefault()
        ev.stopPropagation()
        match ev.button with
        | 0. -> (box.id, model.mousePos - box.upperLeft) |> PickUp |> dispatch
        | 1. -> Delete box.id |> dispatch
        | _ -> ())
      OnMouseUp (fun ev ->
        ev.preventDefault()
        ev.stopPropagation()
        DropOnBox box.id |> dispatch)
      OnWheel (fun ev ->
        ev.preventDefault()
        ev.stopPropagation()
        if ev.deltaY < 0. then Grow box.id |> dispatch
        elif ev.deltaY > 0. then Shrink box.id |> dispatch)
      Style [
        Width box.size
        Height box.size
        Top (box.upperLeft.y - origin.y)
        Left (box.upperLeft.x - origin.x)
        BackgroundColor box.color
        ZIndex (if dragging then 2 else 1)
        Opacity (if dragging then 0.7 else 1.)
        // Never trigger pointer event (e.g. drop) on currently dragging box;
        // let next layer trigger the event
        PointerEvents (if dragging then "none" else "auto")
      ]
    ] [
      model.hierarchy
      |> BoxHierarchy.childrenOf box.id
      |> List.map (boxWithChildren model dispatch box.upperLeft)
      |> ofList
    ]

  let view model dispatch =
    div [
      Class "main-container"
      OnMouseDown (fun ev ->
        ev.preventDefault()
        match ev.button with
        | 0. -> CreateNew { x = ev.pageX; y = ev.pageY } |> dispatch
        | _ -> ())
      OnMouseUp (fun ev -> ev.preventDefault(); dispatch DropOnEmpty)
      OnMouseMove (fun ev ->
        MouseMove { x = ev.pageX; y = ev.pageY } |> dispatch)
    ] [
      div [
        Class "instructions"
        OnMouseMove (fun ev -> ev.stopPropagation())
        OnMouseDown (fun ev -> ev.stopPropagation())
        OnMouseUp (fun ev -> ev.stopPropagation())
      ] [
        p [] [ str "Drag box to move" ]
        p [] [ str "Drag box inside other box to set as child" ]
        p [] [ str "Left-click empty area to create new box" ]
        p [] [ str "Middle-click box to remove" ]
        p [] [ str "Scroll box to resize" ]
        button [
          OnClick (fun _ -> dispatch Undo)
          Disabled model.undo.IsEmpty
        ] [ str "Undo ("; ofInt model.undo.Length; str ")" ]
        button [
          OnClick (fun _ -> dispatch Redo)
          Disabled model.redo.IsEmpty
        ] [ str "Redo ("; ofInt model.redo.Length; str ")" ]
      ]
      model.hierarchy
      |> BoxHierarchy.topLevelBoxes
      |> List.map (boxWithChildren model dispatch { x=0.; y=0. })
      |> ofList
    ]


open Elmish
open Elmish.React

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run