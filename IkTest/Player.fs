module Player

// *****************************************************************************
// * Import
// *****************************************************************************
open Xelmish.Model
open Xelmish.Viewables
open ImGuiNET
open ImGuiNET.XNA.FSharp
//open Common
open Microsoft.Xna.Framework.Input
open System.Numerics
open MonoGame.Extended

// *****************************************************************************
// * Types
// *****************************************************************************

type Segment = { A: Vector2; B: Vector2; _Length: float32 }
type Body = list<Segment>

// *****************************************************************************
// * Model
// *****************************************************************************

type Model = { Body: Body }

let init (length: float32, numSegments: int ) =
    { Body =
      [for i in 1..numSegments ->
       { _Length = length
         A = Vector2(0f, 0f)
         B = Vector2(0f, length) }] }



// *****************************************************************************

let viewModel = {| |}

let syncViewModel (model: Model) =
    ()

// *****************************************************************************
// * Update Helpers
// *****************************************************************************

let rec follow (body: Body) (target: Vector2) =
    let segment = body.Head

    let direction = Vector2.Normalize(target - segment.B)
    let newTail = -1f * segment._Length * direction + target

    let newSegment = { segment with  A = target; B = newTail }
    match body.Tail with
    | [] -> [newSegment]
    | _ -> newSegment::(follow body.Tail newTail)

// *****************************************************************************
// * Update
// *****************************************************************************

type Message =
    | Force of newModel: Model
    | Follow of Vector2

let update message model =
    match message with
    | Force newModel -> newModel
    | Follow (target: Vector2) ->
        let newBody = follow model.Body target
        { model with Body = newBody }

// *****************************************************************************
// * GUI View
// *****************************************************************************

let buildGui model dispatch =
    syncViewModel model

    let set newModel = dispatch (Force newModel)

    Gui.app
        [ Gui.window
              "Game State"
              [ Gui.text $"A: {model.Body.Head.A.X} {model.Body.Head.A.Y}"
                Gui.text $"B: {model.Body.Head.B.X} {model.Body.Head.B.Y}"]

          Gui.statusBar
              "Status Bar"
              [ fun _ -> Gui.text $"{Mouse.GetState().X}" ()
                fun _ -> Gui.text $"{Mouse.GetState().Y}" ()
                fun _ -> Gui.text $"{ImGui.GetIO().Framerate} FPS" () ] ]

// *****************************************************************************
// * View
// *****************************************************************************

let view model dispatch =
    [
        // Objects
        imgui (buildGui model dispatch)

        yield! [for (idx, segment) in List.indexed(model.Body) ->
                let thickness = (float32 (model.Body.Length - idx) / (float32 model.Body.Length / 10.0f) )
                line Colour.Yellow segment.A segment.B thickness]

        // I/O
        onfollowmouse (fun (x: int, y: int) -> dispatch (Follow(Vector2(float32 x, float32 y))))

        onkeydown Keys.Escape exit
    ]
