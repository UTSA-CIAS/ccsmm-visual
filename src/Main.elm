module Main exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle exposing (Angle)
import Browser
import List.Extra
import Browser.Events
import Camera3d
import Vector3d exposing (Vector3d)
import Color
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Triangle3d
import Viewpoint3d
import Direction3d
import Block3d
import Browser.Dom
import Task

type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , dragging : Bool -- Whether the mouse button is currently down
    , mesh1 : Mesh.Plain WorldCoordinates -- Saved Mesh values for rendering
    , mesh2 : Mesh.Plain WorldCoordinates
    , viewportInfo : Browser.Dom.Viewport
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels) (Quantity Float Pixels) (Quantity Float Pixels)
    | GotViewport Browser.Dom.Viewport


init : () -> ( Model, Cmd Msg )
init () =
    -- Create a couple of Mesh values containing a single triangle each and
    -- store them in the model
    let
        mesh1 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 0 0)
                    (Point3d.meters 1 1 0)
                ]

        mesh2 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)
                ]
    in
    ( { azimuth = Angle.degrees 100
      , elevation = Angle.degrees 20
      , dragging = False
      , mesh1 = mesh1
      , mesh2 = mesh2
      , viewportInfo = { scene = { width = 800, height = 600}, viewport = { x = 0, y=0, width=800, height=600}}
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )
    
floor =
    Scene3d.quad (Material.matte Color.darkGrey)
        (Point3d.meters -1 -1 -0.1)
        (Point3d.meters 1 -1 -0.1)
        (Point3d.meters 1 1 -0.1)
        (Point3d.meters -1 1 -0.1)

{-| Create a cube entity by constructing six square faces with different colors
-}
initialCube : Scene3d.Entity WorldCoordinates
initialCube =
    let
        cubeSize = 0.1
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.meters -cubeSize

        positive =
            Length.meters cubeSize

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces with different colors
        bottom =
            Scene3d.quad (Material.color Color.blue) p1 p2 p3 p4

        top =
            Scene3d.quad (Material.color Color.blue) p5 p6 p7 p8

        front =
            Scene3d.quad (Material.color Color.orange) p2 p3 p7 p6

        back =
            Scene3d.quad (Material.color Color.orange) p1 p4 p8 p5

        left =
            Scene3d.quad (Material.color Color.green) p1 p2 p6 p5

        right =
            Scene3d.quad (Material.color Color.green) p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]
    
    
cubeRadius = 5
    
simpleBlock : Material.Uniform coordinates -> Vector3d Length.Meters coordinates -> Scene3d.Entity coordinates
simpleBlock material offsetVector =
    let
        negative =
            Length.centimeters (-cubeRadius)

        positive =
            Length.centimeters (cubeRadius)
    
        p1 =
            Point3d.xyz (negative) negative negative
                |> Point3d.translateBy offsetVector

        p2 =
            Point3d.xyz positive positive positive
                |> Point3d.translateBy offsetVector
    in
    Scene3d.blockWithShadow material (Block3d.from p1 p2)
    
multiBlock =
    let
        gap =
            1
            
        positionInLayer column (height, depth) =
            Vector3d.centimeters 
                (cubeRadius * 2 * (-column) +  ((-column)*gap))
                (cubeRadius * 2 * toFloat depth + toFloat (depth*gap))
                (cubeRadius * 2 * toFloat height + toFloat (height*gap))
    
        rowGridPositions layer =
            List.map (positionInLayer layer)
                (List.Extra.lift2 Tuple.pair (List.range 0 4) (List.range 0 4))
    
        row1 = List.map (simpleBlock (Material.matte Color.red)) (rowGridPositions 0)
        
        row2 = List.map (simpleBlock (Material.matte Color.orange)) (rowGridPositions 1)
        
        row3 = List.map (simpleBlock (Material.matte Color.yellow)) (rowGridPositions 2)
        
        row4 = List.map (simpleBlock (Material.matte Color.blue)) (rowGridPositions 3)
        
        row5 = List.map (simpleBlock (Material.matte Color.green)) (rowGridPositions 4)
    in
    Scene3d.group (List.concat [row1, row2, row3, row4, row5])


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | dragging = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | dragging = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy x y ->
            if model.dragging then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 0.1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)


                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                let
                    portionFromXRight =
                        (model.viewportInfo.viewport.width - (Pixels.toFloat x)) / model.viewportInfo.viewport.width

                    newAzimuth =
                        Angle.degrees (90 - ((tiltRange /2) - portionFromXRight * tiltRange))

                    -- portion 0 to 1 of mouse position up the viewport
                    portionFromYBottom =
                        (model.viewportInfo.viewport.height - (Pixels.toFloat y)) / model.viewportInfo.viewport.height
                    
                    tiltRange =
                        45

                    newElevation =
                        Angle.degrees ((tiltRange /2) - portionFromYBottom * tiltRange)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

        GotViewport viewportInfo ->
            ( { model | viewportInfo = viewportInfo}, Cmd.none)


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map4 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
        (Decode.field "clientX" (Decode.map Pixels.float Decode.float))
        (Decode.field "clientY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            , Browser.Events.onMouseDown (Decode.succeed MouseDown)
            ]

    -- else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        -- Browser.Events.onMouseDown (Decode.succeed MouseDown)


view : Model -> Browser.Document Msg
view model =
    let
        halfCubeWidth =
            -- radius to diameter of 2.5 blocks
            cubeRadius * 5 - 2.5

        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.centimeters (-halfCubeWidth) (halfCubeWidth) (halfCubeWidth)
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 2
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "CCSMM Cube"
    , body =
        [ Scene3d.sunny
            { upDirection = Direction3d.positiveZ
            , sunlightDirection = Direction3d.xyZ model.azimuth (Angle.degrees -150)
            , shadows = True
            , camera = camera
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int (round model.viewportInfo.viewport.width), Pixels.int (round model.viewportInfo.viewport.height) )
            , background = Scene3d.transparentBackground
            , entities =
                [ multiBlock
                -- , floor
                ]
            }
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
