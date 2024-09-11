module Main exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}
import Svg exposing (Svg)
import Svg.Attributes
import Angle exposing (Angle)
import Html.Events.Extra.Wheel as Wheel
import Browser
import List.Extra
import Geometry.Svg as Svg
import Frame2d
import Point2d
import Frame3d exposing (Frame3d)
import Browser.Events
import Camera3d
import Vector3d exposing (Vector3d)
import Color
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels exposing (Pixels)
import Point3d
import Point3d.Projection as Point3d
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
import Html
import Html.Attributes
import Axis3d
import Rectangle2d
import Axis2d
import Direction2d
import Point3d exposing (Point3d)
import Block3d exposing (Block3d)
import Rectangle2d exposing (Rectangle2d)
import Length exposing (Meters)
import Polyline3d exposing (Polyline3d)
import Circle2d
import Svg.Events

type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , hoverAzimuth : Angle -- Orbiting angle of the camera around the focal point
    , hoverElevation : Angle -- Angle of the camera up from the XY plane
    , dragging : Bool -- Whether the mouse button is currently down
    , mesh1 : Mesh.Plain WorldCoordinates -- Saved Mesh values for rendering
    , mesh2 : Mesh.Plain WorldCoordinates
    , viewportInfo : Browser.Dom.Viewport
    , pointerX : Quantity Float Pixels 
    , pointerY : Quantity Float Pixels
    , blockNearMouse : Maybe Block
    , distance : Quantity Float Meters
    , focusedAspect : Maybe Aspect
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels) (Quantity Float Pixels) (Quantity Float Pixels)
    | GotViewport Browser.Dom.Viewport
    | Resize
    | Scroll Wheel.Event
    | ToggleAspectFocus Aspect


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
    ( { azimuth = Angle.degrees 114
      , elevation = Angle.degrees 23
      , hoverAzimuth = Angle.degrees 45
      , hoverElevation = Angle.degrees 200
      , dragging = False
      , mesh1 = mesh1
      , mesh2 = mesh2
      , viewportInfo = { scene = { width = 800, height = 600}, viewport = { x = 0, y=0, width=800, height=600}}
      , pointerX = Pixels.float 0
      , pointerY = Pixels.float 0
      , blockNearMouse = Nothing
      , distance = Length.meters 1.8
      , focusedAspect = Nothing
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )
    
floor =
    Scene3d.quad (Material.matte Color.white)
        (Point3d.meters -1 -1 -0.3)
        (Point3d.meters 1 -1 -0.3)
        (Point3d.meters 1 1 -0.3)
        (Point3d.meters -1 1 -0.3)


    
blockWidthX = 10

blockHeightZ = 10

blockDepthY = 10
    
blockToEntity : Model -> Block -> Scene3d.Entity Float
blockToEntity model block =
    let
        columnColor =
            case block.column of
                0 -> Color.red
                1 -> Color.orange
                2 -> Color.yellow
                3 -> Color.blue
                4 -> Color.green
                _ -> Color.black

        blockMaterial =
            Material.nonmetal { baseColor = columnColor, roughness = 0}
    in
    if model.blockNearMouse == Just block
    then
            Scene3d.blockWithShadow blockMaterial (block.block3d)
    
    else if blockMatchesFocusedAspect block model
    then
        Scene3d.mesh (Material.color columnColor) <| Mesh.lineSegments  <| Block3d.edges block.block3d
    
    else if model.blockNearMouse == Nothing && model.focusedAspect == Nothing
    then
        Scene3d.blockWithShadow blockMaterial (block.block3d)
    
    else if model.focusedAspect == Nothing
    then
        Scene3d.point {radius = Pixels.float 10} (Material.color columnColor) block.position
    else
        Scene3d.point {radius = Pixels.float 1} (Material.color columnColor) block.position
    

blockMatchesFocusedAspect block model =
    Just block.levelLabel == Maybe.map .label model.focusedAspect
        || Just block.scopeLabel == Maybe.map .label model.focusedAspect
        || Just block.dimensionLabel == Maybe.map .label model.focusedAspect



blockGap =
            1

type alias Aspect =
    { position : Int
    , label: String
    }

rows =
    -- four dimensions, bottom to top
    [ Aspect 3 "Awareness" 
    , Aspect 2 "Information Sharing"
    , Aspect 1 "Policy"
    , Aspect 0 "Planning"
    ]

depthLayers =
    -- five scopes, front to back
    [ Aspect 4 "Individual"
    , Aspect 3 "Organization"
    , Aspect 2 "Community"
    , Aspect 1 "State"
    , Aspect 0 "Nation"
    ]


columns =
    -- five levels of improvement, left to right
    [ Aspect 0 "Level 1 (Initial)"
    , Aspect 1 "Level 2 (Established)"
    , Aspect 2 "Level 3 (Self-Assessed)"
    , Aspect 3 "Level 4 (Integrated)"
    , Aspect 4 "Level 5 (Vanguard)"
    ]

type alias Block =
    { column : Int
    , depthLayer : Int
    , row : Int
    , levelLabel : String
    , scopeLabel : String
    , dimensionLabel : String
    , position : Point3d Length.Meters Float
    , description : String
    , block3d : Block3d.Block3d Length.Meters Float
    }

allBlocks =
    let
        blockCombos =
            List.Extra.cartesianProduct [columns, depthLayers, rows]
    in
    List.filterMap aspectsToBlock blockCombos

aspectsToBlock : List Aspect -> Maybe Block
aspectsToBlock aspectList =
    case aspectList of
        [col, layer, row] ->
            let
                blockVector =
                    blockPositionVector (toFloat col.position) (toFloat layer.position) (toFloat row.position)
            in
            Just <|
                { column = col.position
                , depthLayer = layer.position
                , row = row.position
                , levelLabel = col.label
                , scopeLabel = layer.label
                , dimensionLabel = row.label
                , position = blockVector
                , description = col.label ++ " " ++ layer.label ++ " " ++ row.label
                , block3d = Block3d.centeredOn 
                    (Frame3d.atPoint blockVector) 
                    (Length.centimeters blockWidthX, Length.centimeters blockHeightZ, Length.centimeters blockDepthY)
                }

        _ -> Nothing

rowCount = List.length rows        

colCount = List.length columns

layerCount = List.length depthLayers

blockPositionVector : Float -> Float -> Float -> Point3d Meters Float
blockPositionVector column layer row =
    let
        heightNeeded = toFloat <| ((rowCount - 1) * blockDepthY) + (blockGap * (rowCount - 1))

        widthNeeded = toFloat <| ((colCount - 1) * blockWidthX) + (blockGap * (colCount - 1))

        depthNeeded = toFloat <| ((layerCount - 1) * blockDepthY) + (blockGap * (layerCount - 1))
    in
        Point3d.centimeters 
            (blockWidthX  * -(column) +  (-(column)*blockGap) + (widthNeeded /2))
            (blockDepthY  * (layer) +  (layer*blockGap) - (depthNeeded / 2))
            (blockHeightZ * (row) +  (row*blockGap) - (heightNeeded / 2))


entireCube model =
    Scene3d.group <| List.map (blockToEntity model) allBlocks 


blockNearestToMouse model =
    let
        allFocusedBlocks =
            case model.focusedAspect of
                Just _ ->
                    List.filter (\b -> blockMatchesFocusedAspect b model) allBlocks

                Nothing ->
                    allBlocks

        blockDistanceFromPointer block =
            Point2d.distanceFrom
                (Point3d.toScreenSpace (camera model) (screenRectangle model |> Rectangle2d.relativeTo (topLeftFrame model)) block.position)
                (Point2d.xy model.pointerX model.pointerY)
            |> Pixels.toFloat

        blocksWithDistance =
            List.map (\b -> (blockDistanceFromPointer b, b)) allFocusedBlocks

        nearbyBlocksWithDistance =
            List.filter (\(d, _) -> d < 75) blocksWithDistance
    in
    List.Extra.minimumBy Tuple.first nearbyBlocksWithDistance
    |> Maybe.map Tuple.second



aspectLabels model =
    let
        perspectiveBlocksVerticalRear = Quantity.equalWithin (Angle.degrees 7) (model.elevation) (Angle.degrees 0)

        perspectiveBlocksLayerLabels = Quantity.equalWithin (Angle.degrees 12) (model.elevation) (Angle.degrees 0)

        extraLeftColumn = -0.5
        extraRightColumn = toFloat colCount - 0.5

        extraTopRow = toFloat rowCount - 0.5
        
        extraBottomRow = -0.5

        extraFrontLayer = toFloat layerCount - 0.5

        extraBackLayer = -0.5

        lookingAtLeftSide =
            Quantity.equalWithin (Angle.degrees 90) (model.azimuth) (Angle.degrees 0)

        (defaultColumn, alignEnd) = 
            if lookingAtLeftSide then
                (extraRightColumn, False)
            else (extraLeftColumn, True)

        lookingAtTop = Quantity.equalWithin (Angle.degrees 90) (model.elevation) (Angle.degrees 90)

        defaultRow = 
            if lookingAtTop then
                (extraTopRow)
            else (extraBottomRow)

        (rotationDegrees, alignRotatedTextEnd) =
            case (lookingAtLeftSide, lookingAtTop) of
                (False, False) -> (45, False)
                (False, True) -> (-45, False)
                (True, False) -> (-45, True)
                (True, True) -> (45, True)

        defaultLayerForColumns = 
            if perspectiveBlocksVerticalRear then
                extraFrontLayer
            else extraBackLayer


        columnLabelsWithPoints = List.map (\a -> (a, blockPositionVector (toFloat a.position) defaultLayerForColumns defaultRow, rotationDegrees) ) columns

        rowLabelsWithPoints = List.map (\a -> (a, blockPositionVector defaultColumn extraFrontLayer (toFloat a.position), 0)) rows

        layerLabelsWithPoints = if perspectiveBlocksLayerLabels then []
            else List.map (\a -> (a, blockPositionVector defaultColumn (toFloat a.position) defaultRow, 0)) depthLayers
    
        aspectToSvgLabel : (Aspect, Point3d Meters Float, Float) -> Svg Msg
        aspectToSvgLabel (aspect, point, rotation) =
            let
                vertex =
                    Point3d.toScreenSpace (camera model) (screenRectangle model) point

                x = (Pixels.toFloat (Point2d.xCoordinate vertex) - 4)

                y = (Pixels.toFloat (Point2d.yCoordinate vertex) + 4)

                fullRotationString = String.fromFloat rotation ++ "," ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y)

                weightMatchesHoverBlock hoverBlock =
                    if hoverBlock.levelLabel == aspect.label || hoverBlock.scopeLabel == aspect.label || hoverBlock.dimensionLabel == aspect.label then
                        "bold"
                    else
                        "normal"

                (sizeMatchesFocus, decorationMatchesFocus) =
                    if Just aspect.label == Maybe.map .label model.focusedAspect then
                        ("20px", "underline")
                    else
                        ("12px", "none")
                        
            in
            Svg.text_
                [ Svg.Attributes.fill "rgb(92, 92, 92)"
                , Svg.Attributes.fontFamily "sans-serif"
                , Svg.Attributes.fontSize sizeMatchesFocus
                , Svg.Attributes.fontWeight (Maybe.map weightMatchesHoverBlock model.blockNearMouse |> Maybe.withDefault "normal")
                , Svg.Attributes.textDecoration decorationMatchesFocus
                , Svg.Attributes.stroke "none"
                , Svg.Attributes.style "user-select: none; cursor: pointer"
                , Svg.Events.onClick (ToggleAspectFocus aspect)
                , Svg.Attributes.x (String.fromFloat x)
                , Svg.Attributes.y (String.fromFloat y)
                , Svg.Attributes.textAnchor (if alignEnd && (rotation == 0) || (rotation /= 0 && alignRotatedTextEnd) then "end" else "start")
                , Svg.Attributes.transform ("rotate(" ++ fullRotationString ++ ")")
                ]
                [ Svg.text (aspect.label) ]
                -- Hack: flip the text upside down since our later
                -- 'Svg.relativeTo topLeftFrame' call will flip it
                -- back right side up
                |> Svg.mirrorAcross (Axis2d.through vertex Direction2d.x)
            
    in
    List.map aspectToSvgLabel (columnLabelsWithPoints ++ rowLabelsWithPoints ++ layerLabelsWithPoints)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize ->
            ( model, Task.perform GotViewport Browser.Dom.getViewport)
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | dragging = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | dragging = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy x y ->
            let
                portionFromXRight =
                    (model.viewportInfo.viewport.width - (Pixels.toFloat x)) / model.viewportInfo.viewport.width

                hoverAzimuth =
                    Angle.degrees (90 - ((tiltRange /2) - portionFromXRight * tiltRange))
                    |> Angle.normalize

                -- portion 0 to 1 of mouse position up the viewport
                portionFromYBottom =
                    (model.viewportInfo.viewport.height - (Pixels.toFloat y)) / model.viewportInfo.viewport.height
                
                tiltRange =
                    90

                hoverElevation =
                    Angle.degrees ((tiltRange /2) - portionFromYBottom * tiltRange)
                    |> Angle.normalize
            in
            
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
                            |> Angle.normalize
                            |> Quantity.clamp (Angle.degrees 20) (Angle.degrees 160)


                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                            |> Angle.normalize
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation, hoverAzimuth = hoverAzimuth, hoverElevation = hoverElevation
                    , pointerX = x, pointerY = y, blockNearMouse = Nothing
                    }
                , Cmd.none
                )

            else
                ( { model | 
                    hoverAzimuth = hoverAzimuth
                    , hoverElevation = hoverElevation
                    , pointerX = x
                    , pointerY = y
                    , blockNearMouse = blockNearestToMouse {model | pointerX = x, pointerY = y }
                    }
                , Cmd.none
                )

        GotViewport viewportInfo ->
            ( { model | viewportInfo = viewportInfo}, Cmd.none)

        Scroll {deltaY} ->
            ( { model | distance = Quantity.clamp (Length.meters 1) (Length.meters 2) (Quantity.plus model.distance (Length.millimeters deltaY)) }, Cmd.none)

        ToggleAspectFocus aspect ->
            ( { model | focusedAspect = 
                if model.focusedAspect == Nothing || model.focusedAspect /= Just aspect then 
                    Just aspect 
                else Nothing
                }, Cmd.none)


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
            , Browser.Events.onResize (\_ _ -> Resize)
            ]

    -- else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        -- Browser.Events.onMouseDown (Decode.succeed MouseDown)



-- Create a viewpoint by orbiting around a Z axis through the given
-- focal point, with azimuth measured from the positive X direction
-- towards positive Y
viewpoint model =
    Viewpoint3d.orbitZ
        { focalPoint = Point3d.origin
        , azimuth = model.azimuth
        , elevation = model.elevation
        , distance = model.distance
        }


camera model =
    Camera3d.perspective
        { viewpoint = viewpoint model
        , verticalFieldOfView = Angle.degrees 30
        }


-- Used for converting from coordinates relative to the bottom-left
-- corner of the 2D drawing into coordinates relative to the top-left
-- corner (which is what SVG natively works in)
topLeftFrame model =
    Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float model.viewportInfo.viewport.height))
        |> Frame2d.reverseY


-- Defines the shape of the 'screen' that we will be using when
-- projecting 3D points into 2D
screenRectangle model =
    Rectangle2d.from Point2d.origin (Point2d.pixels model.viewportInfo.viewport.width model.viewportInfo.viewport.height)


view : Model -> Browser.Document Msg
view model =
    let
        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        vertices2d =
            allBlocks
                |> List.map .position
                --|> List.map (Point3d.rotateAround Axis3d.z angle)
                |> List.map (Point3d.toScreenSpace (camera model) (screenRectangle model))

        -- Create text SVG labels beside each projected 2D point
        blockToSvgLabel : Block -> Svg msg
        blockToSvgLabel block =
            let
                vertex =
                    Point3d.toScreenSpace (camera model) (screenRectangle model) block.position
            in
            Svg.text_
                [ Svg.Attributes.fill "rgb(92, 92, 92)"
                , Svg.Attributes.fontFamily "sans-serif"
                , Svg.Attributes.fontSize "12px"
                , Svg.Attributes.stroke "none"
                , Svg.Attributes.style "user-select: none"
                , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate vertex) - 4))
                , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate vertex) + 4))
                ]
                [ Svg.text (String.fromInt <| block.column + 1) ]
                -- Hack: flip the text upside down since our later
                -- 'Svg.relativeTo topLeftFrame' call will flip it
                -- back right side up
                |> Svg.mirrorAcross (Axis2d.through vertex Direction2d.x)

        blockToSvgCircle : Block -> Svg msg
        blockToSvgCircle block =
            let
                vertex =
                    Point3d.toScreenSpace (camera model) (screenRectangle model) block.position
            in
            Svg.circle2d
                [ Svg.Attributes.stroke "grey"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                (Circle2d.withRadius (Pixels.float 6) vertex)


    in
    { title = "CCSMM Cube"
    , body =
        [ Html.main_ 
            (if model.dragging then
                [Html.Attributes.style "cursor" "grabbing", Wheel.onWheel Scroll] 
            else if model.blockNearMouse /= Nothing then
                [Html.Attributes.style "cursor" "help"]
            else 
                [Html.Attributes.style "cursor" "grab"]
            )
            [   Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.xyZ 
                    (Quantity.difference (Quantity.plus model.azimuth (Angle.degrees 45)) model.hoverAzimuth) 
                    (Quantity.difference (Quantity.plus model.elevation (Angle.degrees 200)) model.hoverElevation) --(Angle.degrees 45) ((Angle.degrees 200)) 
                , shadows = True
                , camera = camera model
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int (round model.viewportInfo.viewport.width), Pixels.int (round model.viewportInfo.viewport.height) )
                , background = Scene3d.transparentBackground
                , entities =
                    [ entireCube model
                    -- , floor
                    ]
                }
            , Svg.svg
                [ Svg.Attributes.width (String.fromFloat model.viewportInfo.viewport.width)
                , Svg.Attributes.height (String.fromFloat model.viewportInfo.viewport.height)
                , Svg.Attributes.style "position: absolute; top: 0; left: 0; right: 0; bottom: 0;"
                ]
                [ Svg.relativeTo (topLeftFrame model)
                    (Svg.g [] (aspectLabels model))
                ]
            ]
        
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
