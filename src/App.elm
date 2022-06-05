module App exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Ease
import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Parameters exposing (..)
import Railway exposing
    (Coords
    , ShuntingYard
    , Shunt
    , ShuntingYardError(..)
    , ShuntReason(..)
    , shunt_reason_description
    , RailwayPiece(..)
    , Direction(..)
    , Track(..)
    , TrackAction(..)
    , TrainAction(..)
    , Route
    , Train
    , shunting_yard
    , shunt_moves
    , railway_length
    , railway_piece_path
    , point_on_railway
    , angle_on_railway
    , long_junction_down
    , long_junction_up
    , short_junction_down
    , train_distance
    , train_position
    , action_pieces
    , track_name
    , track_position
    , track_direction
    , shunting_error_description
    , plan_route
    , append_input
    , pop_input
    , pop_stack
    , pop_output
    , push_stack
    , push_output
    , unshift_input
    , shunting_yard_step
    )
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Token exposing (Token(..), BinaryOp, tokenise)
import Tuple exposing (first, second)
import Util exposing (..)

-- The app will take over the whole document
main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
    { expr : String
    , yard : Result ShuntingYardError ShuntingYard
    , result : Result ShuntingYardError ShuntingYard
    , moves : List Shunt
    , time : Float
    , train : Train
    , train_speed : Float
    , input_action: (Float, TrackAction)
    , stack_action: (Float, TrackAction)
    , output_action: (Float, TrackAction)
    , thinking : ShuntReason
    , arriving_token: Maybe (Token, Float)
    }

type Msg
    = SetExpr String
    | SetTrainSpeed String
    | Frame Float
    | PushToken Token
    | DropToken

init_model : Model
init_model =
    { expr = "1 + 2*(3-4)^2 + 5^(3+4+5)"
    , yard = Ok <| shunting_yard []
    , result = Ok <| shunting_yard []
    , moves = []
    , time = 0
    , train =
        { route = []
        , direction = Forwards
        , position = (0, 0)
        , start_time = 0
        , pulling = Nothing
        }
    , train_speed = 0
    , input_action = (0, Stationary)
    , stack_action = (0, Stationary)
    , output_action = (0, Stationary)
    , thinking = ReadInput
    , arriving_token = Nothing
    } |> set_shunting_yard

init : () -> (Model, Cmd Msg)
init _ = (init_model, Cmd.none)

nocmd : Model -> (Model, Cmd Msg)
nocmd model = (model, Cmd.none)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    SetExpr expr -> { init_model | expr = expr, train_speed = 0 } |> set_shunting_yard |> nocmd
    SetTrainSpeed s -> case String.toFloat s of
        Just speed -> { model | train_speed = speed } |> nocmd
        Nothing -> nocmd model
    PushToken t -> model |> append_arriving_token |> (\m -> { m | arriving_token = Just (t,incredibly_long_track_length) }) |> nocmd
    DropToken -> case model.arriving_token of
        Just _ -> { model | arriving_token = Nothing } |> nocmd
        Nothing -> { model | yard = Result.map unshift_input model.yard } |> nocmd
    Frame dt -> { model | time = model.time + dt*model.train_speed } |> (\m -> if m.train_speed>0 then update_train m else m) |> update_arriving_tokens |> nocmd

set_shunting_yard : Model -> Model
set_shunting_yard model =
    case tokenise model.expr of
        Ok tokens -> 
            let
                train = model.train
                yard = shunting_yard tokens
                (shunts, result) = plan_route yard
                route = (List.map shunt_moves >> List.concat) shunts
            in
                { model | yard = Ok yard, train = { train | route = route }, result = result }
        _ -> model

subscriptions : Model -> Sub Msg
subscriptions model = onAnimationFrameDelta Frame

railway_path : Coords -> List RailwayPiece -> String
railway_path (x,y) pieces = String.join " " (["M "++(ff x)++" "++(ff y)]++(List.map (\p -> ""++(railway_piece_path p)) pieces))

view_rail_under : (Coords, List RailwayPiece) -> Svg Msg
view_rail_under ((x,y), pieces) =
    let
        d = SA.d <| railway_path (x,y) pieces
    in
        Svg.path
            [ d
            , SA.stroke "#eee"
            , SA.fill "none"
            , SA.strokeWidth <| ff <| track_gauge*3
            ]
            []

view_rails : (Coords, List RailwayPiece) -> Svg Msg
view_rails ((x, y), pieces) =
    let
        d = SA.d <| railway_path (x,y) pieces
    in
        Svg.g
            [ SA.style "mix-blend-mode: multiply" ]
            [ Svg.path
                [ d
                , SA.stroke "#444"
                , SA.fill "none"
                , SA.strokeWidth <| ff track_gauge
                ]
                []   
            , Svg.path
                [ d
                , SA.stroke "white"
                , SA.fill "none"
                , SA.strokeWidth <| ff <| track_gauge * 0.9
                ]
                []   
            , Svg.path
                [ d
                , SA.stroke "black"
                , SA.fill "none"
                , SA.strokeWidth <| ff <| track_gauge * 1.3
                , SA.strokeDasharray "2 8"
                , SA.strokeDashoffset "0"
                ]
                []
            ]

debug_block : String -> Html Msg
debug_block content =
    H.pre
        []
        [ H.text content ]


view : Model -> Browser.Document Msg
view model = 
    { title = "Shunting yard"
    , body =
        [ div
            [ HA.id "controls" ]
            [ speed_input model
            , keypad model
            ]
        , describe_thinking model
        , view_railway model
        , view_algorithm model
        , footer
--        , debug_block <| "stack: " ++ (Debug.toString model.train)
--        , debug_block <| debug_shunt_result model.yard
--        , debug_block <| model.expr
--        , debug_block <| (++) "moves: " <| String.join " " <| List.map debug_shunt model.moves
--        , debug_block <| Debug.toString model.train
--        , debug_block <| Debug.toString <| (List.map (\t -> train_distance t model.train) [0,20, 40, 60 ,80, 100, 120])
        ]
    }

keypad model =
    let
        keypad_button always_on onClick label class =
            H.button
                [ HA.class class
                , HE.onClick onClick
                , HA.disabled (model.train.route /= [] && not always_on)
                ]
                [ H.text label ]
    in
        H.div
            [ HA.id "keypad" ]
            ([ keypad_button True (SetExpr "") "C" "control reset"
             , keypad_button False DropToken "⌫" "control drop-token"
             , keypad_button False (PushToken LeftBracket) "(" "token left-bracket"
             , keypad_button False (PushToken RightBracket) ")" "token right-bracket"
             , keypad_button False (PushToken (Name "a")) "a" "token name"
            ]++(List.map (\i ->
                keypad_button False (PushToken (LiteralNumber i)) (String.fromInt i) "token number"
                )
                (List.range 0 9)
            )++(List.map (\op ->
                keypad_button False (PushToken (BinaryOpToken op)) op.display "token op"
                )
                Token.ops
            ))

view_algorithm model =
    let
        algorithm_step reason text =
            H.p
                [ HA.classList
                    [ ("step", True)
                    , ("active", model.thinking == reason) 
                    ]
                ]
                [ H.text text ]
    in
        H.div
            [ HA.id "algorithm" ]
            [ H.h2 [] [H.text "The algorithm" ]
            , H.ol
                []
                [ H.li [] 
                    [ H.p [] [ H.text "While there are tokens on the input" ]
                    , H.ol []
                        [ H.li [] [algorithm_step ReadInput "Read a token from the input" ]
                        , H.li [] 
                            [ H.text "If the token is:"
                            , H.dl
                                []
                                [ H.dt [] [ H.text "A number or variable" ]
                                , H.dd []
                                    [ algorithm_step InputToOutput "Push it to the output" ]
                                , H.dt [] [ H.text "An operator o₁" ]
                                , H.dd []
                                    [ algorithm_step PopStackToOutput "While there is an operator o₂ at the top of the stack with greater precedence, or with equal precedence and o₁ is left associative, push o₂ from the stack to the output"
                                    , algorithm_step OpToStack "Push o₁ to the stack"
                                    ]
                                , H.dt [] [ H.text "A left bracket" ]
                                , H.dd []
                                    [ algorithm_step LeftBracketToStack "Push it to the stack" ]
                                , H.dt [] [ H.text "A right bracket" ]
                                , H.dd []
                                    [ algorithm_step PopOpsUntilBracket "While there is an operator at the top of the stack that is not a left bracket, push it to the output"
                                    , algorithm_step DiscardBrackets "Discard the left and right parentheses"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , H.li []
                    [ H.p [] [ H.text "While there are operators left on the stack" ]
                    , algorithm_step RemainingOps "Push them to the output"
                    ]
                ]
            ]

describe_thinking : Model -> Html Msg
describe_thinking model =
    let
        text =
            if model.train.route == [] then
                case model.result of
                    Ok _ -> shunt_reason_description model.thinking
                    Err err -> shunting_error_description err
            else
                shunt_reason_description model.thinking
    in
        H.p
            [ HA.id "thinking" ]
            [ H.text text ]

speed_input model =
    H.label
        []
        [ H.text "Train speed"
        , H.input
            [ HA.type_ "range"
            , HA.min "0"
            , HA.max "5"
            , HA.step "0.01"
            , HA.value <| ff model.train_speed
            , HE.onInput SetTrainSpeed
            , HA.id "speed"
            ]
            []
        ]

update_train : Model -> Model
update_train model =
    let
        train = model.train
        time = model.time
    in
        case train.route of
            action::rest -> case action of
                FollowRoute pieces -> 
                    let
                        l = railway_length pieces
                        d = train_distance time train
                        end = case train.direction of
                            Forwards -> d >= l
                            Backwards -> d <= 0
                        (x, y) = train.position
                        (dx, dy) = point_on_railway (0, 0) pieces l
                        endpos = case train.direction of
                            Forwards -> (x+dx, y+dy)
                            Backwards -> (x-dx, y-dy)
                    in
                        if end then
                            let
                                ntrain =
                                    { train 
                                    | position = endpos
                                    , route = rest
                                    , direction = if train.direction == Forwards then Backwards else Forwards
                                    , start_time = time
                                    }
                            in
                                { model | train = ntrain } |> stop_advancing |> update_train
                        else
                            model

                PickUpCarriage track -> 
                    case model.yard of
                        Ok yard ->
                            let
                                res = case track of
                                    Input -> pop_input yard
                                    Stack -> pop_stack yard
                                    Output -> pop_output yard
                                    Bin -> Err (EmptyTrack Bin)
                                next_drop = 
                                    rest 
                                    |> List.filter (\p -> 
                                        case p of
                                            DropCarriage t2 -> True
                                            _ -> False
                                        )
                                    |> List.head
                                    |> Maybe.withDefault (DropCarriage Input)
                                    |> (\p -> case p of
                                        DropCarriage t2 -> t2
                                        _ -> Input
                                       )
                            in
                                case res of
                                    Ok (t, yard2) -> 
                                        {model | train = { train | route = rest, start_time = time, pulling = Just t}, yard = Ok yard2} 
                                        |> set_track_action (model.time, Advancing) track 
                                        |> set_track_action (model.time, Reversing) next_drop 
                                        |> update_train
                                    Err _ -> model

                        Err _ -> model

                DropCarriage track ->
                    case (model.yard, train.pulling) of
                        (Ok yard, Just carriage) ->
                            let
                                yard2 = case track of
                                    Stack -> push_stack carriage yard
                                    Output -> push_output carriage yard
                                    _ -> yard
                            in
                                { model | train = { train | pulling = Nothing, route = rest, start_time = time }, yard = Ok yard2 } 
                                |> set_track_action (model.time, Stationary) track
                                |> update_train

                        _ -> model

                DescribeThinking reason ->
                    { model | thinking = reason, train = { train | route = rest } } |> update_train

            [] -> case model.yard of
                Ok yard ->
                    let
                        (shunts, result) = plan_route yard
                        route = (List.map shunt_moves >> List.concat) shunts
                    in
                        if route /= [] then
                            { model | result = result, train = {train | route = route, start_time=time} } |> update_train
                        else
                            { model | result = result }
                Err _ -> model

update_arriving_tokens model =
    case model.arriving_token of
        Just (t,x) ->
            let
                offset = track_offset model Input
                target = offset + carriage_gap
                dx = (target-x)
                nx = x + dx*0.3
            in
                if (abs dx)< 1 then
                    append_arriving_token model
                else
                    { model | arriving_token = Just (t,nx) }
                
        Nothing -> model

append_arriving_token model = 
    case model.arriving_token of
        Just (t,x) -> { model | yard = Result.map (append_input t) model.yard, arriving_token = Nothing }
        Nothing -> model

set_track_action action track model = case track of
    Input -> { model | input_action = action }
    Stack -> { model | stack_action = action }
    Output -> { model | output_action = action }
    Bin -> model

stop_advancing model =
    let
        fix (time, action) = case action of
            Advancing -> (time, Stationary)
            _ -> (time, action)
    in
        { model 
        | input_action = fix model.input_action
        , stack_action = fix model.stack_action
        , output_action = fix model.output_action
        }

is_follow_route piece = case piece of
    FollowRoute _ -> True
    _ -> False

view_train : Float -> Train -> Svg Msg
view_train time train =
    let
        d = train_distance time train
        (x, y) = train_position d train
        width = train_length
        height = track_gauge * 1.5
        current_segment = Maybe.withDefault [] (Maybe.map action_pieces <| List.head <| List.filter is_follow_route train.route)
        angle = angle_on_railway current_segment d
        carriage_offset = width
        (startx,starty) = train_position 0 train
        (carriage_pos, carriage_angle) = 
            if d < carriage_offset then
                ((x - (cos (angle_on_railway current_segment (if d < carriage_offset then 0 else (d-carriage_offset))))*carriage_offset, starty), pi)
            else
                (train_position (d-carriage_offset) train, angle_on_railway current_segment (d-carriage_offset))

        (connect_x, connect_y) =
            let
                (cx,cy) = carriage_pos
            in
                case train.pulling of
                    Just _ -> (cx + (cos carriage_angle)*carriage_length/2, cy + (sin carriage_angle)*carriage_length/2)
                    Nothing -> (x - (cos angle)*width*0.6, y - (sin angle)*width*0.6)
        connector =
            Svg.line
                [ SA.x1 <| ff <| x
                , SA.y1 <| ff <| y
                , SA.x2 <| ff <| connect_x
                , SA.y2 <| ff <| connect_y
                , SA.stroke "black"
                , SA.strokeWidth "4"
                ]
                []
        body = 
            Svg.path
                [ SA.d <| strf "M % % l % % a % % 0 0 1 % % l % % z"(List.map ff [-width/2, -height/2, width*0.9, 0, height*0.6, height*0.6, 0, height, -width*0.9, 0])
                , SA.fill "hsl(0,0%,50%)"
                , SA.stroke "black"
                , SA.strokeWidth "2"
                ]
                []
        spout = Svg.g []
            [ Svg.circle
                [ SA.cx <| ff <| width*0.45
                , SA.cy <| ff <| 0
                , SA.r <| ff <| height/4
                , SA.fill "hsl(0,0%,50%)"
                , SA.stroke "black"
                , SA.strokeWidth "2"
                ]
                []
            , Svg.circle
                [ SA.cx <| ff <| width*0.45
                , SA.cy <| ff <| 0
                , SA.r <| ff <| height/6
                , SA.fill "black"
                ]
                []
            ]
        knobble =
            Svg.circle
                [ SA.cx <| ff <| width*0.1
                , SA.cy <| ff <| 0
                , SA.r <| ff <| height/5
                , SA.fill "hsl(120,60%,50%)"
                , SA.stroke "black"
                , SA.strokeWidth "2"
                ]
                []
        rivet i =
            Svg.path
                [ SA.d <| strf "M % % a % % 0 0 1 % %" (List.map ff [width*0.3 - (toFloat i)*10, -height/2, height, height, 0 , height])
                , SA.stroke "hsl(0,0%,30%)"
                , SA.strokeWidth "2"
                , SA.fill "none"
                ]
                []

        cab =
            Svg.rect
                [ SA.x <| ff <| -width*0.5
                , SA.y <| ff <| -height*0.6
                , SA.width <| ff <| width*0.3
                , SA.height <| ff <| height*1.2
                , SA.fill "black"
                ]
                []
    in
        Svg.g
            [  ]
            (   [connector]
                ++
                [ Svg.g
                    [ SA.transform <| "translate("++(ff x)++", "++(ff y)++") rotate(" ++ (ff <| radians_to_degrees angle) ++ ")"
                    ]
                    ([ body ]++(List.map rivet (List.range 0 4))++[ spout, knobble, cab ])
                ]
                ++(case train.pulling of
                    Nothing -> []
                    Just c -> 
                        [ view_carriage c carriage_pos carriage_angle
                        ]
                )
            )

view_queues : Model -> List (Svg Msg)
view_queues model =
    ([ view_carriages model Input 
    , view_carriages model Stack
    , view_carriages model Output
    ]
    ++(case model.arriving_token of
        Just (t,x) -> [view_carriage t (x,0) 0]
        Nothing -> []
    )
    )

track_offset : Model -> Track -> Float
track_offset model track =
    let
        (start_time, action) = case track of
            Input -> model.input_action
            Stack -> model.stack_action
            Output -> model.output_action
            Bin -> (0,Stationary)
        carriages = case model.yard of
            Ok yard -> case track of
                Input -> yard.input
                Stack -> yard.stack
                Output -> yard.output
                Bin -> []
            Err _ -> []
        n = List.length carriages
        action_offset = case action of
            Stationary -> 0
            Advancing -> carriage_gap - (clamp 0 carriage_gap (train_distance model.time model.train))
            Reversing -> case (List.head model.train.route, List.head (List.drop 1 model.train.route)) of
                (Just (FollowRoute pieces), Just (DropCarriage t2)) -> 
                    if t2==track then
                        let
                            d = train_distance model.time model.train
                            l = railway_length pieces
                        in
                            clamp 0 carriage_gap (carriage_gap - d)
                    else
                        0
                _ -> 0
    in
        carriage_gap * (toFloat (n-1)) + train_length + action_offset

view_carriages : Model -> Track -> Svg Msg
view_carriages model track =
    let
        offset = track_offset model track
        (x,y) = track_position track
        direction = track_direction track
        carriages = case model.yard of
            Ok yard -> case track of
                Input -> yard.input
                Stack -> yard.stack
                Output -> yard.output
                Bin -> []
            Err _ -> []
        angle = case track of
            Input -> pi
            _ -> 0
    in
        Svg.g
            []
            (List.indexedMap (\i -> \c -> view_carriage c (x+direction*(offset - (toFloat i)*carriage_gap), y) angle) (List.reverse carriages))


view_carriage : Token -> Coords -> Float -> Svg Msg
view_carriage carriage (x, y) angle =
    let
        width = carriage_length
        height = track_gauge * 1.5
        class = case carriage of
            Name _ -> "name"
            LiteralNumber _ -> "number"
            LeftBracket -> "left-bracket"
            RightBracket -> "right-bracket"
            BinaryOpToken _ -> "op"
            Comma -> "comma"
        inlay = 0.2

        opacity angle2 = (cos (angle+angle2))*0.1 + 0.2
    in
        Svg.g
            [ SA.transform <| "translate("++(ff x)++", "++(ff y)++") rotate(" ++ (ff <| radians_to_degrees angle) ++ ")"
            , SA.class <| "token "++class
            ]
            [ Svg.line
                [ SA.x1 <| ff <| -width*0.6
                , SA.y1 "0"
                , SA.x2 <| ff <| width*0.6
                , SA.y2 "0"
                , SA.strokeWidth "4"
                , SA.stroke "black"
                ]
                []
            , Svg.rect  -- Body
                [ SA.width (ff width)
                , SA.class "body"
                , SA.height (ff height)
                , SA.x (ff <| -width/2)
                , SA.y (ff <| -height/2)
                , SA.strokeWidth (ff <| width/10)
                , SA.strokeLinejoin "round"
                ]
                []
            , Svg.path
                [ SA.d <| strf "M % % l % % l % % l % %" (List.map ff [-width/2, -height/2, width, 0, -width*inlay, height*inlay, -width*(1-2*inlay), 0, -width*inlay, -height*inlay])
                , SA.fill "black"
                , SA.fillOpacity <| ff <| opacity 0
                ]
                []
            , Svg.path
                [ SA.d <| strf "M % % l % % l % % l % %" (List.map ff [width/2, -height/2, -width*inlay, height*inlay, 0, height*(1-2*inlay), width*inlay, height*inlay])
                , SA.fill "black"
                , SA.fillOpacity <| ff <| opacity (pi/2)
                ]
                []
            , Svg.path
                [ SA.d <| strf "M % % l % % l % % l % %" (List.map ff [-width/2, -height/2, width*inlay, height*inlay, 0, height*(1-2*inlay), -width*inlay, height*inlay])
                , SA.fill "black"
                , SA.fillOpacity <| ff <| opacity (-pi/2)
                ]
                []
            , Svg.path
                [ SA.d <| strf "M % % l % % l % % l % %" (List.map ff [-width/2, height/2, width, 0, -width*inlay, -height*inlay, -width*(1-2*inlay), 0, -width*inlay, height*inlay])
                , SA.fill "black"
                , SA.fillOpacity <| ff <| opacity (pi)
                ]
                []
            , Svg.text_
                [ SA.dominantBaseline "middle"
                , SA.textAnchor "middle"
                , SA.fill "black"
                , SA.fontWeight "bold"
                , SA.fontSize <| ff <| height*2/3
                , SA.fontFamily "monospace"
                , SA.transform <| "rotate("++(ff <| radians_to_degrees -angle)++")"
                ]
                [ Svg.text <| Token.toString carriage
                ]
            
            ]
        

view_railway : Model -> Html Msg
view_railway model =
    let
        padding = 50
        lead_in = 400
        canvas_width = 2*padding + 3*track_length + long_junction_width + 2*lead_in
        canvas_height = 2*padding + uturn_height + 2*long_junction_height
        rail_segments = 
            [ ((0, 0), [ IncrediblyLongHorizontal ])
            , ((0, 0), ([ AnticlockwiseTurn ]++short_junction_down++[ ShortHorizontal, ShortHorizontal, ClockwiseDown, Vertical]))
            , ((-incredibly_long_track_length, uturn_height + short_junction_height), [ IncrediblyLongHorizontal, MidHorizontal ])
            , ((-incredibly_long_track_length, uturn_height + short_junction_height + long_junction_height), ([ IncrediblyLongHorizontal, TinyHorizontal] ++ long_junction_up))
            ]

        label_offset = track_gauge * 2.2
    in
        Svg.svg
            [ SA.viewBox <| (ff <| -padding - lead_in)++" "++(ff -padding)++" "++(ff canvas_width)++" "++(ff canvas_height)
            , SA.id "railway"
            ]
            (
                [ Svg.g
                    []
                    (List.map view_rail_under rail_segments)
                , Svg.g
                    []
                    (List.map view_rails rail_segments)
                , Svg.text_
                    [ SA.x <| ff 0
                    , SA.y <| ff <| label_offset
                    ]
                    [ Svg.text "Input"
                    ]
                , Svg.text_
                    [ SA.x <| ff 0
                    , SA.y <| ff <| uturn_height + short_junction_height + label_offset
                    , SA.textAnchor "end"
                    ]
                    [ Svg.text "Stack"
                    ]
                , Svg.text_
                    [ SA.x <| ff 0
                    , SA.y <| ff <| uturn_height + short_junction_height + long_junction_height + label_offset
                    , SA.textAnchor "end"
                    ]
                    [ Svg.text "Output"
                    ]
                , view_train model.time model.train
                ]
                ++(view_queues model)
            )

footer =
    H.footer
        []
        [ H.p []
            [ H.text "Made by "
            , H.a
                [ HA.href "https://somethingorotherwhatever.com" ]
                [ H.text "clp" ]
            ]
        ]

debug_shunt : Shunt -> String
debug_shunt s = 
    (track_name s.from)++"→"++(track_name s.to)++(if s.carry then "!" else "")

debug_yard : ShuntingYard -> String
debug_yard yard = 
        "input:  " ++ (List.map Token.toString yard.input |> String.join " ") ++ "\n" 
    ++  "stack:  " ++ (List.map Token.toString yard.stack |> String.join " ") ++ "\n"
    ++  "output: " ++ (List.map Token.toString yard.output |> String.join " ")

debug_shunt_result : Result ShuntingYardError ShuntingYard -> String
debug_shunt_result r = case r of
    Ok yard -> String.join "\n" <| List.map debug_shunt <| first <| plan_route yard
    Err err -> shunting_error_description err

expr_input : Model -> Html Msg
expr_input model =
    H.label
        []
        [ H.text "Expression"
        , H.input
            [ HE.onInput SetExpr
            , HA.value model.expr
            , HA.id "expression"
            ]
            []
        ]

