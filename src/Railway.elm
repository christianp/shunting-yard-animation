module Railway exposing 
    (Coords
    , RailwayPiece(..)
    , Direction(..)
    , TrainAction(..)
    , Route
    , Train
    , Track(..)
    , TrackAction(..)
    , ShuntingYard
    , Shunt
    , ShuntingYardError(..)
    , ShuntReason(..)
    , shunt_reason_description
    , shunting_yard
    , shunt_moves
    , shunting_error_description
    , track_name
    , track_position
    , track_direction
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

import Parameters exposing (..)
import Token exposing (Token(..), BinaryOp)
import Util exposing (strf, ff)


type alias Coords = (Float, Float)

type RailwayPiece
    = ShortHorizontal
    | MidHorizontal
    | TinyHorizontal
    | IncrediblyLongHorizontal
    | CurveDownStart
    | DiagonalDown
    | CurveDownEnd
    | CurveUpStart
    | DiagonalUp
    | CurveUpEnd
    | AnticlockwiseTurn
    | ClockwiseDown
    | Vertical

type Direction
    = Forwards
    | Backwards

type Track
    = Input
    | Stack
    | Output
    | Bin

type TrackAction
    = Stationary
    | Reversing
    | Advancing

type TrainAction
    = FollowRoute (List RailwayPiece)
    | PickUpCarriage Track
    | DropCarriage Track
    | DescribeThinking ShuntReason

type alias Route = List TrainAction

type alias Train =
    { route : Route
    , direction: Direction
    , position: Coords
    , start_time : Float
    , pulling : Maybe Token
    }

-- Make a fresh shunting yard with the given input
shunting_yard : List Token -> ShuntingYard
shunting_yard input =
    { input = input
    , output = []
    , stack = []
    }


long_junction_down : List RailwayPiece
long_junction_down =
    [ ShortHorizontal
    , CurveDownStart
    , DiagonalDown
    , CurveDownEnd
    , ShortHorizontal
    ]

short_junction_down : List RailwayPiece
short_junction_down =
    [ CurveDownStart
    , CurveDownEnd
    ]

long_junction_up : List RailwayPiece
long_junction_up =
    [ CurveUpStart
    , DiagonalUp
    , CurveUpEnd
    ]


-- Convert a description of a shunt to a sequence of moves on the railway pieces.
shunt_moves : Shunt -> Route
shunt_moves s =
    let
        route = case (s.from, s.to) of
            (Input, Output) -> 
                [ FollowRoute ([AnticlockwiseTurn] ++ short_junction_down++[ShortHorizontal, ShortHorizontal])
                , FollowRoute ([TinyHorizontal]++long_junction_up++[ShortHorizontal])
                ]
            (Input, Stack) -> 
                [ FollowRoute ([AnticlockwiseTurn] ++ short_junction_down++[ShortHorizontal])
                , FollowRoute [MidHorizontal, ShortHorizontal]
                ]
            (Input, Bin) ->
                [FollowRoute ([AnticlockwiseTurn] ++ short_junction_down++[ShortHorizontal, ShortHorizontal, ClockwiseDown, Vertical])]
            (Stack, Input) -> 
                [ FollowRoute [MidHorizontal, ShortHorizontal]
                , FollowRoute ([AnticlockwiseTurn] ++ short_junction_down++[ShortHorizontal])
                ]
            (Stack, Output) -> 
                [ FollowRoute [MidHorizontal, ShortHorizontal, ShortHorizontal]
                , FollowRoute ([TinyHorizontal]++long_junction_up++[ShortHorizontal])
                ]
            (Stack, Bin) ->
                [ FollowRoute [MidHorizontal, ShortHorizontal, ShortHorizontal, ClockwiseDown, Vertical] ]
            (Output, Input) -> 
                [ FollowRoute ([TinyHorizontal]++long_junction_up++[ShortHorizontal])
                , FollowRoute ([AnticlockwiseTurn] ++ short_junction_down++[ShortHorizontal, ShortHorizontal])
                ]
            (Output, Stack) -> 
                [ FollowRoute ([TinyHorizontal]++long_junction_up++[ShortHorizontal])
                , FollowRoute [MidHorizontal, ShortHorizontal, ShortHorizontal]
                ]
            (Output, Bin) ->
                [ FollowRoute ([TinyHorizontal]++long_junction_up++[ShortHorizontal, ClockwiseDown, Vertical]) ]

            (Bin, Input) ->
                [FollowRoute ([AnticlockwiseTurn] ++ short_junction_down++[ShortHorizontal, ShortHorizontal, ClockwiseDown, Vertical])]
            (Bin, Stack) ->
                [ FollowRoute [MidHorizontal, ShortHorizontal, ShortHorizontal, ClockwiseDown, Vertical] ]
            (Bin, Output) ->
                [ FollowRoute ([TinyHorizontal]++long_junction_up++[ShortHorizontal, ClockwiseDown, Vertical]) ]

            (Input, Input) -> []
            (Stack, Stack) -> []
            (Bin, Bin) -> []
            (Output, Output) -> []
        described_route = case s.reason of
            Just r -> [DescribeThinking r]++route
            Nothing -> route
    in
        if s.carry then 
            [PickUpCarriage s.from]++described_route++[DropCarriage s.to]
        else
            described_route

railway_piece_path : RailwayPiece -> String
railway_piece_path p = case p of
    ShortHorizontal -> "l "++(ff track_length)++" 0"
    MidHorizontal -> "l "++(ff mid_track_length)++" 0"
    TinyHorizontal -> "l "++(ff <| tiny_track_length)++" 0"
    IncrediblyLongHorizontal -> "l "++(ff <| incredibly_long_track_length)++" 0"
    CurveDownStart -> "a "++(ff track_length)++" "++(ff track_length)++" 0 0 1 "++(ff turn_width)++" "++(ff turn_height)
    DiagonalDown -> "l "++(ff turn_width)++" "++(ff turn_width)
    CurveDownEnd -> "a "++(ff track_length)++" "++(ff track_length)++" 0 0 0 "++(ff turn_width)++" "++(ff turn_height)
    CurveUpStart -> "a "++(ff track_length)++" "++(ff track_length)++" 0 0 0 "++(ff turn_width)++" "++(ff -turn_height)
    DiagonalUp -> "l "++(ff turn_width)++" "++(ff -turn_width)
    CurveUpEnd -> "a "++(ff track_length)++" "++(ff track_length)++" 0 0 1 "++(ff turn_width)++" "++(ff -turn_height)
    AnticlockwiseTurn -> "a "++(ff track_length)++" "++(ff track_length)++" 0 0 0 0 "++(ff uturn_height)
    ClockwiseDown -> "a "++(ff track_length)++" "++(ff track_length)++" 0 0 1 "++(ff track_length)++" "++(ff track_length)
    Vertical -> "l 0 "++(ff vertical_length)

railway_piece_length : RailwayPiece -> Float
railway_piece_length p = case p of
    ShortHorizontal -> track_length
    MidHorizontal -> mid_track_length
    TinyHorizontal -> tiny_track_length
    IncrediblyLongHorizontal -> incredibly_long_track_length
    CurveDownStart -> curve_length
    DiagonalDown -> diagonal_length
    CurveDownEnd -> curve_length
    CurveUpStart -> curve_length
    DiagonalUp -> diagonal_length
    CurveUpEnd -> curve_length
    AnticlockwiseTurn -> uturn_length
    ClockwiseDown -> pi * track_length / 2
    Vertical -> vertical_length

point_on_railway_piece : RailwayPiece -> Float -> Coords
point_on_railway_piece piece d =
    let
        t = clamp -1 1 (d/(railway_piece_length piece))
        (cx, cy) = (track_length * (sin (degrees (45*t))), track_length * (1 - cos (degrees (45*t))))
        (rx, ry) = (turn_width - track_length * (sin (degrees (45*(1-t)))), turn_height - track_length * (1 - cos (degrees (45*(1-t)))))
        (ux, uy) = (track_length * (sin (pi*t)), track_length * (1-(cos (pi*t))))
        (qx, qy) = (track_length * (sin (pi*t/2)), track_length * (1-(cos (pi*t/2))))
    in
        case piece of
            ShortHorizontal -> (t*track_length, 0)
            MidHorizontal -> (t*mid_track_length, 0)
            TinyHorizontal -> (t*tiny_track_length, 0)
            IncrediblyLongHorizontal -> (t*incredibly_long_track_length, 0)
            CurveDownStart -> (cx, cy)
            DiagonalDown -> (t*turn_width, t*turn_width)
            CurveDownEnd -> (rx, ry)
            CurveUpStart -> (cx, -cy)
            DiagonalUp -> (t*turn_width, -t*turn_width)
            CurveUpEnd -> (rx, -ry)
            AnticlockwiseTurn -> (-ux,uy)
            ClockwiseDown -> (qx, qy)
            Vertical -> (0, t * vertical_length) 


point_on_railway : Coords -> List RailwayPiece -> Float -> Coords
point_on_railway (sx, sy) pieces sd =
    let
        handle_piece : RailwayPiece -> (Float, Float, Float) -> (Float, Float, Float)
        handle_piece p (x, y, d) = 
            if d>0 then
                let
                    l = railway_piece_length p
                    (dx, dy) = point_on_railway_piece p d
                in
                    (x+dx, y+dy, d-l)
            else
                (x, y, d)
    in
        List.foldl handle_piece (sx, sy, sd) pieces |> \(x, y, d) -> (x, y)

angle_on_railway_piece : RailwayPiece -> Float -> Float
angle_on_railway_piece piece d = 
    let
        t = clamp 0 1 (d/(railway_piece_length piece))
    in
        case piece of
            CurveDownStart -> t*(degrees 45)
            CurveDownEnd -> (1-t)*(degrees 45)
            DiagonalDown -> degrees 45
            CurveUpStart -> -t*(degrees 45)
            CurveUpEnd -> -(1-t)*(degrees 45)
            DiagonalUp -> degrees -45
            AnticlockwiseTurn -> pi*(1-t)
            ShortHorizontal -> 0
            MidHorizontal -> 0
            TinyHorizontal -> 0
            IncrediblyLongHorizontal -> 0
            ClockwiseDown -> t*pi/2
            Vertical -> pi/2


angle_on_railway : List RailwayPiece -> Float -> Float
angle_on_railway pieces sd =
    let
        initial_angle = case List.head pieces of
            Just AnticlockwiseTurn -> pi
            _ -> 0
        handle_piece : RailwayPiece -> Coords -> Coords
        handle_piece p (angle, d) = 
            if d>0 then
                let
                    l = railway_piece_length p
                    a2 = angle_on_railway_piece p d
                in
                    (a2, d-l)
            else
                (angle, d)
    in
        if pieces == [] then 
            pi 
        else
            List.foldl handle_piece (initial_angle, sd) pieces |> \(a, d) -> a

railway_length : List RailwayPiece -> Float
railway_length = List.sum << (List.map railway_piece_length)

track_name : Track -> String
track_name track = case track of
    Input -> "Input"
    Stack -> "Stack"
    Output -> "Output"
    Bin -> "Bin"

track_position track = case track of
    Input -> (0,0)
    Stack -> (0, short_junction_height + uturn_height)
    Output -> (0, long_junction_height + short_junction_height + uturn_height)
    Bin -> (1000, long_junction_height)

track_direction track = case track of
    Input -> 1
    Output -> -1
    Stack -> -1
    Bin -> 0


train_distance : Float -> Train -> Float
train_distance time train =
    case train.route of
        (FollowRoute pieces)::rest ->
            let
                l = railway_length pieces
                dt = max 0 (time - train.start_time)
                total_time = 2*accelerate_time + (l-2*accelerate_distance)/max_speed
                d = 
                    if dt<accelerate_time then
                        0.5 * acceleration * (dt ^ 2)
                    else if dt < total_time - accelerate_time then
                        accelerate_distance + (dt - accelerate_time)*max_speed
                    else if dt >= total_time then
                        l
                    else
                        let
                            t = (dt - (total_time - accelerate_time))
                        in
                            l - accelerate_distance + max_speed * t - 0.5 * acceleration * (t ^ 2)
            in
                case train.direction of
                    Forwards -> d
                    Backwards -> l - d

        _ -> 0

train_position : Float -> Train -> Coords
train_position d train =
    case List.head train.route of
        Just (FollowRoute pieces) ->
            let
                (ox, oy) = point_on_railway (0, 0) pieces (railway_length pieces)
                (x, y) = point_on_railway train.position pieces d
                (dx, dy) = case train.direction of
                    Forwards -> (0, 0)
                    Backwards -> (-ox, -oy)
            in
                (x+dx, y+dy)
                
        _ -> train.position

action_pieces : TrainAction -> List RailwayPiece
action_pieces action = case action of
    FollowRoute pieces -> pieces
    _ -> []

type alias ShuntingYard =
    { input : List Token
    , stack : List Token
    , output : List Token
    }

type alias Shunt =
    { from : Track
    , to: Track
    , carry: Bool
    , reason : Maybe ShuntReason
    }

type ShuntingYardError
    = EmptyTrack Track
    | EndOfInput
    | UnhandledError String
    | NoMatchingLeftBracket
    | NoMatchingRightBracket

type ShuntReason
    = PopStackToOutput
    | DiscardBrackets
    | PopOpsUntilBracket
    | InputToOutput
    | OpToStack
    | LeftBracketToStack
    | RemainingOps
    | ReadInput

shunt_reason_description reason = case reason of
    PopStackToOutput -> "Move a higher-precedence operator from the stack to the output"
    DiscardBrackets -> "Discard the brackets"
    PopOpsUntilBracket -> "Move operators from the stack to the output until a left bracket is reached"
    InputToOutput -> "Push to the output"
    OpToStack -> "Push the operator to the stack"
    LeftBracketToStack -> "Push the left bracket to the stack"
    RemainingOps -> "Move remaining operators on the stack to the output"
    ReadInput -> "Read a token from the input"

shunting_error_description err = case err of
    EmptyTrack track -> "The track "++(track_name track)++" is empty"
    EndOfInput -> "Reached the end of the input"
    UnhandledError s -> "Unhandled error: " ++ s
    NoMatchingLeftBracket -> "Saw a right bracket without a matching left bracket"
    NoMatchingRightBracket -> "Saw a left bracket without a matching right bracket"

push_output t yard = { yard | output = t::yard.output }

pop_output : ShuntingYard -> Result ShuntingYardError (Token, ShuntingYard)
pop_output yard = case yard.output of
    t::rest -> Ok (t, { yard | output = rest })
    _ -> Err (EmptyTrack Output)


append_input : Token -> ShuntingYard -> ShuntingYard
append_input t yard = { yard | input = yard.input++[t] }

unshift_input : ShuntingYard -> ShuntingYard
unshift_input yard = { yard | input = List.take ((List.length yard.input)-1) yard.input }

push_stack t yard = { yard | stack = t::yard.stack }

pop_stack : ShuntingYard -> Result ShuntingYardError (Token, ShuntingYard)
pop_stack yard = case yard.stack of
    t::rest -> Ok (t, { yard | stack = rest })
    _ -> Err (EmptyTrack Stack)

pop_input : ShuntingYard -> Result ShuntingYardError (Token, ShuntingYard)
pop_input yard = case yard.input of
    t::rest -> Ok (t, { yard | input = rest })
    _ -> Err EndOfInput

shunt from to carry reason =
    { from = from
    , to = to
    , carry = carry
    , reason = reason
    }

error_step reason = ([shunt Input Input False Nothing], Err reason)

pop_operators : BinaryOp -> ShuntingYard -> (List Shunt, ShuntingYard)
pop_operators op1 yard1 =
    case pop_stack yard1 of
        Ok (t, yard2) -> 
            case t of
                BinaryOpToken op2 ->
                    if op2.precedence < op1.precedence || (op2.precedence == op1.precedence && op1.leftAssociative) then
                        Tuple.mapFirst (between Nothing [shunt Stack Output True (Just PopStackToOutput)]) (push_output t yard2 |> pop_operators op1)
                    else
                        ([], yard1)
                _ -> ([], yard1)

        Err err -> ([], yard1)


close_bracket : ShuntingYard -> (List Shunt, Result ShuntingYardError ShuntingYard)
close_bracket yard1 =
    case pop_stack yard1 of
        Ok (t, yard2) ->
            case t of
                LeftBracket -> ([shunt Stack Bin True (Just DiscardBrackets), shunt Bin Input False Nothing, shunt Input Bin True Nothing], Ok yard2)

                _ -> 
                    let
                        (shunts, ryard3) = close_bracket yard2
                    in
                        case ryard3 of
                            Ok yard3 ->  
                                (between Nothing [shunt Stack Output True (Just PopOpsUntilBracket)] shunts, Ok <| push_output t yard3)
                            Err _ -> (shunts, ryard3)

        Err err -> error_step NoMatchingLeftBracket
        
shunting_yard_step : ShuntingYard -> (List Shunt, Result ShuntingYardError ShuntingYard)
shunting_yard_step yard =
    case (yard.input, yard.stack) of
        (a::rest, _) -> 
            case pop_input yard of
                Ok (t, yard2) ->
                    case t of
                        Name _ -> ([shunt Input Output True (Just InputToOutput)], Ok <| push_output t yard2)

                        LiteralNumber _ -> ([shunt Input Output True (Just InputToOutput)], Ok <| push_output t yard2)

                        BinaryOpToken op -> 
                            let
                                (pops, yard3) = pop_operators op yard2
                            in
                                (  pops
                                ++ (if pops==[] then [] else [shunt Output Input False Nothing])
                                ++ [shunt Input Stack True (Just OpToStack)]
                                , Ok <| push_stack t yard3
                                )

                        LeftBracket ->
                            ([shunt Input Stack True (Just LeftBracketToStack)], Ok <| push_stack t yard2)

                        RightBracket ->
                            let
                                (pops, yard3) = close_bracket yard2
                            in
                                (pops, yard3)

                        _ -> ([], Err (UnhandledError "don't know about this token"))

                Err err -> ([], Err err)

        ([], a::rest) -> case a of
            LeftBracket -> error_step NoMatchingRightBracket
            _ -> ([shunt Stack Output True (Just RemainingOps)], Ok { yard | output = a::yard.output, stack = rest })

        _ -> error_step EndOfInput


between : Maybe ShuntReason -> List Shunt -> List Shunt -> List Shunt
between reason s1 s2 = 
    let
        m = case (List.head <| List.reverse s1, List.head s2) of
            (Just a, Just b) -> if a.to /= b.from then [shunt a.to b.from False reason] else []
            _ -> []
    in
        s1 ++ m ++ s2

plan_route : ShuntingYard -> (List Shunt, Result ShuntingYardError ShuntingYard)
plan_route yard =
    let
        (shunts, result) = shunting_yard_step yard
    in
        case result of
            Ok yard2 -> 
                let
                    (nshunts, result2) = plan_route yard2
                in
                    (between (Just ReadInput) shunts nshunts, result2)
            Err err -> (shunts, result)
