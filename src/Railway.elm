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

{-
Railway and shunting yard
#########################

The shunting yard algorithm takes place on three queues:

* An input, consisting of a sequence of tokens.
* A stack, for holding stuff.
* An output, representing the result of the algorithm.

The aim is to move tokens one by one from the input, and rearrange them on the output.

Stuff moves from the input either directly to the output, or held in the stack before going to the output.

Some tokens are discarded, either immediately or after being held in the stack.

So to draw this, we need to show a railway with three arbitrarily long sections representing each of the queues, a run-off for discarding tokens, and ways of getting between them.

There will be a train which moves between each of the points.
It should back onto each of the queues, to pick up or drop off tokens, represented by carriages.

I'd like to build the track out of a small number of different-shaped pieces, like Brio.

We need an implementation of the shunting yard algorithm that produces a description of each of the steps taken, along with a description of the reasoning.

The descriptions of the steps will be used to determine where the train goes, and the reasoning will be shown alongside.

The animation has as its state:
* a global time
* the planned route of the train, and the time it started moving on the segment of track it's currently on
* the contents of each of the queues.

The state of the shunting yard is stored as a ``Result``, representing either the contents of the queues or a description of an error.
-}

-- A type alias to conveniently refer to a pair of coordinates
type alias Coords = (Float, Float)

-- The different kinds of railway pieces
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

-- Which way is the train driving?
type Direction
    = Forwards
    | Backwards

-- Each of the four bits of the track the train can drive to
type Track
    = Input
    | Stack
    | Output
    | Bin


{-
It'd be nice to smoothly move the carriages on each track, either forwards when a carriage is pulled away, or backwards when a carriage is dropped off.

When the train picks up a carriage, it'll set the track it's at to Advancing, and the track it's going to to Reversing.

The carriages on those tracks will be drawn in the right position based on the distance of the train from the front of the queue.
-}
type TrackAction
    = Stationary
    | Reversing
    | Advancing

{-
Actions for the train: each shunt is translated to a list of these actions.

``FollowRoute`` actions alternate between interpreted as moving forwards and backwards.
-}
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

{-
Some pre-made sequences of track pieces, which are each only used at most once. 🤷
-}

-- A sequence of pieces moving down one track, with a diagonal bit in the middle to make it longer.
long_junction_down : List RailwayPiece
long_junction_down =
    [ ShortHorizontal
    , CurveDownStart
    , DiagonalDown
    , CurveDownEnd
    , ShortHorizontal
    ]

-- A shorter sequence of pieces moving down one track.
short_junction_down : List RailwayPiece
short_junction_down =
    [ CurveDownStart
    , CurveDownEnd
    ]

-- A sequence of pieces moving up one track, with a diagonal bit in the middle to make it longer.
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

{-
A portion of an SVG path representing a railway piece.

Represented as relative moves, so they can be strung together without knowing any context.
-}
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

{-
The length of a piece of railway, to calculate how far the train has to go.
-}
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

-- Add up the lengths of a list of pieces to get the total length.
railway_length : List RailwayPiece -> Float
railway_length = List.sum << (List.map railway_piece_length)

{-
Get the coordinates of a point on a railway piece, given the distance along the track from the start point.

The distance is clamped between 0 and the piece's length, so it'll always return a point on the piece.
This means you can pass in a big negative distance and it'[ll just stay at the start point.

The pieces are all either straight or a circular segment, so they're easy to interpolate.
-}
point_on_railway_piece : RailwayPiece -> Float -> Coords
point_on_railway_piece piece d =
    let
        t = clamp 0 1 (d/(railway_piece_length piece))
        -- this list of coordinates for each of the kinds of turn is too long; it could be replaced with a single function.
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

{-
Given a list of connected railway pieces and a distance along it from the start of the first piece, get the coordinates of the corresponding point.

Working through each piece, count down ``d`` until it's within the bounds of the piece you're looking at.
Then stick with the coordinates on that piece.
-}
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

{-
Do the same thing, but returning the angle of the tangent to the track at that point.
-}
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

{-
Every piece other than the anti-clockwise turn goes left-to-right, but the input queue starts with that piece, so an empty list of pieces corresponds to the train waiting at the start of the input.
So return an angle of pi for that; otherwise use ``angle_on_railway_piece``.
-}
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

track_name : Track -> String
track_name track = case track of
    Input -> "Input"
    Stack -> "Stack"
    Output -> "Output"
    Bin -> "Bin"

-- The positions of the fronts of each of the queues.
track_position track = case track of
    Input -> (0,0)
    Stack -> (0, short_junction_height + uturn_height)
    Output -> (0, long_junction_height + short_junction_height + uturn_height)
    Bin -> (1000, long_junction_height)

-- This determines the x-direction of subsequent carriages on each track.
track_direction track = case track of
    Input -> 1
    Output -> -1
    Stack -> -1
    Bin -> 0

{-
How far along its current segment of track should the train be at the given time?

The train starts from standing, accelerates for a bit, then moves at max speed until it decelerates at the same rate to stop at the end of the segment.

The acceleration is calculated so that the train gets to full speed in the length of a single ``ShortHorizontal`` piece. a
The equations of motion are used to work out the acceleration that satisfies this, and the time taken::

    distance = (initial velocity = 0)*(time) + 1/2 * acceleration * (time^2)

    final velocity = (initial velocity = 0) + acceleration * time

    so

    acceleration = final velocity / time

    distance = 1/2 * (final velocity / time) * (time^2)
             = 1/2 * (final velocity) * time

    so

    time = 2 * distance * (final velocity)

If the train is moving backwards, it start at 0 and advances to the full distance.

If the train is moving backwards, it starts at full distance and moves backwards to 0.
-}
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

{-
The current coordinates of the train.

The only wrinkle is to account for whether the train is moving forwards or backwards.
-}
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

-- Get the segment of railway that the train is currently on, to work out its coordinates and angle.
action_pieces : TrainAction -> List RailwayPiece
action_pieces action = case action of
    FollowRoute pieces -> pieces
    _ -> []



{-
Shunting yard model
~~~~~~~~~~~~~~~~~~~
-}


{-
The state of the shunting yard algorithm is just the contents of the three queues.

The train is decoration on top of that.
-}
type alias ShuntingYard =
    { input : List Token
    , stack : List Token
    , output : List Token
    }

{-
A description of a step in the algorithm: the train moves between two tracks, may or may not pull a carriage with it, and may or may not describe its reasoning.

If no reason is given, then the previous reason continues to be shown.
-}
type alias Shunt =
    { from : Track
    , to: Track
    , carry: Bool
    , reason : Maybe ShuntReason
    }

{-
A type representing each kind of error that can happen during the course of the algorithm.
-}
type ShuntingYardError
    = EmptyTrack Track
    | EndOfInput
    | UnhandledError String
    | NoMatchingLeftBracket
    | NoMatchingRightBracket

shunting_error_description err = case err of
    EmptyTrack track -> "The track "++(track_name track)++" is empty"
    EndOfInput -> "Reached the end of the input"
    UnhandledError s -> "Unhandled error: " ++ s
    NoMatchingLeftBracket -> "Saw a right bracket without a matching left bracket"
    NoMatchingRightBracket -> "Saw a left bracket without a matching right bracket"

{-
Reasons for each move made by the algorithm.

These correspond roughly one-to-one with lines in the written algorithm.
-}
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


{-
Functions for manipulating the queues.

Each queue only uses some of the possible operations on it.

If Elm had a generic update-record syntax like it has for accessing records, a lot of duplication would be avoided.
-}

pop_input : ShuntingYard -> Result ShuntingYardError (Token, ShuntingYard)
pop_input yard = case yard.input of
    t::rest -> Ok (t, { yard | input = rest })
    _ -> Err EndOfInput

append_input : Token -> ShuntingYard -> ShuntingYard
append_input t yard = { yard | input = yard.input++[t] }

unshift_input : ShuntingYard -> ShuntingYard
unshift_input yard = { yard | input = List.take ((List.length yard.input)-1) yard.input }

push_output : Token -> ShuntingYard -> ShuntingYard
push_output t yard = { yard | output = t::yard.output }

pop_output : ShuntingYard -> Result ShuntingYardError (Token, ShuntingYard)
pop_output yard = case yard.output of
    t::rest -> Ok (t, { yard | output = rest })
    _ -> Err (EmptyTrack Output)

push_stack : Token -> ShuntingYard -> ShuntingYard
push_stack t yard = { yard | stack = t::yard.stack }

pop_stack : ShuntingYard -> Result ShuntingYardError (Token, ShuntingYard)
pop_stack yard = case yard.stack of
    t::rest -> Ok (t, { yard | stack = rest })
    _ -> Err (EmptyTrack Stack)

{-
Each step produced by the algorithm just says where something should move from and to.

In order for the train to do that, it will need to move to the 'from' track first.

Rather than carrying along the current position of the train into each bit of the algorithm, the ``between`` function takes two sequences of steps, and adds a step in the middle to move the train from the final stop of the first sequence and to the first stop of the second sequence.
-}
between : Maybe ShuntReason -> List Shunt -> List Shunt -> List Shunt
between reason s1 s2 = 
    let
        m = case (List.head <| List.reverse s1, List.head s2) of
            (Just a, Just b) -> if a.to /= b.from then [Shunt a.to b.from False reason] else []
            _ -> []
    in
        s1 ++ m ++ s2

-- Construct a step corresponding to the algorithm entering an error state: move the train back to the input, and describe the error.
error_step reason = ([Shunt Input Input False Nothing], Err reason)

-- The step before pushing an operator to the stack: pop any lower-precedence (or equal-precedence if the pushed op is left-associative) ops off the stack and on to the output.
pop_operators : BinaryOp -> ShuntingYard -> (List Shunt, ShuntingYard)
pop_operators op1 yard1 =
    case pop_stack yard1 of
        Ok (t, yard2) -> 
            case t of
                BinaryOpToken op2 ->
                    if op2.precedence < op1.precedence || (op2.precedence == op1.precedence && op1.leftAssociative) then
                        Tuple.mapFirst (between Nothing [Shunt Stack Output True (Just PopStackToOutput)]) (push_output t yard2 |> pop_operators op1)
                    else
                        ([], yard1)
                _ -> ([], yard1)

        Err err -> ([], yard1)

{-
The steps carried out when a right bracket is read: pop any operators off the stack and into the output until a left bracket is reached, then discard the left and right brackets.

An error can happen here, if there's no left bracket on the stack.
-}
close_bracket : ShuntingYard -> (List Shunt, Result ShuntingYardError ShuntingYard)
close_bracket yard1 =
    case pop_stack yard1 of
        Ok (t, yard2) ->
            case t of
                LeftBracket -> ([Shunt Stack Bin True (Just DiscardBrackets), Shunt Bin Input False Nothing, Shunt Input Bin True Nothing], Ok yard2)

                _ -> 
                    let
                        (shunts, ryard3) = close_bracket yard2
                    in
                        case ryard3 of
                            Ok yard3 ->  
                                (between Nothing [Shunt Stack Output True (Just PopOpsUntilBracket)] shunts, Ok <| push_output t yard3)
                            Err _ -> (shunts, ryard3)

        Err err -> error_step NoMatchingLeftBracket
        
{-
Given the current state of the shunting yard, work out what the next step should be.

This depends on the next token in the input.

If the input is empty, the only move is to pop an operator from the stack and push it to the output.
If there's a left bracket still on the stack, then it must be missing a right bracket and there's an error.
-}
shunting_yard_step : ShuntingYard -> (List Shunt, Result ShuntingYardError ShuntingYard)
shunting_yard_step yard =
    case (yard.input, yard.stack) of
        (a::rest, _) -> 
            case pop_input yard of
                Ok (t, yard2) ->
                    case t of
                        Name _ -> ([Shunt Input Output True (Just InputToOutput)], Ok <| push_output t yard2)

                        LiteralNumber _ -> ([Shunt Input Output True (Just InputToOutput)], Ok <| push_output t yard2)

                        BinaryOpToken op -> 
                            let
                                (pops, yard3) = pop_operators op yard2
                            in
                                (  pops
                                ++ (if pops==[] then [] else [Shunt Output Input False Nothing])
                                ++ [Shunt Input Stack True (Just OpToStack)]
                                , Ok <| push_stack t yard3
                                )

                        LeftBracket ->
                            ([Shunt Input Stack True (Just LeftBracketToStack)], Ok <| push_stack t yard2)

                        RightBracket -> close_bracket yard2

                        _ -> ([], Err (UnhandledError "don't know about this token"))

                Err err -> ([], Err err)

        ([], a::rest) -> case a of
            LeftBracket -> error_step NoMatchingRightBracket
            _ -> ([Shunt Stack Output True (Just RemainingOps)], Ok { yard | output = a::yard.output, stack = rest })

        _ -> error_step EndOfInput


{-
Plan out the entire execution of the algorithm, from the current state of the shunting yard.

This computes each step, and puts a move in between each pair of adjacent steps to make sure the train is in the right place.
-}
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
