module ShuntingYard exposing
    ( ShuntingYard
    , Shunt
    , ShuntingYardError(..)
    , ShuntReason(..)
    , shunt_reason_description
    , shunting_yard
    , shunt_moves
    , shunting_error_description
    , track_name
    )

import Railway exposing (Track(..), TrackAction(..))
import Token exposing (Token)

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

