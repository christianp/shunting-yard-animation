module Token exposing (Token(..), Expression(..), BinaryOp, toString, tokenise, ops)

import Parser as P exposing (Parser, (|.), (|=))
import Set

{-
Types and functions for parsing and manipulating strings of tokens.
-}

type alias BinaryOp =
    { symbol : String
    , precedence : Int
    , leftAssociative: Bool
    , display : String
    }

type Token
    = Name String
    | LiteralNumber Int
    | BinaryOpToken BinaryOp
    | LeftBracket
    | RightBracket
    | Comma

type Expression
    = Atom Token
    | BinaryOpApplication Expression Expression
    | FunctionApplication (List Expression)


toString : Token -> String
toString t = case t of
    Name name -> name
    LiteralNumber n -> String.fromInt n
    BinaryOpToken op -> op.display
    LeftBracket -> "("
    RightBracket -> ")"
    Comma -> ", "

parse_op : Parser Token
parse_op = P.oneOf <| List.map (\t -> P.succeed (BinaryOpToken t) |. P.symbol t.symbol) ops

parse_token : Parser Token
parse_token = P.oneOf
    [ P.succeed LeftBracket |. P.symbol "("
    , P.succeed RightBracket |. P.symbol ")"
    , P.succeed Comma |. P.symbol ", "
    , P.succeed Name |= P.variable { start = Char.isAlpha, inner = Char.isAlpha, reserved = Set.empty }
    , P.succeed LiteralNumber |= P.int
    , parse_op
    ]

tokeniser : Parser (List Token)
tokeniser = P.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = P.spaces
    , item = parse_token
    , trailing = P.Optional
    } |. P.end

tokenise : String -> Result (List P.DeadEnd) (List Token)
tokenise = P.run tokeniser

ops : List BinaryOp
ops = 
    [ {display = "×", symbol = "*", precedence = 1, leftAssociative = True}
    , {display = "÷", symbol = "/", precedence = 1, leftAssociative = True}
    , {display = "^", symbol = "^", precedence = 0, leftAssociative = False}
    , {display = "+", symbol = "+", precedence = 2, leftAssociative = True}
    , {display = "-", symbol = "-", precedence = 2, leftAssociative = True}
    ]

