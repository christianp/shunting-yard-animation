module Token exposing (Token(..), BinaryOp, toString, tokenise, ops)

import Parser as P exposing (Parser, (|.), (|=))
import Set

{-
Types and functions for parsing and manipulating strings of tokens.

The idea is to parse a string containing a mathematical expression like ``1 + 2^(3-4)`` into a string of tokens representing each of the pieces of the expression, so that they can be manipulated.
-}

{-
A representation of a binary operator.

The precedence determines the order of operations.
A lower precedence number means that the operator should be evaluated first.
-}
type alias BinaryOp =
    { symbol : String
    , precedence : Int
    , leftAssociative: Bool
    , display : String
    }

{-
Kinds of token.

The ``Comma`` token is in there because it'd be necessary for n-ary function calls, but I didn't implement that.
-}
type Token
    = Name String
    | LiteralNumber Int
    | BinaryOpToken BinaryOp
    | LeftBracket
    | RightBracket
    | Comma

{-
Represent a token as a string. (the opposite of parsing)
-}
toString : Token -> String
toString t = case t of
    Name name -> name
    LiteralNumber n -> String.fromInt n
    BinaryOpToken op -> op.display
    LeftBracket -> "("
    RightBracket -> ")"
    Comma -> ", "

{-
Parse an operator: for each of the defined operators, if its symbol is next, then return a token representing that operation.
-}
parse_op : Parser Token
parse_op = P.oneOf <| List.map (\t -> P.succeed (BinaryOpToken t) |. P.symbol t.symbol) ops

{-
Parse a single token.
-}
parse_token : Parser Token
parse_token = P.oneOf
    [ P.succeed LeftBracket |. P.symbol "("
    , P.succeed RightBracket |. P.symbol ")"
    , P.succeed Comma |. P.symbol ", "
    , P.succeed Name |= P.variable { start = Char.isAlpha, inner = Char.isAlpha, reserved = Set.empty }
    , P.succeed LiteralNumber |= P.int
    , parse_op
    ]

{-
A parser which produces a list of tokens.
-}
tokeniser : Parser (List Token)
tokeniser = P.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = P.spaces
    , item = parse_token
    , trailing = P.Optional
    } |. P.end

{-
Execute the parser on a given string.

If it can't be parsed, the returned error is a list of points at which the parser got stuck.
-}
tokenise : String -> Result (List P.DeadEnd) (List Token)
tokenise = P.run tokeniser

{-
The defined operations.
-}
ops : List BinaryOp
ops = 
    [ {display = "×", symbol = "*", precedence = 1, leftAssociative = True}
    , {display = "÷", symbol = "/", precedence = 1, leftAssociative = True}
    , {display = "^", symbol = "^", precedence = 0, leftAssociative = False}
    , {display = "+", symbol = "+", precedence = 2, leftAssociative = True}
    , {display = "-", symbol = "-", precedence = 2, leftAssociative = True}
    ]

