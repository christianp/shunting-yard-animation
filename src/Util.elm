module Util exposing (..)

import Tuple exposing (first, second)

{-
Generic utility functions.
-}

-- Substitute values into a string: replace each % character in `template` with the corresponding entry in the list `bits`.
strf : String -> List String -> String
strf template bits =
    let
        next_bit cbits = case cbits of
            a::rest -> (a,rest)
            [] -> ("",[])
    in
        first <| List.foldl (\chr -> \(out,cbits) -> 
            if chr=='%' then
                let
                    (suffix,nbits) = next_bit cbits
                in
                    (out++suffix, nbits)
            else
                (out++(String.fromChar chr), cbits)
        ) ("",bits) (String.toList template)


-- A short-hand to convert a float to a string, because it's used so often.
ff = String.fromFloat

-- Convert from radians to degrees.
-- Not sure why this is missing from the Basic package!
radians_to_degrees : Float -> Float
radians_to_degrees r = r*180/pi

