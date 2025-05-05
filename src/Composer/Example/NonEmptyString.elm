module Composer.Example.NonEmptyString exposing (NonEmptyString, fromString, toString)


type NonEmptyString
    = NonEmptyString String


toString : NonEmptyString -> String
toString (NonEmptyString s) =
    s


fromString : String -> Maybe NonEmptyString
fromString s =
    if s == "" then
        Nothing

    else
        Just <| NonEmptyString s
