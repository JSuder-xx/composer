module Composer.Example.FieldStatus exposing (FieldStatus(..), codec, comparison, max)

import Codec exposing (Codec)


type FieldStatus
    = NotVisited
    | Focused
    | Changed
    | Blurred


all : List FieldStatus
all =
    [ NotVisited, Focused, Changed, Blurred ]


max : FieldStatus -> FieldStatus -> FieldStatus
max x y =
    if comparison x y == GT then
        x

    else
        y


toInt : FieldStatus -> Int
toInt x =
    case x of
        NotVisited ->
            0

        Focused ->
            1

        Changed ->
            2

        Blurred ->
            3


codec : Codec FieldStatus
codec =
    Codec.enum Codec.int <| List.map (\x -> ( toInt x, x )) all


comparison : FieldStatus -> FieldStatus -> Order
comparison x y =
    compare (toInt x) (toInt y)
