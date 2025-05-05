module Composer.Example.Field exposing
    ( Input, int, float, parseToNewType, nonEmptyString, requiredInput, string
    , FieldDefinition, ViewConfig, ViewContext, checkbox, input
    )

{-| This module will NOT be part of the core `Composer` library.


## Input Building Blocks

@docs Input, int, float, parseToNewType, nonEmptyString, requiredInput, string


## FieldDefinition Producers

@docs FieldDefinition, ViewConfig, ViewContext, checkbox, input

-}

import Codec
import Composer
import Composer.Example.FieldStatus as FieldStatus exposing (FieldStatus)
import Composer.Example.NonEmptyString as NonEmptyString exposing (NonEmptyString)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Maybe.Extra
import Result.Extra


type alias FieldState =
    { status : FieldStatus
    , value : String
    }


initFieldState : String -> FieldState
initFieldState value_ =
    { value = value_
    , status = FieldStatus.NotVisited
    }


fieldStateCodec : Codec.Codec FieldState
fieldStateCodec =
    Codec.tuple FieldStatus.codec Codec.string
        |> Codec.map (\( status_, value_ ) -> { status = status_, value = value_ }) (\r -> ( r.status, r.value ))


updateStatus : (FieldStatus -> FieldStatus) -> FieldState -> FieldState
updateStatus f r =
    { status = f r.status
    , value = r.value
    }


type Input a
    = Input
        { parse : String -> Result String a
        , initialize : Maybe a -> String
        , label : String
        , type_ : String
        , additionalAttributes : List ( String, Codec.Value )
        }


{-| Change the data type of an input.
-}
parseToNewType : List ( String, Codec.Value ) -> (new -> original) -> (original -> Result String new) -> Input original -> Input new
parseToNewType additionalAttributes g f (Input input_) =
    Input
        { parse = input_.parse >> Result.andThen f
        , initialize = Maybe.map g >> input_.initialize
        , label = input_.label
        , type_ = input_.type_
        , additionalAttributes = input_.additionalAttributes ++ additionalAttributes
        }


string : String -> Input String
string label =
    Input
        { parse = Ok
        , initialize = Maybe.withDefault ""
        , type_ = "text"
        , label = label
        , additionalAttributes = []
        }


nonEmptyString : String -> Input (Maybe NonEmptyString)
nonEmptyString label =
    Input
        { parse = NonEmptyString.fromString >> Ok
        , initialize = Maybe.Extra.join >> Maybe.Extra.unwrap "" NonEmptyString.toString
        , type_ = "text"
        , label = label
        , additionalAttributes = []
        }


int : String -> Input (Maybe Int)
int label =
    Input
        { parse =
            \s ->
                if s == "" then
                    Ok Nothing

                else
                    String.toInt s |> Result.fromMaybe "Invalid" |> Result.map Just
        , initialize = Maybe.Extra.join >> Maybe.Extra.unwrap "" String.fromInt
        , type_ = "number"
        , label = label
        , additionalAttributes = []
        }


float : String -> Input (Maybe Float)
float label =
    Input
        { parse = String.toFloat >> Ok
        , initialize = Maybe.Extra.join >> Maybe.Extra.unwrap "" String.fromFloat
        , type_ = "number"
        , label = label
        , additionalAttributes = []
        }


focusBlurAttributes : List (Html.Attribute (FieldState -> FieldState))
focusBlurAttributes =
    [ Events.onFocus (updateStatus (FieldStatus.max FieldStatus.Focused))
    , Events.onBlur (updateStatus (FieldStatus.max FieldStatus.Blurred))
    ]


textInputAttributes : FieldState -> List (Html.Attribute (FieldState -> FieldState))
textInputAttributes oldState =
    focusBlurAttributes
        ++ [ Events.onInput <|
                \s newState ->
                    if newState.value == s then
                        newState

                    else
                        { value = s
                        , status = FieldStatus.max FieldStatus.Changed newState.status
                        }
           , Attributes.value oldState.value
           ]


type alias ViewConfig msg =
    { renderError : FieldStatus -> String -> Html msg
    , labelAttributes : { hasErrors : Bool, status : FieldStatus } -> List (Html.Attribute msg)
    , controlAttributes : { hasErrors : Bool, status : FieldStatus } -> List (Html.Attribute msg)
    }


type alias ViewContext =
    { extraError : Maybe String }


type alias FieldDefinition a =
    Composer.ChildDefinition ViewContext FieldState a


attributeToFieldState : Html.Attribute msg -> Html.Attribute (FieldState -> FieldState)
attributeToFieldState =
    Attributes.map (always identity)


htmlToFieldState : Html msg -> Html (FieldState -> FieldState)
htmlToFieldState =
    Html.map (always identity)


viewError : ViewConfig msg -> FieldState -> Maybe String -> List (Html (FieldState -> FieldState))
viewError view state =
    Maybe.Extra.unwrap [] (List.singleton << htmlToFieldState << view.renderError state.status)


checkbox :
    ViewConfig msg
    -> String
    -> FieldDefinition Bool
checkbox view label =
    let
        checkedToString b =
            if b then
                "on"

            else
                ""

        stringToChecked s =
            s == "on"
    in
    { codec = fieldStateCodec
    , view =
        \viewContext state ->
            let
                attributeConfig =
                    { hasErrors = Maybe.Extra.isJust viewContext.extraError, status = state.status }
            in
            Html.label (List.map attributeToFieldState <| view.labelAttributes attributeConfig)
                ([ Html.input
                    ([ Events.onCheck <|
                        \checked newState ->
                            let
                                newValue =
                                    checkedToString checked
                            in
                            if newState.value == newValue then
                                state

                            else
                                { value = newValue
                                , status = FieldStatus.max FieldStatus.Changed newState.status
                                }
                     , Attributes.checked <| stringToChecked state.value
                     , Attributes.type_ "checkbox"
                     ]
                        ++ (List.map attributeToFieldState (view.controlAttributes attributeConfig) ++ focusBlurAttributes)
                    )
                    []
                 , Html.text label
                 ]
                    ++ viewError view state viewContext.extraError
                )
    , parse = .value >> stringToChecked >> Just
    , initialize = Maybe.withDefault False >> checkedToString >> initFieldState
    }


input :
    ViewConfig msg
    -> Input a
    -> FieldDefinition a
input viewConfig (Input input_) =
    { codec = fieldStateCodec
    , view =
        \context state ->
            let
                error =
                    Maybe.Extra.or (Result.Extra.error <| input_.parse state.value) context.extraError

                attributeConfig =
                    { hasErrors = Maybe.Extra.isJust error, status = state.status }
            in
            Html.label (List.map attributeToFieldState <| viewConfig.labelAttributes attributeConfig) <|
                [ Html.text input_.label
                , Html.input (Attributes.type_ input_.type_ :: List.map attributeToFieldState (viewConfig.controlAttributes attributeConfig) ++ textInputAttributes state) []
                ]
                    ++ viewError viewConfig state error
    , parse = .value >> input_.parse >> Result.toMaybe
    , initialize = input_.initialize >> initFieldState
    }


requiredInput : Input (Maybe a) -> Input a
requiredInput =
    parseToNewType [ ( "required", Codec.encodeToValue Codec.bool True ) ] Just (Result.fromMaybe "Required")
