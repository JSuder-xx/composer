module Test exposing (..)

import Browser
import Composer as Container exposing (ParentDefinition, aParent, hasChild, parentAsChild)
import Composer.Example.Field as Input
import Composer.Example.FieldStatus as FieldStatus
import Composer.Example.NonEmptyString exposing (NonEmptyString)
import Html
import Html.Attributes exposing (style)
import Maybe.Extra
import Result.Extra


type alias IntRange =
    { start : Int
    , end : Int
    }


intRange : ParentDefinition () IntRange
intRange =
    (\startE endE ->
        let
            initialParsed =
                Maybe.map2 IntRange
                    startE.parsed
                    endE.parsed

            parsed =
                initialParsed
                    |> Maybe.Extra.unwrap (Err "")
                        (\range ->
                            if range.end < range.start then
                                Err "End must be after start"

                            else
                                Ok range
                        )
        in
        { parsed = Result.toMaybe parsed
        , view =
            \_ ->
                Html.fieldset []
                    [ startE.view { extraError = Nothing }
                    , endE.view { extraError = Result.Extra.error parsed }
                    ]
        }
    )
        |> aParent
        |> hasChild .start (Input.input viewConfig <| Input.requiredInput <| Input.int "Start")
        |> hasChild .end (Input.input viewConfig <| Input.requiredInput <| Input.int "End")


type alias Address =
    { line1 : NonEmptyString
    , line2 : String
    , city : NonEmptyString
    , zipCode : NonEmptyString
    }


address : ParentDefinition () Address
address =
    aParent
        (\line1E line2E cityE zipCodeE ->
            { parsed =
                Maybe.map4 Address
                    line1E.parsed
                    line2E.parsed
                    cityE.parsed
                    zipCodeE.parsed
            , view =
                \_ ->
                    Html.fieldset []
                        [ line1E.view { extraError = Nothing }
                        , line2E.view { extraError = Nothing }
                        , cityE.view { extraError = Nothing }
                        , zipCodeE.view { extraError = Nothing }
                        ]
            }
        )
        |> hasChild .line1 (Input.input viewConfig <| Input.requiredInput <| Input.nonEmptyString "Line 1")
        |> hasChild .line2 (Input.input viewConfig <| Input.string "Line 2")
        |> hasChild .city (Input.input viewConfig <| Input.requiredInput <| Input.nonEmptyString "City")
        |> hasChild .zipCode (Input.input viewConfig <| Input.requiredInput <| Input.nonEmptyString "Zip Code")


type alias Person =
    { firstName : NonEmptyString
    , lastName : NonEmptyString
    , isCool : Bool
    , numberOfDogs : Int
    , address : Address
    , range : IntRange
    }


person : ParentDefinition () Person
person =
    aParent
        (\firstNameE lastNameE isCoolE numberOfDogsE addressE rangeE ->
            { parsed =
                Just Person
                    |> Maybe.Extra.andMap firstNameE.parsed
                    |> Maybe.Extra.andMap lastNameE.parsed
                    |> Maybe.Extra.andMap isCoolE.parsed
                    |> Maybe.Extra.andMap numberOfDogsE.parsed
                    |> Maybe.Extra.andMap addressE.parsed
                    |> Maybe.Extra.andMap rangeE.parsed
            , view =
                \_ ->
                    Html.form []
                        [ firstNameE.view { extraError = Nothing }
                        , lastNameE.view { extraError = Nothing }
                        , isCoolE.view { extraError = Nothing }
                        , numberOfDogsE.view { extraError = Nothing }
                        , addressE.view ()
                        , rangeE.view ()
                        ]
            }
        )
        |> hasChild .firstName (Input.input viewConfig <| Input.requiredInput <| Input.nonEmptyString "First Name")
        |> hasChild .lastName (Input.input viewConfig <| Input.requiredInput <| Input.nonEmptyString "Last Name")
        |> hasChild .isCool (Input.checkbox viewConfig "Is Cool")
        |> hasChild .numberOfDogs (Input.input viewConfig <| Input.requiredInput <| Input.int "Number of Dogs")
        |> hasChild .address (parentAsChild address)
        |> hasChild .range (parentAsChild intRange)


viewConfig : Input.ViewConfig msg
viewConfig =
    { renderError =
        \status msg ->
            if FieldStatus.Blurred == status then
                Html.small [ style "color" "red" ] [ Html.text msg ]

            else
                Html.text ""
    , controlAttributes =
        \{ hasErrors, status } ->
            style "margin-left" "4px"
                :: (if status == FieldStatus.Blurred && hasErrors then
                        [ style "border" "solid 1px #800" ]

                    else
                        []
                   )
    , labelAttributes = \_ -> [ style "display" "block", style "margin" "4px" ]
    }


main : Program () (Container.ParentInstance Person) (Container.ParentInstance Person -> Container.ParentInstance Person)
main =
    Browser.sandbox
        { init = Container.initInstance person Nothing
        , view =
            \state ->
                Html.div [ style "padding" "8px" ]
                    [ Container.viewParent person () state
                    , Html.text <| Maybe.Extra.unwrap "Invalid" (always "Valid") <| Container.parse person state
                    ]
        , update = (<|)
        }
