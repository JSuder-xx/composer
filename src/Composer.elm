module Composer exposing
    ( ChildDefinition, ChildInstance
    , ParentDefinition, aParent, hasChild, parentAsChild
    , ParentInstance, initInstance, parse, viewParent
    )

{-|

@docs ChildDefinition, ChildInstance
@docs ParentDefinition, aParent, hasChild, parentAsChild
@docs ParentInstance, initInstance, parse, viewParent

-}

import Array exposing (Array)
import Array.Extra
import Codec exposing (Codec, Value)
import Html exposing (Html)


{-| Protocol to define an element that can be composed as a child of a parent.

**NOTE:** This record is not intended prescribe how to model elements. Model however is appropriate and then
convert that model to this record in order to plugin into `Composer`.

-}
type alias ChildDefinition viewContext state output =
    { view : viewContext -> state -> Html (state -> state)
    , codec : Codec state
    , parse : state -> Maybe output
    , initialize : Maybe output -> state
    }


{-| A child passed to a parent in its combineAndView where the child has state and a parsed output.
-}
type alias ChildInstance viewContext state output outputCombined =
    { view : viewContext -> Html (ParentInstance outputCombined -> ParentInstance outputCombined)
    , parsed : Maybe output
    , state : state
    }


type ParentInstance output
    = Instance (Array Value)


type ParentDefinition_ combineAndView output
    = ParentDefinition
        { produce : ParentInstance output -> combineAndView
        , toInitialValues : Maybe output -> List Value
        , fieldCount : Int
        }


type alias Composed viewContext output =
    { parsed : Maybe output, view : viewContext -> Html (ParentInstance output -> ParentInstance output) }


{-| Definition of a composed parent (consisting of children).
-}
type alias ParentDefinition viewContext output =
    ParentDefinition_ (Composed viewContext output) output


{-| Initialize an instance from a definition and (optionally) the model that the definition produces. This is generally called inside your model init.
-}
initInstance : ParentDefinition_ combineAndView output -> Maybe output -> ParentInstance output
initInstance (ParentDefinition { toInitialValues }) =
    Instance << Array.fromList << List.reverse << toInitialValues


{-| Start defining a parent.

    aParent
        (\appleInstance bananaInstance ->
            { parsed = Maybe.map2 SomeModel appleInstance.parsed bananaInstance.parsed
            , view =
                \_ ->
                    Html.fieldset []
                        [ appleInstance.view { extraError = Nothing }
                        , bananaInstance.view { extraError = Nothing }
                        ]
            }
        )
        |> hasChild .apple Apple.childDefinition
        |> hasChild .banana Banana.childDefinition

-}
aParent : compose -> ParentDefinition_ compose output
aParent compose =
    ParentDefinition
        { produce = \_ -> compose
        , fieldCount = 0
        , toInitialValues = \_ -> []
        }


{-| Add a child _definition_ to a parent. The child _instance_ will be passed to the parent's composition function.

    aParent
        (\appleInstance bananaInstance ->
            { parsed = Maybe.map2 SomeModel appleInstance.parsed bananaInstance.parsed
            , view =
                \_ ->
                    Html.fieldset []
                        [ appleInstance.view { extraError = Nothing }
                        , bananaInstance.view { extraError = Nothing }
                        ]
            }
        )
        |> hasChild .apple Apple.childDefinition
        |> hasChild .banana Banana.childDefinition

-}
hasChild :
    (outputCombined -> output)
    -> ChildDefinition childViewContext state output
    -> ParentDefinition_ (ChildInstance childViewContext state output outputCombined -> compose) outputCombined
    -> ParentDefinition_ compose outputCombined
hasChild combinedToOutput childDefinition_ (ParentDefinition original) =
    ParentDefinition
        { fieldCount = original.fieldCount + 1
        , produce =
            \(Instance originalValues) ->
                let
                    evaluateAtomState : Array Value -> state
                    evaluateAtomState elementValues =
                        elementValues
                            |> Array.get original.fieldCount
                            |> Maybe.andThen (Codec.decodeValue childDefinition_.codec >> Result.toMaybe)
                            |> Maybe.withDefault (childDefinition_.initialize Nothing)

                    atomState : state
                    atomState =
                        evaluateAtomState originalValues

                    atomView : childViewContext -> Html (ParentInstance outputCombined -> ParentInstance outputCombined)
                    atomView childViewContext =
                        childDefinition_.view childViewContext atomState
                            |> Html.map
                                (\updateAtomState (Instance newValues) ->
                                    evaluateAtomState newValues
                                        |> updateAtomState
                                        |> Codec.encodeToValue childDefinition_.codec
                                        |> (\value -> Instance <| Array.Extra.update original.fieldCount (always value) newValues)
                                )

                    childInstance : ChildInstance childViewContext state output outputCombined
                    childInstance =
                        { parsed = childDefinition_.parse atomState
                        , view = atomView
                        , state = atomState
                        }
                in
                original.produce (Instance originalValues) childInstance
        , toInitialValues =
            \maybeOutputCombined -> Codec.encodeToValue childDefinition_.codec (childDefinition_.initialize <| Maybe.map combinedToOutput maybeOutputCombined) :: original.toInitialValues maybeOutputCombined
        }


{-| Convert a `ParentDefinition` into a `Child` that can be embedded in another parent. This allows indefinite nesting of models in models.

    address : Definition Address
    address = Debug.todo "ellided"

    order : Definition Order
    order =
        (\billingAddressE shippingAddressE ->
            { composed = Result.fromMaybe "" <| Maybe.map2 Order billingAddressE.parsed shippingAddressE.parsed
            , view =
                HTML.div
                    []
                    [ billingAddressE.view ()
                    , shippingAddressE.view ()
                    ]
            }
        )
            |> define
            |> atom .billingAddress (toAtom address)
            |> atom .shippingAddress (toAtom address)

-}
parentAsChild : ParentDefinition viewContext output -> ChildDefinition viewContext (ParentInstance output) output
parentAsChild parentDefinition_ =
    { view = viewParent parentDefinition_
    , codec = Codec.array Codec.value |> Codec.map Instance (\(Instance x) -> x)
    , parse = parse parentDefinition_
    , initialize = initInstance parentDefinition_
    }


{-| Render a `Composer` instance.

**BE CAREFUL!!!** The `Definition` and `Instance` are unified on the output type BUT it is possible
to make multiple definitions for the same output. If you apply a definition X to an instance that was initialized
with definition Y then you will get wonky behavior at run-time / it won't work.

-}
viewParent : ParentDefinition context output -> context -> ParentInstance output -> Html (ParentInstance output -> ParentInstance output)
viewParent (ParentDefinition pd) context instance_ =
    (pd.produce instance_).view context


{-| Attempt to get an output from a `Composer` instance.

**BE CAREFUL!!!** The `Definition` and `Instance` are unified on the output type BUT it is possible
to make multiple definitions for the same output. If you apply a definition X to an instance that was initialized
with definition Y then you will get wonky behavior at run-time / it won't work.

-}
parse : ParentDefinition context output -> ParentInstance output -> Maybe output
parse (ParentDefinition pd) instance_ =
    (pd.produce instance_).parsed
