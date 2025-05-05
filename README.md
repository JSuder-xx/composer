# composer

Elm package to remove boiler plate from parent/child communication when no side effects are required. The Api has

- BOTH extraordinary benefits
  - Zero boiler plate.
  - Infinite nesting / composition of elements.
- AND significant requirements
  - Each element must provide a "functionalized" version of itself where it can produce a function `state -> Html (state -> state)`. This is not as steep a requirement
  as you might think. Given `update : Msg -> Model -> Model` and `view : Model -> Html Msg` then `view >> Html.map update` produces a function that works with this library.
  - Each element operates on its own data. Relationships between elements are formed in a parent's composition function.

See [Example](src/Example.elm) for an illustration of succinctly declaring nested user interfaces.

This repository will become TWO packages

- `jsuder-xx/composer` a narrowly scoped package that defines an Api that removes all boiler plate in composing parent/child interfaces.
- `jsuder-xx/composer-field` an _example_ of form fields that can be included in a parent as children.

Many of the patterns were inspired by `https://github.com/dillonkearns/elm-form` but this library has a few significant differences

- It is more general. `elm-form` builds forms of fields. Forms cannot be nested inside other forms. This library supports defining arbitrary nested user interfaces of any kind of data.
- It is even more succinct. When defining a child field in `elm-form` with [Form.field](https://package.elm-lang.org/packages/dillonkearns/elm-form/latest/Form#field) the developer must provide a name for the field.
  This library has no such requirement.
