module Counter exposing (main)

import Browser
import Html exposing (Html, button, div, map, text)
import Html.Events exposing (onClick)
import Transformer as T


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Int


transformerModel : T.Transformer Model
transformerModel =
    T.int


init : Model
init =
    0


type Msg
    = Decrement
    | Increment
    | MsgTransformer T.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1

        MsgTransformer msgTransformer ->
            T.update msgTransformer
                |> Maybe.map (T.decode transformerModel)
                |> Maybe.withDefault model


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        , map MsgTransformer (T.viewFormElmUiAsHtml (T.encode transformerModel model))
        ]
