module Form.SelectVariant exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Svg exposing (svg, use)
import Svg.Attributes as SvgAttr



-- MODEL


type alias Model =
    { description : String
    , isChecked : Bool
    , id : Int
    }


init : String -> Int -> Model
init desc id =
    { description = desc
    , isChecked = False
    , id = id
    }



-- UPDATE


type Msg
    = Edit String
    | Checked Bool
    | Delete


update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        Edit rawDescription ->
            let
                description =
                    String.trim rawDescription
            in
            Just
                { model
                    | description = description
                }

        Checked bool ->
            Just { model | isChecked = bool }

        Delete ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    let
        description =
            model.description

        elementId =
            "variant-" ++ String.fromInt model.id
    in
    div [ class "form-row align-items-end mt-2" ]
        [ div [ class "col" ]
            [ label [ class "sr-only", for "value-title" ] [ text "Значение" ]
            , input
                [ type_ "text"
                , id elementId
                , class "form-control"
                , value description
                , onInput Edit
                ]
                []
            ]
        , div [ class "col-auto" ]
            [ div [ class "form-check mb-2" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , checked model.isChecked
                    , onClick (Checked (not model.isChecked))
                    ]
                    []
                , label [ class "form-check-label" ] []
                ]
            ]
        , div [ class "col-auto" ]
            [ button [ class "btn btn-link text-danger", onClick Delete ]
                [ svg [ SvgAttr.class "icon", SvgAttr.width "20", SvgAttr.height "20" ]
                    [ use [ SvgAttr.xlinkHref "/assets/icons/sprite.svg#img-trash" ] []
                    ]
                ]
            ]
        ]
