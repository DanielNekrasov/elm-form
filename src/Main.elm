module Main exposing (..)

import Browser
import Form.SelectVariant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as Encode
import List


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { form : Form
    }


type alias Form =
    { name : String
    , description : String
    , isImportant : Bool
    , measurementType : MeasurementType
    , yesNoParams : YesNoParams
    , numberParams : NumberParams
    , twoNumbersParams : TwoNumbersParams
    , selectParams : SelectParams
    , csrfToken : String
    }


type MeasurementType
    = YesNo
    | Number
    | TwoNumbers
    | Select


measurementToString : MeasurementType -> String
measurementToString type_ =
    case type_ of
        YesNo ->
            "bool"

        Number ->
            "number"

        TwoNumbers ->
            "two-numbers"

        Select ->
            "select"


type alias YesNoParams =
    { reference : Bool
    }


type alias NumberParams =
    { referenceFrom : String
    , referenceTo : String
    }


type alias TwoNumbersParams =
    ( NumberParams, NumberParams )



-- SELECT MODEL


type alias SelectParams =
    { type_ : SelectView
    , items : List Form.SelectVariant.Model
    , uid : Int
    }


type SelectView
    = SelectView
    | RadioView
    | CheckboxView


selectViewToString : SelectView -> String
selectViewToString type_ =
    case type_ of
        SelectView ->
            "select"

        RadioView ->
            "radio"

        CheckboxView ->
            "checkbox"


emptyModel : Model
emptyModel =
    { form =
        { name = ""
        , description = ""
        , isImportant = False
        , measurementType = YesNo
        , yesNoParams = { reference = False }
        , numberParams = { referenceFrom = "", referenceTo = "" }
        , twoNumbersParams =
            ( { referenceFrom = "", referenceTo = "" }
            , { referenceFrom = "", referenceTo = "" }
            )
        , selectParams = { type_ = SelectView, items = [], uid = 0 }
        , csrfToken = ""
        }
    }



--init : String -> ( Model, Cmd Msg )
--init csrfToken =
--    updateForm (\form -> { form | csrfToken = csrfToken }) emptyModel


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel
    , Cmd.none
    )



--UPDATE


type Msg
    = SubmittedForm
    | EnteredName String
    | EnteredDescription String
    | TypeChanged MeasurementType
    | ToggleIsImportant
    | YesNoMsg YesNoMsg
    | NumberMsg NumberMsg
    | TwoNumbersMsg TwoNumbersMsg
    | SelectMsg SelectMsg
    | CompletedSend (Result Http.Error String)


type YesNoMsg
    = EnteredReference Bool


type NumberMsg
    = EnteredReferenceFrom String
    | EnteredReferenceTo String


type TwoNumbersMsg
    = ChangedFirst NumberMsg
    | ChangedSecond NumberMsg


type SelectMsg
    = UpdateVariant ( Int, Form.SelectVariant.Msg )
    | ChangedView SelectView
    | AddVariant String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            ( model, submit model.form )

        TypeChanged type_ ->
            updateForm (\form -> { form | measurementType = type_ }) model

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        ToggleIsImportant ->
            updateForm (\form -> { form | isImportant = not form.isImportant }) model

        YesNoMsg yesNoMsg ->
            case yesNoMsg of
                EnteredReference reference ->
                    updateForm (\form -> updateYesNoParams (\params -> { params | reference = reference }) form) model

        NumberMsg numberMsg ->
            case numberMsg of
                EnteredReferenceFrom referenceFrom ->
                    updateForm (\form -> updateNumberParams (\params -> { params | referenceFrom = referenceFrom }) form) model

                EnteredReferenceTo referenceTo ->
                    updateForm (\form -> updateNumberParams (\params -> { params | referenceTo = referenceTo }) form) model

        TwoNumbersMsg twoNumbersMsg ->
            case twoNumbersMsg of
                ChangedFirst referenceMsg ->
                    case referenceMsg of
                        EnteredReferenceFrom referenceFrom ->
                            updateForm
                                (\form ->
                                    updateTwoNumbersParams
                                        (\params -> Tuple.mapFirst (\first -> { first | referenceFrom = referenceFrom }) params)
                                        form
                                )
                                model

                        EnteredReferenceTo referenceTo ->
                            updateForm
                                (\form ->
                                    updateTwoNumbersParams
                                        (\params -> Tuple.mapFirst (\first -> { first | referenceTo = referenceTo }) params)
                                        form
                                )
                                model

                ChangedSecond referenceMsg ->
                    case referenceMsg of
                        EnteredReferenceFrom referenceFrom ->
                            updateForm
                                (\form ->
                                    updateTwoNumbersParams
                                        (\params -> Tuple.mapSecond (\second -> { second | referenceFrom = referenceFrom }) params)
                                        form
                                )
                                model

                        EnteredReferenceTo referenceTo ->
                            updateForm
                                (\form ->
                                    updateTwoNumbersParams
                                        (\params -> Tuple.mapSecond (\second -> { second | referenceTo = referenceTo }) params)
                                        form
                                )
                                model

        SelectMsg selectMsg ->
            case selectMsg of
                ChangedView type_ ->
                    updateForm
                        (\form ->
                            updateSelectParams
                                (\params ->
                                    { params | type_ = type_ }
                                )
                                form
                        )
                        model

                AddVariant _ ->
                    updateForm
                        (\form ->
                            updateSelectParams
                                (\params ->
                                    { params
                                        | uid = params.uid + 1
                                        , items = params.items ++ [ Form.SelectVariant.init "" params.uid ]
                                    }
                                )
                                form
                        )
                        model

                UpdateVariant ( id, variantMsg ) ->
                    let
                        updateTask t =
                            if t.id == id then
                                Form.SelectVariant.update variantMsg t

                            else
                                Just t

                        newModel =
                            updateForm
                                (\form ->
                                    updateSelectParams
                                        (\params ->
                                            { params | items = List.filterMap updateTask params.items }
                                        )
                                        form
                                )
                                model
                    in
                    case variantMsg of
                        _ ->
                            newModel

        CompletedSend (Err error) ->
            ( model, Cmd.none )

        CompletedSend (Ok response) ->
            ( model, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


updateYesNoParams : (YesNoParams -> YesNoParams) -> Form -> Form
updateYesNoParams transform form =
    { form | yesNoParams = transform form.yesNoParams }


updateNumberParams : (NumberParams -> NumberParams) -> Form -> Form
updateNumberParams transform form =
    { form | numberParams = transform form.numberParams }


updateTwoNumbersParams : (TwoNumbersParams -> TwoNumbersParams) -> Form -> Form
updateTwoNumbersParams transform form =
    { form | twoNumbersParams = transform form.twoNumbersParams }


updateSelectParams : (SelectParams -> SelectParams) -> Form -> Form
updateSelectParams transform form =
    { form | selectParams = transform form.selectParams }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


submit : Form -> Cmd Msg
submit form =
    let
        body =
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "description", Encode.string form.description )
                , ( "type", Encode.string (measurementToString form.measurementType) )
                , ( "is_important", Encode.bool form.isImportant )
                , ( "options"
                  , Encode.object
                        [ ( "bool"
                          , Encode.object
                                [ ( "reference", Encode.bool form.yesNoParams.reference )
                                ]
                          )
                        , ( "number"
                          , Encode.object
                                [ ( "reference_from", Encode.string form.numberParams.referenceFrom )
                                , ( "reference_to", Encode.string form.numberParams.referenceTo )
                                ]
                          )
                        , ( "two-numbers"
                          , Encode.list
                                (\item ->
                                    Encode.object
                                        [ ( "reference_from", Encode.string item.referenceFrom )
                                        , ( "reference_to", Encode.string item.referenceTo )
                                        ]
                                )
                                [ Tuple.first form.twoNumbersParams, Tuple.second form.twoNumbersParams ]
                          )
                        , ( "select"
                          , Encode.object
                                [ ( "select_type", Encode.string (selectViewToString form.selectParams.type_) )
                                , ( "values"
                                  , Encode.list
                                        (\item ->
                                            Encode.object
                                                [ ( "title", Encode.string item.description )
                                                , ( "reference", Encode.bool item.isChecked )
                                                ]
                                        )
                                        form.selectParams.items
                                  )
                                ]
                          )
                        ]
                  )
                ]
                |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-CSRF-TOKEN" form.csrfToken ]
        , url = "/app/admin/measurements"
        , body = body
        , expect = Http.expectString CompletedSend
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW


view : Model -> Html Msg
view model =
    viewForm model.form


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ div [ class "form-group" ]
            [ label [ for "name" ] [ text "Название" ]
            , input [ id "name", class "form-control", type_ "text", value form.name, onInput EnteredName ] []
            ]
        , div [ class "form-group" ]
            [ label [ for "description" ] [ text "Описание" ]
            , textarea [ id "description", class "form-control", onInput EnteredDescription ] [ text form.description ]
            ]
        , div [ class "form-group form-check" ]
            [ label []
                [ input
                    [ type_ "checkbox"
                    , class "form-check-input"
                    , checked form.isImportant
                    , onClick ToggleIsImportant
                    ]
                    []
                , text "Важное"
                ]
            ]
        , div [ class "form-group" ]
            [ label [ for "measurement-type" ] [ text "Тип измерения" ]
            , select
                [ class "form-control"
                , id "measurement-type"
                , onInput
                    (\str ->
                        if str == "bool" then
                            TypeChanged YesNo

                        else if str == "number" then
                            TypeChanged Number

                        else if str == "two-numbers" then
                            TypeChanged TwoNumbers

                        else
                            TypeChanged Select
                    )
                ]
              <|
                List.map
                    (\item ->
                        option
                            [ selected (item.type_ == form.measurementType), value item.value ]
                            [ text item.label
                            ]
                    )
                    [ { value = measurementToString YesNo, type_ = YesNo, label = "Да / Нет" }
                    , { value = measurementToString Number, type_ = Number, label = "Число" }
                    , { value = measurementToString TwoNumbers, type_ = TwoNumbers, label = "Два числа" }
                    , { value = measurementToString Select, type_ = Select, label = "Выбор значения" }
                    ]
            ]
        , viewTypedForm form
        , button [ type_ "submit", class "btn btn-primary w-xs-100 mt-2" ] [ text "Добавить" ]
        ]


viewTypedForm : Form -> Html Msg
viewTypedForm form =
    case form.measurementType of
        YesNo ->
            viewYesNoProps form.yesNoParams

        Number ->
            viewNumberProps form.numberParams NumberMsg

        TwoNumbers ->
            viewTwoNumbersProps form.twoNumbersParams

        Select ->
            viewSelectProps form.selectParams


viewNumberProps : NumberParams -> (NumberMsg -> msg) -> Html msg
viewNumberProps props msgOnInput =
    div [ class "form-row" ]
        [ div [ class "form-group col-6" ]
            [ div [ class "form-group" ]
                [ label [ for "reference-from" ] [ text "Норма \"от\"" ]
                , input
                    [ id "reference-from"
                    , class "form-control"
                    , type_ "number"
                    , value props.referenceFrom
                    , onInput (msgOnInput << EnteredReferenceFrom)
                    ]
                    []
                ]
            ]
        , div [ class "form-group col-6" ]
            [ div [ class "form-group" ]
                [ label [ for "reference-to" ] [ text "Норма \"до\"" ]
                , input
                    [ id "reference-to"
                    , class "form-control"
                    , type_ "number"
                    , value props.referenceTo
                    , onInput (msgOnInput << EnteredReferenceTo)
                    ]
                    []
                ]
            ]
        ]


viewTwoNumbersProps : TwoNumbersParams -> Html Msg
viewTwoNumbersProps props =
    div []
        [ viewNumberProps (Tuple.first props) (TwoNumbersMsg << ChangedFirst)
        , viewNumberProps (Tuple.second props) (TwoNumbersMsg << ChangedSecond)
        ]


viewYesNoProps : YesNoParams -> Html Msg
viewYesNoProps props =
    div
        [ class "form-group" ]
        [ label [ for "yes-no-value" ] [ text "Норма" ]
        , select
            [ class "form-control"
            , id "yes-no-value"
            , onInput
                (\str ->
                    if str == "yes" then
                        (YesNoMsg << EnteredReference) True

                    else
                        (YesNoMsg << EnteredReference) False
                )
            ]
          <|
            List.map
                (\item ->
                    option
                        [ selected (item.type_ == props.reference), value item.value ]
                        [ text item.text
                        ]
                )
                [ { value = "yes", type_ = True, text = "Да" }
                , { value = "no", type_ = False, text = "Нет" }
                ]
        ]


viewSelectProps : SelectParams -> Html Msg
viewSelectProps props =
    div []
        [ div
            [ class "form-group" ]
            [ label [ for "select-view" ] [ text "Вид" ]
            , select
                [ class "form-control"
                , id "select-view"
                , onInput
                    (\str ->
                        if str == "radio" then
                            (SelectMsg << ChangedView) RadioView

                        else if str == "checkbox" then
                            (SelectMsg << ChangedView) CheckboxView

                        else
                            (SelectMsg << ChangedView) SelectView
                    )
                ]
              <|
                List.map
                    (\item ->
                        option
                            [ selected (item.type_ == props.type_), value item.value ]
                            [ text item.text
                            ]
                    )
                    [ { value = "radio", type_ = RadioView, text = "Радио кнопки" }
                    , { value = "checkbox", type_ = CheckboxView, text = "Влажки" }
                    , { value = "select", type_ = SelectView, text = "Выпадающий список" }
                    ]
            ]
        , p [ class "mb-2" ] [ text "Значения" ]
        , div [] <|
            List.map
                (\item ->
                    let
                        taskView =
                            Form.SelectVariant.view item
                    in
                    Html.map (\msg -> (SelectMsg << UpdateVariant) ( item.id, msg )) taskView
                )
                props.items
        , div [ class "form-group mt-2" ]
            [ button [ type_ "button", class "btn btn-outline-secondary", onClick ((SelectMsg << AddVariant) "") ]
                [ text "Добавить значение"
                ]
            ]
        ]
