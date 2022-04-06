module Main exposing (..)

-- For Api.Object.Show scope
import Api.Object exposing (Show)
import Api.Object.Show as ShowFields
import Api.Query as Query
import Browser
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData)


type alias Response =
    List Show


type alias Show =
    { title: String
    , releaseYear: Int
    }


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | ChangeText String
    | Scan


type alias Model =
    { filter : String
    , shows : RemoteData (Graphql.Http.Error Response) Response
    }


query : Model -> SelectionSet Response RootQuery
query model =
    Query.shows { titleFilter = model.filter } showInfoSelection


showInfoSelection : SelectionSet Show Api.Object.Show
showInfoSelection =
    SelectionSet.map2 Show
        ShowFields.title
        ShowFields.releaseYear

makeRequest : Model -> Cmd Msg
makeRequest model =
    query model
        |> Graphql.Http.queryRequest "http://localhost:8080/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { shows = RemoteData.NotAsked, filter = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( { model | shows = response }, Cmd.none )

        ChangeText s ->
            ( { model | filter = s }, Cmd.none )

        Scan ->
            ( model, makeRequest model )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.filter, onInput ChangeText ] []
        , div []
            [ button [ onClick Scan ] [ text "Get Shows" ]
            ]
        , div []
            [ viewResponse model.shows
            ]
        ]


viewResponse model =
    case model of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "loading"

        RemoteData.Success response ->
            text (Debug.toString response)

        RemoteData.Failure httpError ->
            text ("Error: " ++ Debug.toString httpError)
