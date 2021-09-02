module FetchJsonData1 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Post =
    { id : Int
    , name : String
    , tagline : String
    , first_brewed : String    
    , contributed_by: String
    , image_url: String
    }



type alias Model =
    { posts : List Post
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from url" ]
        , viewPostsOrError model
        ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewPosts model.posts


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewPost posts)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Name" ]
        , th []
            [ text "Tagline" ]
        , th []
            [ text "First Brewed" ]
        , th []
            [ text "Contributed By" ]
        , th []
            [ text "Image URL" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    tr []
        [ td []
            [ text (String.fromInt post.id) ]
        , td []
            [ text post.name ]
        , td []
            [ text post.tagline ]
        , td []
            [ text post.first_brewed ]
        , td []
            [ text post.contributed_by ]        
        , td []
            [ a [ href post.image_url ] [ text post.image_url ] ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Post))


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "name" string
        |> required "tagline" string
        |> required "first_brewed" string
        |> required "contributed_by" string
        |> required "image_url" string


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "https://api.punkapi.com/v2/beers"
        , expect = Http.expectJson DataReceived (list postDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok posts) ->
            ( { model
                | posts = posts
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
