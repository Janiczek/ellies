module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Table


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { nameFilter : String
    , tableState : Table.State
    , ellies : WebData (List Ellie)
    }


type alias Ellie =
    { url : String
    , name : String
    , clicks : Int
    }


type Msg
    = RefreshEllies
    | GotEllies (Result Http.Error (List Ellie))
    | NewTableState Table.State
    | SetNameFilter String
    | Click String
    | ClickResult String (Result Http.Error Int)


init : () -> ( Model, Cmd Msg )
init flags =
    ( { nameFilter = ""
      , tableState = Table.initialSort "Name"
      , ellies = Loading
      }
    , refreshEllies
    )


refreshEllies : Cmd Msg
refreshEllies =
    Http.get
        { url = "https://janiczek-ellies.builtwithdark.com/list"
        , expect = Http.expectJson GotEllies (Decode.list ellieDecoder)
        }


incrementClicks : String -> Cmd Msg
incrementClicks url =
    Http.get
        { url = "https://janiczek-ellies.builtwithdark.com/click?id=" ++ url
        , expect = Http.expectJson (ClickResult url) Decode.int
        }


ellieDecoder : Decoder Ellie
ellieDecoder =
    Decode.map3 Ellie
        (Decode.field "url" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "clicks" Decode.int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshEllies ->
            ( { model | ellies = Loading }
            , refreshEllies
            )

        GotEllies result ->
            ( { model | ellies = RemoteData.fromResult result }
            , Cmd.none
            )

        NewTableState state ->
            ( { model | tableState = state }
            , Cmd.none
            )

        SetNameFilter filter ->
            ( { model | nameFilter = filter }
            , Cmd.none
            )

        Click url ->
            ( model
            , incrementClicks url
            )

        ClickResult _ (Err _) ->
            -- ???
            ( model, Cmd.none )

        ClickResult url (Ok newClicks) ->
            ( { model
                | ellies =
                    model.ellies
                        |> RemoteData.map
                            (\ellies ->
                                ellies
                                    |> List.map
                                        (\ellie ->
                                            if ellie.url == url then
                                                { ellie | clicks = newClicks }

                                            else
                                                ellie
                                        )
                            )
              }
            , Cmd.none
            )


refreshElliesBtn : Html Msg
refreshElliesBtn =
    Html.button
        [ Events.onClick RefreshEllies ]
        [ Html.text "Refresh Ellies" ]


tableConfig : Table.Config Ellie Msg
tableConfig =
    Table.config
        { toId = .url
        , toMsg = NewTableState
        , columns =
            [ Table.customColumn
                { name = "Ellie ID"
                , viewData = .url
                , sorter = Table.unsortable
                }
            , Table.veryCustomColumn
                { name = "Name"
                , viewData =
                    \{ url, name } ->
                        { attributes = []
                        , children =
                            [ Html.a
                                [ Events.onClick (Click url)
                                , Attrs.href ("https://ellie-app.com/" ++ url)
                                , Attrs.target "_blank"
                                ]
                                [ Html.text name ]
                            ]
                        }
                , sorter = Table.increasingOrDecreasingBy .name
                }
            , Table.intColumn "Clicks" .clicks
            ]
        }


bookmarkletUrl : String
bookmarkletUrl =
    "https://raw.githubusercontent.com/Janiczek/ellies/master/src/save-ellie.min.js"


view : Model -> Browser.Document Msg
view model =
    { title = "Ellies catalog"
    , body =
        [ Html.h1 [] [ Html.text "Ellies catalog" ]
        , Html.h2 []
            [ Html.text "Use "
            , Html.a
                [ Attrs.href bookmarkletUrl
                , Attrs.target "_blank"
                ]
                [ Html.text "this bookmarklet" ]
            , Html.text " on an Ellie to save it here!"
            ]
        , case model.ellies of
            NotAsked ->
                -- ???
                refreshElliesBtn

            Loading ->
                Html.text "Loading Ellies"

            Failure err ->
                Html.div []
                    [ refreshElliesBtn
                    , Html.div []
                        [ Html.text "Error loading Ellies! Please ping "
                        , Html.a
                            [ Attrs.href "https://twitter.com/janiczek"
                            , Attrs.target "_blank"
                            ]
                            [ Html.text "@janiczek" ]
                        , Html.text "."
                        ]
                    ]

            Success ellies ->
                let
                    filteredEllies =
                        if String.isEmpty model.nameFilter then
                            ellies

                        else
                            ellies
                                |> List.filter
                                    (\{ name } ->
                                        String.toLower name
                                            |> String.contains (String.toLower model.nameFilter)
                                    )
                in
                Html.div []
                    [ Html.input
                        [ Attrs.placeholder "Filter the name..."
                        , Events.onInput SetNameFilter
                        , Attrs.value model.nameFilter
                        ]
                        []
                    , Table.view tableConfig model.tableState filteredEllies
                    ]
        ]
    }
