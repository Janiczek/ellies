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
      , tableState = Table.initialSort "Clicks"
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
    Html.div []
        [ Html.button
            [ Events.onClick RefreshEllies ]
            [ Html.text "Refresh Ellies" ]
        ]


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
                                , Events.on "auxclick" (middleButtonDecoder (Click url))
                                , Attrs.href ("https://ellie-app.com/" ++ url)
                                , Attrs.target "_blank"
                                ]
                                [ Html.text name ]
                            ]
                        }
                , sorter = Table.increasingOrDecreasingBy .name
                }
            , Table.customColumn
                { name = "Clicks"
                , viewData = String.fromInt << .clicks
                , sorter = Table.decreasingOrIncreasingBy .clicks
                }
            ]
        }


middleButtonDecoder : msg -> Decoder msg
middleButtonDecoder msg =
    Decode.field "which" Decode.int
        |> Decode.andThen
            (\which ->
                if which == 2 then
                    Decode.succeed msg

                else
                    Decode.fail "not the middle mouse button"
            )


screencastUrl : String
screencastUrl =
    "https://janiczek.github.io/ellies/screencast.mp4"


bookmarkletSource : String
bookmarkletSource =
    """(function(){if(document.location.origin!=='https://ellie-app.com'){alert('Try this bookmarklet on an Ellie page!');return}var id=document.location.pathname.slice(1);if(id==='new'){alert("Didn't save to the catalog yet - you'll have to save your Ellie first!");return}var name=prompt('Name the Ellie');if(name==null){return}var xhr=new XMLHttpRequest();xhr.onload=function(){if(xhr.status>=200&&xhr.status<300){alert(xhr.response)}else{alert('Something went wrong :( Ping @janiczek on Twitter or Elm Slack!')}};xhr.open('GET','https://janiczek-ellies.builtwithdark.com/save?id='+encodeURIComponent(id)+'&name='+encodeURIComponent(name));xhr.send()}());"""


view : Model -> Browser.Document Msg
view model =
    { title = "Ellies catalog"
    , body =
        [ Html.h1 [] [ Html.text "Ellies catalog" ]
        , Html.p []
            [ Html.strong []
                [ Html.a
                    [ Attrs.href "https://ellie-app.com/new"
                    , Attrs.target "_blank"
                    ]
                    [ Html.text "Ellie" ]
                , Html.text " is an "
                , Html.a
                    [ Attrs.href "https://elm-lang.org/"
                    , Attrs.target "_blank"
                    ]
                    [ Html.text "Elm" ]
                , Html.text " snippet sharing service."
                ]
            , Html.text " It doesn't have any saved Ellies listing or search functionality though, so many cool and interesting Ellies get lost."
            ]
        , Html.p []
            [ Html.text "This app aims to change that. When you find an interesting Ellie, click "
            , Html.node "bookmarklet-link"
                [ Attrs.attribute "src" bookmarkletSource
                , Attrs.attribute "title" "this bookmarklet"
                ]
                []
            , Html.text " to save it here!"
            ]
        , Html.ul []
            [ Html.li []
                [ Html.a
                    [ Attrs.href screencastUrl
                    , Attrs.target "_blank"
                    ]
                    [ Html.text "Screencast (655K, .mp4)" ]
                ]
            , Html.li []
                [ Html.a
                    [ Attrs.href "https://github.com/Janiczek/ellies"
                    , Attrs.target "_blank"
                    ]
                    [ Html.text "Source on GitHub" ]
                ]
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
                    [ refreshElliesBtn
                    , Html.input
                        [ Attrs.placeholder "Filter the name..."
                        , Events.onInput SetNameFilter
                        , Attrs.value model.nameFilter
                        ]
                        []
                    , Table.view tableConfig model.tableState filteredEllies
                    ]
        ]
    }
