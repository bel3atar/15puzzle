module Main exposing (..)

import Array
import Board exposing (..)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HtmlAttributes exposing (css, step, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Random exposing (generate)



---- MODEL ----


type alias Model =
    { board : Board }


defaultSize : Int
defaultSize =
    3


init : ( Model, Cmd Msg )
init =
    ( { board = Board.new defaultSize }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | GotBoard Board
    | ShuffleButtonClicked
    | SizeSliderChanged String
    | TileClicked Tile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleButtonClicked ->
            ( model, Random.generate GotBoard <| Board.random model.board.size )

        TileClicked t ->
            ( { model | board = Board.move t model.board }, Cmd.none )

        GotBoard b ->
            ( { model | board = b }, Cmd.none )

        SizeSliderChanged size ->
            case String.toInt size of
                Just s ->
                    ( model, Random.generate GotBoard <| Board.random s )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        boardSizeString =
            String.fromInt model.board.size
    in
    div []
        [ div []
            [ div []
                [ span [] [ text boardSizeString ]
                , input
                    [ value boardSizeString
                    , type_ "range"
                    , HtmlAttributes.max "8"
                    , HtmlAttributes.min "3"
                    , step "1"
                    , onInput SizeSliderChanged
                    ]
                    []
                ]
            , button [ onClick ShuffleButtonClicked ] [ text "Shuffle" ]
            ]
        , viewBoard model.board
        ]


tileSize =
    100


viewBoard : Board -> Html Msg
viewBoard { size, tiles } =
    let
        viewTile : Tile -> Html Msg
        viewTile n =
            div
                [ css
                    [ width <| px tileSize
                    , height <| px tileSize
                    , displayFlex
                    , alignItems center
                    , fontSize <| px 32
                    , justifyContent center
                    , boxSizing borderBox
                    , border3 (px 2) solid (hex "000")
                    ]
                , onClick <| TileClicked n
                ]
                [ text <|
                    if n == 0 then
                        ""

                    else
                        String.fromInt n
                ]
    in
    div
        [ css
            [ displayFlex
            , width << px << toFloat <| tileSize * size
            , flexWrap wrap
            ]
        ]
    <|
        List.map viewTile tiles



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = \model -> { title = "", body = [ view model |> toUnstyled ] }
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
