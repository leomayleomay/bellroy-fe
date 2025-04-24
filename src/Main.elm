module Main exposing (..)

import Browser
import Html exposing (div, text)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import RemoteData exposing (RemoteData(..))


type alias Price =
    { channel : String
    , currencyCode : String
    , priceInCents : Int
    , priceRoleIdentifier : String
    }


priceDecoder : Decoder Price
priceDecoder =
    Decode.map4 Price
        (field "channel" string)
        (field "currency_code" string)
        (field "price_in_cents" int)
        (field "price_role_identifier" string)


type alias Dimensions =
    { color : List String
    , material : List String
    , size : List String
    , productType : List String
    , productSubType : List String
    , webCollection : List String
    , webPrimarySubCategory : String
    , netWeightG : String
    , productVolumeMl : String
    , warrantyYears : String
    }


type alias Product =
    { id : String
    , name : String
    , sku : String
    , barcode : String
    , canonicalUri : String
    , price : Price
    }


productDecoder : Decoder Product
productDecoder =
    Decode.map6 Product
        (field "id" string)
        (field "attributes" (field "name" string))
        (field "attributes" (field "sku" string))
        (field "attributes" (field "barcode" string))
        (field "attributes" (field "canonical_uri" string))
        (field "attributes" (field "price" priceDecoder))


productsDecoder : Decoder (List Product)
productsDecoder =
    field "products" (Decode.list productDecoder)


type alias Model =
    { products : RemoteData Http.Error (List Product) }


init : ( Model, Cmd Msg )
init =
    ( { products = NotAsked }
    , fetchProducts
    )


fetchProducts : Cmd Msg
fetchProducts =
    Http.get
        { url = "https://products.bellroy.com/v2/products?channel=bellroy.com&currency_identifier=24e93ad60e6042c01dce9d12008131fbdb51af44&price_role_identifier=BpxmBaMDPhkYyg82EFytxg==&filter[dimensions][web_collection]=product_spotlight&filter[not][dimensions][searchability]=non-searchable"
        , expect = Http.expectJson (RemoteData.fromResult >> GotProducts) productsDecoder
        }


type Msg
    = GotProducts (RemoteData Http.Error (List Product))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts remoteData ->
            ( { model | products = remoteData }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error, please check your connection"

        Http.BadStatus statusCode ->
            "Server error: HTTP " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Unexpected response: " ++ message


view : Model -> Html.Html Msg
view model =
    case model.products of
        NotAsked ->
            text "Products not requested yet"

        Loading ->
            text "Loading products..."

        Failure err ->
            text ("Error: " ++ httpErrorToString err)

        Success products ->
            div []
                (List.map
                    (\product ->
                        div []
                            [ text product.name
                            , text (" - SKU: " ++ product.sku)
                            , text (" - Price: " ++ product.price.currencyCode ++ " " ++ String.fromFloat (toFloat product.price.priceInCents / 100))
                            ]
                    )
                    products
                )
