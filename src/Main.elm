module Main exposing (..)

import Browser
import Html exposing (a, article, button, div, h1, h2, img, p, span, text)
import Html.Attributes exposing (alt, class, href, rel, src, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import RemoteData exposing (RemoteData(..))
import String exposing (toUpper)


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
    ( { products = Loading }
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
    | Retry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts remoteData ->
            ( { model | products = remoteData }, Cmd.none )

        Retry ->
            ( { model | products = Loading }, fetchProducts )


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


productImageUrl : Product -> String
productImageUrl product =
    "https://bellroy-product-images.imgix.net/bellroy_dot_com_range_page_image/"
        ++ toUpper product.price.currencyCode
        ++ "/"
        ++ product.sku
        ++ "/0?auto=format&fit=max&w=160"


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Bellroy Products" ]
        , case model.products of
            NotAsked ->
                p [] [ text "Products not requested yet." ]

            Loading ->
                div [ class "loading" ]
                    [ span [ class "spinner" ] []
                    , text " Loading products..."
                    ]

            Failure err ->
                article [ class "error" ]
                    [ p [] [ text ("Error: " ++ httpErrorToString err) ]
                    , button [ onClick Retry ] [ text "Retry" ]
                    ]

            Success products ->
                if List.isEmpty products then
                    p [] [ text "No products found." ]

                else
                    div [ class "grid" ]
                        (List.map
                            (\product ->
                                a
                                    [ href ("https://bellroy.com/" ++ product.canonicalUri)
                                    , target "_blank"
                                    , rel "noopener noreferrer"
                                    , class "card-link"
                                    ]
                                    [ article [ class "card" ]
                                        [ img
                                            [ src (productImageUrl product)
                                            , alt ("Image of " ++ product.name)
                                            , class "product-image"
                                            ]
                                            []
                                        , h2 [] [ text product.name ]
                                        , p [] [ text ("SKU: " ++ product.sku) ]
                                        , p [] [ text ("Price: " ++ toUpper product.price.currencyCode ++ " $" ++ String.fromFloat (toFloat product.price.priceInCents / 100)) ]
                                        ]
                                    ]
                            )
                            products
                        )
        ]
