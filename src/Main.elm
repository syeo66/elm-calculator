import Browser
import Html exposing(Html, div, button, text)
import Html.Attributes as Attr exposing (id, class)
import Html.Events exposing (onClick)

main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type Mode
    = Input
    | Decimals
    | Addition
    | Subtraction
    | Division
    | Multiplication

type alias Model =
    { display : String
    , clearDisplay : Bool
    , input : Float
    , buffer : Float
    , calcUpdatesBuffer : Bool
    , mode : Mode
    }

init : Model 
init =
    { display = "0"
    , clearDisplay = True
    , input = 0.0
    , buffer = 0.0
    , calcUpdatesBuffer = True
    , mode = Input
    }

-- UPDATE

type Msg
    = Number Float
    | Dot
    | Add
    | Sub 
    | SubRev
    | Mult 
    | Div 
    | DivRev
    | Clear
    | ClearEntry
    | Calculate

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Number n ->
            { model 
            | display = 
                if model.clearDisplay
                then String.fromFloat n
                else model.display ++ (String.fromFloat n)
            , clearDisplay = n == 0 && model.display == "0"
            , calcUpdatesBuffer = True
            }

        Dot ->
            { model
            | mode = Decimals
            , clearDisplay = False
            , display = 
                -- only add '.' if we have not one yet
                if model.mode == Decimals 
                then model.display
                else if model.clearDisplay 
                    then "0."
                    else model.display ++ "."
            }

        Add ->
            calculationMode Addition model

        Sub ->
            calculationMode Subtraction model

        Div ->
            calculationMode Division model
    
        Mult ->
            calculationMode Multiplication model
        
        Clear ->
            init

        ClearEntry ->
            { model 
            | display = "0"
            , clearDisplay = True
            }
        
        Calculate ->
            case model.mode of 
                Addition ->
                    calculateModel Add model

                Subtraction ->
                    if model.calcUpdatesBuffer
                    then calculateModel Sub model
                    else calculateModel SubRev model

                Multiplication ->
                    calculateModel Mult model

                Division -> 
                    if model.calcUpdatesBuffer
                    then calculateModel Div model
                    else calculateModel DivRev model

                other ->
                    model
        
        other ->
            model

calculationMode : Mode -> Model -> Model
calculationMode mode model =
    { model 
    | mode = mode
    , clearDisplay = True
    , buffer = 
        Maybe.withDefault 0 (String.toFloat model.display)
    }

calculateModel : Msg -> Model -> Model 
calculateModel msg model =
    { model 
    | display = 
        calculateToDisplay msg model.buffer model.display
    , clearDisplay = True
    , calcUpdatesBuffer = False
    , buffer = 
        if model.calcUpdatesBuffer
        then Maybe.withDefault 0 (String.toFloat model.display)
        else model.buffer
    }

calculateToDisplay : Msg -> Float -> String -> String
calculateToDisplay msg a b =
    String.fromFloat (calculateStringToFloat msg a b)

calculateStringToFloat : Msg -> Float -> String -> Float
calculateStringToFloat msg a b =
    calculate msg a (Maybe.withDefault 0 (String.toFloat b))

calculate : Msg -> Float -> Float -> Float
calculate msg a b =
    case msg of 
        Add ->
            a + b

        Sub ->
            a - b

        SubRev ->
            b - a
        
        Mult ->
            a * b

        Div ->
            a / b

        DivRev ->
            b / a
        
        other ->
            0

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [ id "display" ] [ text model.display ]
        , div [] 
            [ button [ onClick (Number 7) ] [ text "7" ]
            , button [ onClick (Number 8) ] [ text "8" ]
            , button [ onClick (Number 9) ] [ text "9" ]
            , button [ onClick (Div) ] [ text "/" ]
            , button [ onClick (Clear) ] [ text "ON" ]
            ]
        , div [] 
            [ button [ onClick (Number 4) ] [ text "4" ]
            , button [ onClick (Number 5) ] [ text "5" ]
            , button [ onClick (Number 6) ] [ text "6" ]
            , button [ onClick (Mult) ] [ text "*" ]
            , button [ onClick (Clear) ] [ text "C" ]
            ]
        , div [] 
            [ button [ onClick (Number 1) ] [ text "1" ]
            , button [ onClick (Number 2) ] [ text "2" ]
            , button [ onClick (Number 3) ] [ text "3" ]
            , button [ onClick (Sub) ] [ text "-" ]
            , button [ onClick (ClearEntry) ] [ text "CE" ]
            ]
        , div [] 
            [ button 
                [ class "zero"
                , onClick (Number 0)
                ] 
                [ text "0" ]
            , button [ onClick (Dot) ] [ text "." ]
            , button [ onClick (Add) ] [ text "+" ]
            , button 
                [ class "calculate"
                , onClick (Calculate)
                ] 
                [ text "=" ]
            ]
        ]