module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import IorinParser exposing (..)

type alias Model =
    { inputString : String
    , strForCalc : Res Exp
    }

initialModel : Model
initialModel =
    { inputString = ""
    , strForCalc = Failed
    }

type Exp
    = Number Int
    | Add Exp Exp   --<Exp> + <Exp>
    | Sub Exp Exp   --<Exp> - <Exp>
    | Mul Exp Exp   --<Exp> * <Exp>
    | Pow Exp Exp   --<Exp> ^ <Exp>
    | Paren Exp     --(<Exp>)

type Msg
    = Input String
    | Pressed


update : Msg -> Model -> Model
update msg model =
  case msg of
    Input str ->
      { model | inputString = str }

    Pressed ->
      { model | strForCalc = model.inputString
        |> expParser
      }

evaluate : Exp -> Int
evaluate exp =
  case exp of
    Number n -> n
    Add left right -> evaluate left + evaluate right
    Sub left right -> evaluate left - evaluate right
    Mul left right -> evaluate left * evaluate right
    Pow left right -> evaluate left ^ evaluate right
    Paren expression -> evaluate expression

expParser : Parser Exp
expParser = expressionParser

parenParser () =
  concat parenOpenParser
    ( concat expressionParser parenCloseParser
      (\exp _ -> exp)
    )
    (\_ exp -> Paren exp)

numOrParen =
  unitOr parenParser numParser

hatNumParser =
  concat hatParser numOrParen
    (\hat exp ->(\left -> hat left exp) )
hatNumListParser =
  zeroOrMore hatNumParser

powParser =
  concat numOrParen hatNumListParser
    (\left expList ->
      expList
        |> List.foldl (\exp result -> exp result) left
    )

starNumParser =
  concat starParser powParser
    (\star exp ->(\left -> star left exp) )
starNumListParser =
  zeroOrMore starNumParser

mulParser =
  concat powParser starNumListParser
    (\left expList ->
      expList
        |> List.foldl (\exp result -> exp result) left
    )

plusMinusParser =
  or plusParser minusParser
addNumParser =
  concat plusMinusParser mulParser
    (\addOrSub exp ->(\left -> addOrSub left exp) )
addNumListParser =
  zeroOrMore addNumParser

expressionParser =
  concat mulParser addNumListParser
  (\left expList ->
    expList
      |> List.foldl (\exp result -> exp result) left
  )

numParser : Parser Exp
numParser =
  oneOrMore digitParser
    |> map (String.concat >> String.toInt)
    |> fmap (\ maybeInt tl ->
      case maybeInt of
        Just n -> Success (Number n) tl
        Nothing -> Failed
    )

digitParser : Parser String
digitParser =
  char Char.isDigit
    |> map String.fromChar

plusParser : Parser (Exp -> Exp -> Exp)
plusParser =
  char ((==) '+')
    |> map (always Add)

minusParser : Parser (Exp -> Exp -> Exp)
minusParser =
  char ((==) '-')
    |> map (always Sub)

starParser : Parser (Exp -> Exp -> Exp)
starParser =
  char ((==) '*')
    |> map (always Mul)

hatParser : Parser (Exp -> Exp -> Exp)
hatParser =
  char ((==) '^')
    |> map (always Pow)

parenOpenParser : Parser ()
parenOpenParser =
  char ((==) '(')
    |> map (always ())

parenCloseParser : Parser ()
parenCloseParser =
  char ((==) ')')
    |> map (always ())

view : Model -> Html Msg
view model =
  div []
    [ input
      [ type_ "textbox"
      , onInput Input
      ][]
    , button [ onClick Pressed ][ text "Parse it !" ]
    , br[][]
    , text <| resultToString model.strForCalc
    , br[][]
    , text <| resultToEvaluatedString model.strForCalc
    ]

resultToEvaluatedString : Res Exp -> String
resultToEvaluatedString result =
  case result of
    Success exp tl ->
      exp
        |> evaluate
        |> String.fromInt
    _ -> "Error"

resultToString : Res Exp -> String
resultToString result =
  case result of
    Success exp tl -> expToString exp
    Failed -> "Error"

expToString : Exp -> String
expToString exp =
  case exp of
    Number n -> String.fromInt n
    Add left right ->
      "(Add " ++ (expToString left) ++ " " ++ (expToString right) ++ ")"
    Sub left right ->
      "(Sub " ++ (expToString left) ++ " " ++ (expToString right) ++ ")"
    Mul left right ->
      "(Mul " ++ (expToString left) ++ " " ++ (expToString right) ++ ")"
    Pow left right ->
      "(Pow " ++ (expToString left) ++ " " ++ (expToString right) ++ ")"
    Paren expression ->
      "( " ++ expToString expression ++ " )"

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
