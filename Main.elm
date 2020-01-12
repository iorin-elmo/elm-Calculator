module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import IorinParser exposing (..)

import Debug exposing (log)

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
    | Let Variable Exp Exp-- let x=2 in x*4
    | Var Variable
    | If Exp Exp Exp
    | GreaterThan Exp Exp
    | LessThan Exp Exp
    | Equal Exp Exp


type alias Variable = String

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



evaluate : Dict Variable Int -> Exp -> Int
evaluate dict exp =
  case exp of
    Number n -> n
    Add left right -> evaluate dict left + evaluate dict right
    Sub left right -> evaluate dict left - evaluate dict right
    Mul left right -> evaluate dict left * evaluate dict right
    Pow left right -> evaluate dict left ^ evaluate dict right
    Paren expression -> evaluate dict expression
    Let var e1 e2 -> evaluate (Dict.insert var (evaluate dict e1) dict) e2
    Var variable ->
      let
        maybeInt = Dict.get variable dict
      in
        case maybeInt of
          Just e -> e
          Nothing -> 0
    If cond t f ->
      if evaluate dict cond /= 0
      then evaluate dict t
      else evaluate dict f
    GreaterThan left right ->
      if evaluate dict left > evaluate dict right
      then 1
      else 0
    LessThan left right ->
      if evaluate dict left < evaluate dict right
      then 1
      else 0
    Equal left right ->
      if evaluate dict left == evaluate dict right
      then 1
      else 0

expParser : Parser Exp
expParser = expressionParser

zeroOrMoreSpaceParser =
  zeroOrMore (charMatch ' ')
    |> map (always ())

oneOrMoreSpaceParser =
  concat
    (charMatch ' ') zeroOrMoreSpaceParser
    (\_ _ -> ())

ifParser =
  intersperceConcat oneOrMoreSpaceParser
    (
      intersperceConcat3 oneOrMoreSpaceParser
        (string "if") conditionParser (string "then")
        (\_ cond _ -> cond)
    )
    (
      intersperceConcat3 oneOrMoreSpaceParser
        expressionParser (string "else") expressionParser
        (\t _ f -> ( t, f ))
    )
    (\cond ( t, f ) -> If cond t f)

conditionParser : Parser Exp
conditionParser =
  intersperceConcat3 zeroOrMoreSpaceParser
    expressionParser comparatorParser expressionParser
    (\left compare right -> compare left right)

comparatorParser : Parser (Exp -> Exp -> Exp)
comparatorParser =
  choice [gtParser,ltParser,eqParser]


varDecl =
  intersperceConcat3 zeroOrMoreSpaceParser
    variableParser (charMatch '=') expressionParser
    (\var _ exp -> ( var, exp ))

letParser =
  intersperceConcat4 oneOrMoreSpaceParser
    (string "let") varDecl (string "in") expressionParser
    (\_ ( var, e1 ) _ e2 -> Let var e1 e2)

variableParser : Parser String
variableParser =
  concat
    (char Char.isAlpha) (zeroOrMore (char (\c->(Char.isAlphaNum c)||(c=='_') )))
    (\c list ->
      c :: list
        |> String.fromList
    )
    |> fmap
      (\str ->
        case str of
          "let" -> fail
          "in"  -> fail
          _     -> return str
      )

parenParser : Parser Exp
parenParser =
  intersperceConcat3 zeroOrMoreSpaceParser
    parenOpenParser expressionParser parenCloseParser
    (\_ exp _ -> Paren exp)

loopables () =
  choice [parenParser,letParser,ifParser]

numOrLoopables =
  unitChoice loopables [numParser,(variableParser |> map Var)]

hatNumParser =
  intersperceConcat zeroOrMoreSpaceParser
    hatParser numOrLoopables
    (\hat exp ->(\left -> hat left exp) )

hatNumListParser =
  zeroOrMore
    ( concat
      zeroOrMoreSpaceParser
      hatNumParser
      (\_ exp->exp)
    )

powParser =
  concat
    numOrLoopables hatNumListParser
    (\left expList ->
      expList
        |> List.foldl (\exp result -> exp result) left
    )

starNumParser =
  intersperceConcat zeroOrMoreSpaceParser
    starParser powParser
    (\star exp ->(\left -> star left exp) )

starNumListParser =
  zeroOrMore
    ( concat
      zeroOrMoreSpaceParser
      starNumParser
      (\_ exp->exp)
    )

mulParser =
  concat
    powParser starNumListParser
    (\left expList ->
      expList
        |> List.foldl (\exp result -> exp result) left
    )

plusMinusParser =
  or plusParser minusParser
addNumParser =
  intersperceConcat zeroOrMoreSpaceParser
    plusMinusParser mulParser
    (\addOrSub exp ->(\left -> addOrSub left exp) )

addNumListParser =
  zeroOrMore
    ( concat
      zeroOrMoreSpaceParser
      addNumParser
      (\_ exp->exp)
    )

expressionParser =
  concat
    mulParser addNumListParser
  (\left expList ->
    expList
      |> List.foldl (\exp result -> exp result) left
  )

numParser : Parser Exp
numParser =
  oneOrMore digitParser
    |> map (String.concat >> String.toInt)
    |> fmap (\ maybeInt ->
      case maybeInt of
        Just n -> return (Number n)
        Nothing -> fail
    )

digitParser : Parser String
digitParser =
  char Char.isDigit
    |> map String.fromChar

plusParser : Parser (Exp -> Exp -> Exp)
plusParser =
  charMatch '+'
    |> map (always Add)

minusParser : Parser (Exp -> Exp -> Exp)
minusParser =
  charMatch '-'
    |> map (always Sub)

starParser : Parser (Exp -> Exp -> Exp)
starParser =
  charMatch '*'
    |> map (always Mul)

hatParser : Parser (Exp -> Exp -> Exp)
hatParser =
  charMatch '^'
    |> map (always Pow)

parenOpenParser : Parser ()
parenOpenParser =
  charMatch '('

parenCloseParser : Parser ()
parenCloseParser =
  charMatch ')'

commaParser : Parser ()
commaParser =
  charMatch ','

gtParser : Parser (Exp -> Exp -> Exp)
gtParser =
  charMatch '>'
    |> map (always GreaterThan)

ltParser : Parser (Exp -> Exp -> Exp)
ltParser =
  charMatch '<'
    |> map (always LessThan)

eqParser : Parser (Exp -> Exp -> Exp)
eqParser =
  charMatch '='
    |> map (always Equal)


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
    , text <| resultToEvaluatedString model
    ]

resultToEvaluatedString : Model -> String
resultToEvaluatedString model =
  case model.strForCalc of
    Success exp tl ->
      exp
        |> evaluate Dict.empty
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
      "Add( " ++ (expToString left) ++ ", " ++ (expToString right) ++ " )"
    Sub left right ->
      "Sub( " ++ (expToString left) ++ ", " ++ (expToString right) ++ " )"
    Mul left right ->
      "Mul( " ++ (expToString left) ++ ", " ++ (expToString right) ++ " )"
    Pow left right ->
      "Pow( " ++ (expToString left) ++ ", " ++ (expToString right) ++ " )"
    Paren expression ->
      "( " ++ expToString expression ++ " )"
    Let var e1 e2 ->
      "Let( " ++ var ++ " = " ++ expToString e1 ++ " In " ++ expToString e2 ++ " )"
    Var var ->
      "Var("++var++")"
    If cond t f ->
      "If "++expToString cond++" Then "++expToString t++" Else "++expToString f
    GreaterThan left right ->
      expToString left++" > "++expToString right
    LessThan left right ->
      expToString left++" < "++expToString right
    Equal left right ->
      expToString left++" = "++expToString right



main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }