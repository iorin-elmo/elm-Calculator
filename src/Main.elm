module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import IorinParser exposing (..)

import Debug exposing (log)

type alias Model =
  { inputString : String
  , strForCalc : Term
  , result : Term
  , types : Result String Type
  }

initialModel : Model
initialModel =
  { inputString = ""
  , strForCalc = LambdaVar -1 TypeBool
  , result = LambdaVar -1 TypeBool
  , types = Err "***"
  }

--<term>::= <var>
--        | \ <var> : <type> -> <term>
--        | <term> <term>
--        | true
--        | false
--        | if <term> then <term> else <term>
--        | <int>
--        | <term> <binaryOp> <term>
--        | <term> <compOp> <term>

--<type>::= <bool>
--        | <int>
--        | <type> -> <type>

--<binaryOp>::= + | - | * | ^
--<compOp>  ::= > | < | =


type Term
  = LambdaVar Int Type
  | LambdaAbs Variable Type Term
  | LambdaApp Term Term
  | TermTrue
  | TermFalse
  | TermIf Term Term Term
  | TermInt Int
  | Calc BinaryOp Term Term
  | Compare CompOp Term Term

type CompOp
  = GreaterThan
  | LessThan
  | Equal

type BinaryOp
  = Add
  | Sub
  | Mul
  | Pow

type Type
  = TypeBool
  | TypeInt
  | TypeFunction Type Type

type alias Variable = String
type alias NamingContext = List Variable
type alias TypeContext = List ( Variable, Type )

type Msg
  = Input String
  | Pressed

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input str ->
      { model | inputString = str }

    Pressed ->
      let
        parseRes =
          model.inputString
            |> String.toLower
            |> termParser []
            |> (\res ->
                case res of
                  Success hd tl -> hd
                  _ -> LambdaVar -1 TypeBool
              )
      in
        { model |
          strForCalc = parseRes
        , result = parseRes |> evaluate
        , types = parseRes |> typeOf []
          --|> expParser
        }

evaluate : Term -> Term
evaluate exp =
  if isValue exp
  then exp
  else evaluate (reduction exp)

isValue : Term -> Bool
isValue exp =
  case exp of
    TermTrue -> True
    TermFalse-> True
    TermInt _-> True
    LambdaAbs _ _ _ -> True
    LambdaVar n _ -> if n == -1 then True else False
    _ -> False

termToInt : Term -> Int
termToInt t =
  case t of
    TermInt n -> n
    _ -> 0

reduction : Term -> Term
reduction exp =
  case exp of
    LambdaApp left right ->
      case left of
        LambdaAbs _ _ e ->
          let
            test = replace 0 right e
            debug = log "exp" (termToString test)
          in
            test
        LambdaApp _ _ -> LambdaApp (reduction left) right
        _ -> exp
    TermIf cond t1 t2 ->
      if (evaluate cond == TermTrue)
      then t1
      else t2
    Calc op t1 t2 ->
      let
        i1 = t1 |> evaluate |> termToInt
        i2 = t2 |> evaluate |> termToInt
      in
        case op of
          Add -> TermInt (i1+i2)
          Sub -> TermInt (i1-i2)
          Mul -> TermInt (i1*i2)
          Pow -> TermInt (i1^i2)
    Compare op t1 t2 ->
      let
        i1 = t1 |> evaluate |> termToInt
        i2 = t2 |> evaluate |> termToInt
        boolToTerm bool =
          if bool
          then TermTrue
          else TermFalse
      in
        case op of
          GreaterThan -> boolToTerm (i1 > i2)
          LessThan    -> boolToTerm (i1 < i2)
          Equal       -> boolToTerm (i1 ==i2)
    _ -> exp |> log "exp"

--    (\t-> t^2) (x+2)
-- == replace 0 (x+2) x^2
-- == (t+2)^2
replace : Int -> Term -> Term -> Term
replace from to target =
  case target of
    LambdaVar n t ->
      if n == from
      then to
      else LambdaVar n t
    LambdaApp left right ->
      LambdaApp (replace from to left) (replace from to right)
    LambdaAbs var ty e ->
      LambdaAbs var ty (replace (from+1) to e )
    TermIf cond t1 t2 ->
      TermIf (replace from to cond) (replace from to t1) (replace from to t2)
    Calc op t1 t2 ->
      Calc op (replace from to t1) (replace from to t2)
    Compare op t1 t2 ->
      Compare op (replace from to t1) (replace from to t2)
    _ -> target

typeOf : TypeContext -> Term -> Result String Type
typeOf list term =
  case term of
    TermTrue -> Ok TypeBool
    TermFalse -> Ok TypeBool
    TermInt _ -> Ok TypeInt
    Calc _ t1 t2 ->
      let
        typet1 = typeOf list t1
        typet2 = typeOf list t2
      in
        if (typet1 == Ok TypeInt)&&(typet2 == Ok TypeInt)
        then Ok TypeInt
        else Err "Calc Argument is wrong"
    Compare _ t1 t2 ->
      let
        typet1 = typeOf list t1
        typet2 = typeOf list t2
      in
        if (typet1 == Ok TypeInt)&&(typet2 == Ok TypeInt)
        then Ok TypeBool
        else Err "Compare Argument is wrong"
    TermIf cond t1 t2 ->
      let
        typet1 = typeOf list t1
        typet2 = typeOf list t2
      in
        if ((typeOf list cond) == (Ok TypeBool))
        then
          case (typet1,typet2) of
            (Ok ty1,Ok ty2) ->
              if ty1 == ty2
              then Ok ty1
              else Err "If term has different types"
            _ -> Err "If Term is wrong"
        else
          Err "If Condition is wrong"
    LambdaVar _ ty -> Ok ty
    LambdaAbs _ ty t ->
      Result.map (TypeFunction ty) (typeOf list t)
    LambdaApp t1 t2 ->
      case typeOf list t1 of
        Ok (TypeFunction arg ret) ->
          if Ok arg == typeOf list t2
          then Ok ret
          else Err "Argument of LambdaApply is wrong(2)"
        _ -> Err "Argument of LambdaApply is wrong(1)"

typeToString : Result String Type -> String
typeToString ty =
  case ty of
    Ok (TypeFunction arg ret) ->
      "(" ++ typeToString (Ok arg) ++ "->" ++ typeToString (Ok ret) ++ ")"
    Ok TypeBool ->
      "Bool"
    Ok TypeInt ->
      "Int"
    Err str -> str

termParser : TypeContext -> Parser Term
termParser list =
  choice
    [ lambdaAbsParser list
    , termIfParser list
    , calcCompParser list
    , lambdaAppParser list
    , strongerThanApp list
    ]

-- TERM PARSER

lambdaAbsParser : TypeContext -> Parser Term
lambdaAbsParser list =
  concat
    (
      intersperseConcat4 zeroOrMoreSpaceParser
        bsParser variableParser typeAnnotationParser (string "->")
        (\_ var ty _ -> ( var, ty ):: list )
    )
    zeroOrMoreSpaceParser
    (\varList _ -> varList)
  |> fmap
    (\varList ->
      termParser varList
        |> fmap
          (\exp ->
            case varList of
              [] -> fail
              ( hdVar, hdType ) :: tl ->
                return (LambdaAbs hdVar hdType exp)
          )
    )
  |> pLog "lambdaAbs"

termIfParser : TypeContext -> Parser Term
termIfParser list =
  intersperseConcat4 oneOrMoreSpaceParser
    (string "if") (calcCompParser list)
    (string "then")
    (intersperseConcat3 oneOrMoreSpaceParser
      (calcCompParser list)
      (string "else")
      (calcCompParser list)
      (\t1 _ t2 -> (t1,t2))
    )
    (\_ c _ (t1,t2) -> TermIf c t1 t2)
  |> pLog "termIf"

calcCompParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      comparatorParser
      (\_ x -> x)
    )
    (calcIntParser list)

strongerThanApp list =
  choice
    [ lazy (\() -> termParenParser list)
    , boolParser
    , intParser
    , lambdaVarParser list
    ]

lambdaAppParser : TypeContext -> Parser Term
lambdaAppParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      (return LambdaApp)
      (\_ x -> x)
    )
    (strongerThanApp list)
  |> pLog "lambdaApp"

-- STRONGER THAN APPLY

termParenParser : TypeContext -> Parser Term
termParenParser list =
  intersperseConcat3 zeroOrMoreSpaceParser
    parenOpenParser (termParser list) parenCloseParser
    (\_ exp _ -> exp)

boolParser : Parser Term
boolParser =
  or
    (string "true" |> map (always TermTrue))
    (string "false" |> map (always TermFalse))

intParser : Parser Term
intParser =
  oneOrMore digitParser
    |> map (String.concat >> String.toInt)
    |> fmap (\ maybeInt ->
      case maybeInt of
        Just n -> return (TermInt n)
        Nothing -> fail
    )
  |> pLog "int"

lambdaVarParser : TypeContext -> Parser Term
lambdaVarParser list =
  variableParser
    |> map (\var -> varSearch var list 0)
    |> fmap
      (\maybeTuple ->
        case maybeTuple of
          Just (n,ty) -> return (LambdaVar n ty)
          _ -> fail
      )
    |> pLog "LambdaVar"



typeAnnotationParser : Parser Type
typeAnnotationParser =
  intersperseConcat zeroOrMoreSpaceParser
    (charMatch ':') (typeParser ())
    (\_ ty -> ty)

typeParser : () -> Parser Type
typeParser =
  (\()->
    intersperseConcat zeroOrMoreSpaceParser
      (
        choice
          [ lazy
            (\() ->
              (intersperseConcat3 zeroOrMoreSpaceParser
                parenOpenParser (lazy typeParser) parenCloseParser
                (\_ ty _ -> ty)
              )
            )
          , (string "bool" |> map (always TypeBool))
          , (string "int"  |> map (always TypeInt ))
          ]

      )
      (zeroOrMore (lazy arrowAndTypeParser))
      (\ty tylist ->
        List.foldl (\a b ->a b) ty tylist
      )
    |> pLog "typeParser"
  )

arrowAndTypeParser : () -> Parser (Type -> Type)
arrowAndTypeParser =
  (\()->
    intersperseConcat zeroOrMoreSpaceParser
      (string "->") (lazy typeParser)
      (\_ ty -> (\arg -> TypeFunction arg ty))
  )

varSearch : Variable -> TypeContext -> Int -> Maybe ( Int, Type )
varSearch var list cnt =
  case list of
    [] -> Nothing
    (hd,ty) :: tl ->
      if hd == var
      then (cnt, ty) |> Just
      else varSearch var tl (cnt+1)

termToString : Term -> NamingContext -> String
termToString term list =
  case term of
    LambdaVar n _ ->
      if n == -1
      then "Something is wrong"
      else getName n (list |> log "varlist")
    LambdaAbs var _ exp ->
      let
        newVar = getNewVar var list
      in
        "(\\ "++newVar++" ->"++(termToString exp (newVar::list) )++")"
    LambdaApp left right ->
      "("++(termToString left list)++" "++(termToString right list)++")"
    TermTrue -> "True"
    TermFalse-> "False"
    TermInt i -> String.fromInt i
    Calc op t1 t2 ->
      let
        t1Str = termToString t1 list
        t2Str = termToString t2 list
      in
        case op of
          Add -> "(Add "++t1Str++", "++t2Str++")"
          Sub -> "(Sub "++t1Str++", "++t2Str++")"
          Mul -> "(Mul "++t1Str++", "++t2Str++")"
          Pow -> "(Pow "++t1Str++", "++t2Str++")"
    Compare op t1 t2 ->
      let
        t1Str = termToString t1 list
        t2Str = termToString t2 list
      in
        case op of
          GreaterThan -> "("++t1Str++">"++t2Str++")"
          LessThan    -> "("++t1Str++"<"++t2Str++")"
          Equal       -> "("++t1Str++"="++t2Str++")"
    TermIf cond t1 t2 ->
      "If "++
      (termToString cond list)++
      " Then "++
      (termToString t1 list)++
      " Else "++
      (termToString t2 list)

getNewVar : Variable -> NamingContext -> Variable
getNewVar var list =
  if List.member var list
  then getNewVar (var++"'") list
  else var

getName : Int -> NamingContext -> String
getName n list =
  let
    getNameHelper =
      case n of
        0 -> List.head list
        _ ->
          case list of
            [] -> Nothing
            hd :: tl ->
              getName (n-1) tl |> Just
  in
    case getNameHelper of
      Just str -> str
      _ -> "Parse Error"

zeroOrMoreSpaceParser =
  zeroOrMore (charMatch ' ')
    |> map (always ())

oneOrMoreSpaceParser =
  concat
    (charMatch ' ') zeroOrMoreSpaceParser
    (\_ _ -> ())

comparatorParser : Parser (Term->Term->Term)
comparatorParser =
  choice [gtParser,ltParser,eqParser]

{-
varDecl =
  intersperseConcat3 zeroOrMoreSpaceParser
    variableParser (charMatch '=') expressionParser
    (\var _ exp -> ( var, exp ))

letParser =
  intersperseConcat4 oneOrMoreSpaceParser
    (string "let") varDecl (string "in") expressionParser
    (\_ ( var, e1 ) _ e2 -> Let var e1 e2)
-}
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
          "if"  -> fail
          "then"-> fail
          "else"-> fail
          "bool"-> fail
          "int" -> fail
          _     -> return str
      )

powParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      hatParser
      (\_ h -> h)
    )
    (lambdaAppParser list)
  |> pLog "powParser"

mulParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      starParser
      (\_ s -> s)
    )
    (powParser list)
  |> pLog "mulParser"

plusMinusParser =
  or plusParser minusParser

calcIntParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      plusMinusParser
      (\_ x -> x)
    )
    (mulParser list)

digitParser : Parser String
digitParser =
  char Char.isDigit
    |> map String.fromChar

plusParser : Parser (Term->Term->Term)
plusParser =
  charMatch '+'
    |> map (always <| Calc Add)

minusParser : Parser (Term->Term->Term)
minusParser =
  charMatch '-'
    |> map (always <| Calc Sub)

starParser : Parser (Term->Term->Term)
starParser =
  charMatch '*'
    |> map (always <| Calc Mul)

hatParser : Parser (Term->Term->Term)
hatParser =
  charMatch '^'
    |> map (always <| Calc Pow)


parenOpenParser : Parser ()
parenOpenParser =
  charMatch '('

parenCloseParser : Parser ()
parenCloseParser =
  charMatch ')'

commaParser : Parser ()
commaParser =
  charMatch ','

gtParser : Parser (Term->Term->Term)
gtParser =
  charMatch '>'
    |> map (always <| Compare GreaterThan)

ltParser : Parser (Term->Term->Term)
ltParser =
  charMatch '<'
    |> map (always <| Compare LessThan)

eqParser : Parser (Term->Term->Term)
eqParser =
  charMatch '='
    |> map (always <| Compare Equal)

bsParser =
  charMatch '\\'


view : Model -> Html Msg
view model =
  let
    lambdaStr = termToString model.result []
  in
    div []
      [ input
        [ type_ "textbox"
        , onInput Input
        ][]
      , button [ onClick Pressed ][ text "Parse it !" ]
      , br[][]
      --, text <| resultToString model.strForCalc
      , text <| termToString model.strForCalc []
      , br[][]
      , text <|
        case model.types of
          Ok _ -> termToString model.result []
          Err _ -> "Type Mismatch"

      --, text <| resultToEvaluatedString model
      , br[][]
      , text <|
        case lambdaStr of
          "Something is wrong" -> "Parse Error"
          "Parse Error" -> "Parse Error"
          _ -> typeToString model.types
      ]


main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
