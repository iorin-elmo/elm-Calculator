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
--        | let <var> = <term> in <term>

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
  | Let Variable Term Term
  | LetVar Variable

type CompOp
  = GreaterThan
  | LessThan
  | Equal

type BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

type Type
  = TypeBool
  | TypeInt
  | TypeFunction Type Type

type alias Variable = String
type alias NamingContext = List Variable
type alias TypeContext = List ( Variable, Type )
type alias LetVarDict = Dict Variable Term

type WhichVar
  = LambdaVar_ Int Type
  | LetVar_ Variable

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
        types = typeOf Dict.empty [] parseRes
        result =
          case types of
            Ok _ -> evaluate Dict.empty parseRes
            _ -> LambdaVar -1 TypeBool
      in
        { model |
          strForCalc = parseRes
        , result = result
        , types = types
        }

evaluate : LetVarDict -> Term -> Term
evaluate dict exp =
  if isValue exp
  then exp
  else evaluate dict (reduction dict (exp |> log "reduction"))

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

reduction : LetVarDict -> Term -> Term
reduction dict exp =
  case exp of
    LambdaApp left right ->
      case left of
        LambdaAbs _ _ e ->
          let
            test = replace 0 right e
            debug = log "exp" (termToString test)
          in
            test
        LambdaApp _ _ -> LambdaApp (reduction dict left) right
        LetVar v ->
          let
            newL = evaluate dict (Maybe.withDefault (LambdaVar -1 TypeBool) (Dict.get v dict))
          in
            evaluate dict (LambdaApp newL right)
        _ -> exp
    TermIf cond t1 t2 ->
      if (evaluate dict cond == TermTrue)
      then t1
      else t2
    Calc op t1 t2 ->
      let
        i1 = t1 |> evaluate dict |> termToInt
        i2 = t2 |> evaluate dict |> termToInt
      in
        case op of
          Add -> TermInt (i1+i2)
          Sub -> TermInt (i1-i2)
          Mul -> TermInt (i1*i2)
          Div -> TermInt (i1//i2)
          Mod -> TermInt (modBy i2 i1)
          Pow -> TermInt (i1^i2)
    Compare op t1 t2 ->
      let
        i1 = t1 |> evaluate dict |> termToInt
        i2 = t2 |> evaluate dict |> termToInt
        boolToTerm bool =
          if bool
          then TermTrue
          else TermFalse
      in
        case op of
          GreaterThan -> boolToTerm (i1 > i2)
          LessThan    -> boolToTerm (i1 < i2)
          Equal       -> boolToTerm (i1 ==i2)
    LetVar var -> evaluate dict (Maybe.withDefault (LambdaVar -1 TypeBool) (Dict.get var dict))
    Let var t1 t2 ->
      evaluate
      ( Dict.insert
        var
        (evaluate dict t1)
        dict
      )
      t2

    _ -> exp |> log "exp"


--    (\t-> t^2) (x+2)
-- == replace 0 (x+2) t^2
-- == (x+2)^2
replace : Int -> Term -> Term -> Term
replace from to target =
  case target of
    LambdaVar n t ->
      if n == from
      then to
      else LambdaVar n t
    Let var t1 t2 ->
      Let var (replace from to t1) (replace from to t2)
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

typeOf : LetVarDict -> TypeContext -> Term -> Result String Type
typeOf dict list term =
  case term of
    TermTrue -> Ok TypeBool
    TermFalse -> Ok TypeBool
    TermInt _ -> Ok TypeInt
    LetVar v ->
      Maybe.withDefault (Err "Undifined Variable")
        <| Maybe.map (typeOf dict list) (Dict.get v dict)
    Let v t1 t2 -> typeOf (Dict.insert v t1 dict) list t2
    Calc _ t1 t2 ->
      let
        typet1 = typeOf dict list t1
        typet2 = typeOf dict list t2
      in
        if (typet1 == Ok TypeInt)&&(typet2 == Ok TypeInt)
        then Ok TypeInt
        else Err "Calc Argument is wrong"
    Compare _ t1 t2 ->
      let
        typet1 = typeOf dict list t1
        typet2 = typeOf dict list t2
      in
        if (typet1 == Ok TypeInt)&&(typet2 == Ok TypeInt)
        then Ok TypeBool
        else Err "Compare Argument is wrong"
    TermIf cond t1 t2 ->
      let
        typet1 = typeOf dict list t1
        typet2 = typeOf dict list t2
      in
        if ((typeOf dict list cond) == (Ok TypeBool))
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
      Result.map (TypeFunction ty) (typeOf dict list t)
    LambdaApp t1 t2 ->
      case typeOf dict list t1 of
        Ok (TypeFunction arg ret) ->
          if Ok arg == typeOf dict list t2
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
    , letParser list
    , termIfParser list
    , calcCompParser list
    , lambdaAppParser list
    , strongerThanApp list
    ]

-- TERM PARSER
letParser : TypeContext -> Parser Term
letParser list =
  let
    varDecl =
      intersperseConcat3 zeroOrMoreSpaceParser
      variableParser (charMatch '=') (lazy (\()->termParser list))
      (\var _ t -> (var,t) )
  in
    intersperseConcat4 oneOrMoreSpaceParser
    (string "let") varDecl (string "in") (lazy (\()->termParser list))
    (\_ (var,t1) _ t2 -> Let var t1 t2)


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
    , varParser list
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

varParser : TypeContext -> Parser Term
varParser list =
  variableParser
    |> map (\var -> varSearch var list 0)
    |> fmap
      (\whichVar ->
        case whichVar of
          LambdaVar_ n ty -> return (LambdaVar n ty)
          LetVar_ s -> return (LetVar s)
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

varSearch : Variable -> TypeContext -> Int -> WhichVar
varSearch var list cnt =
  case list of
    [] -> LetVar_ var
    (hd,ty) :: tl ->
      if hd == var
      then LambdaVar_ cnt ty
      else varSearch var tl (cnt+1)

termToString : Term -> NamingContext -> String
termToString term list =
  case term of
    LetVar v ->
      "LetVar("++v++")"
    Let v t1 t2 ->
      "Let "++v++" = "++(termToString t1 list)++" in "++(termToString t2 list)
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
          Div -> "(Div "++t1Str++", "++t2Str++")"
          Mod -> "( "++t1Str++"(Mod "++t2Str++"))"
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
          "let"  -> fail
          "in"   -> fail
          "if"   -> fail
          "then" -> fail
          "else" -> fail
          "bool" -> fail
          "int"  -> fail
          _      -> return str
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

mulDivModParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      starSlashPercent
      (\_ s -> s)
    )
    (powParser list)
  |> pLog "mulParser"

plusMinusParser =
  or plusParser minusParser

starSlashPercent =
  choice
    [starParser,slachParser,percentParser]

calcIntParser list =
  foldl
    ( concat
      zeroOrMoreSpaceParser
      plusMinusParser
      (\_ x -> x)
    )
    (mulDivModParser list)

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

slachParser : Parser (Term->Term->Term)
slachParser =
  charMatch '/'
    |> map (always <| Calc Div)

percentParser : Parser (Term->Term->Term)
percentParser =
  charMatch '%'
    |> map (always <| Calc Mod)

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
