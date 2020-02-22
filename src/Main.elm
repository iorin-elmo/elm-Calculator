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

{-
type Exp
  = Number Int
  | Add Exp Exp   --<Exp> + <Exp>
  | Sub Exp Exp   --<Exp> - <Exp>
  | Mul Exp Exp   --<Exp> * <Exp>
  | Pow Exp Exp   --<Exp> ^ <Exp>
  | Paren Exp     --(<Exp>)
  | Let Variable Exp Exp -- let x=2 in x*4
  | Var Variable
  | If Exp Exp Exp -- if condition then t else f
  | GreaterThan Exp Exp -- exp > exp (1 or 0)
  | LessThan Exp Exp -- exp < exp (1 (if)or 0)
  | Equal Exp Exp -- exp = exp (1 or 0)
-}
type Term
  = LambdaVar Int Type
  | LambdaAbs Variable Type Term
  | LambdaApp Term Term
  | TermTrue
  | TermFalse
  | TermIf Term Term Term

type Type
  = TypeBool
  | TypeFunction Type Type

{-
type LambdaExp
  = LambdaVar Int
  | LambdaAbs Variable LambdaExp
  | LambdaApp LambdaExp LambdaExp
-}

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
      let
        parseRes =
          model.inputString
            |> String.toLower
            |> lambdaParser []
            |> (\res ->
                case res of
                  Success hd tl -> hd
                  _ -> LambdaVar -1 TypeBool
              )
      in
        { model |
          strForCalc = parseRes
        , result = parseRes |> lambdaEval
        , types = parseRes |> typeOf []
          --|> expParser
        }

lambdaEval : Term -> Term
lambdaEval exp =
  if isValue exp
  then exp
  else lambdaEval (reduction exp)

isValue : Term -> Bool
isValue exp =
  case exp of
    TermTrue -> True
    TermFalse-> True
    LambdaAbs _ _ _ -> True
    LambdaVar n _ -> if n == -1 then True else False
    _ -> False

reduction : Term -> Term
reduction exp =
  case exp of
    LambdaApp left right ->
      case left of
        LambdaAbs _ _ e ->
          let
            test = replace 0 right e
            debug = log "exp" (lambdaExpToString test)
          in
            test
        LambdaApp _ _ -> LambdaApp (reduction left) right
        _ -> exp
    TermIf cond t1 t2 ->
      if (lambdaEval cond == TermTrue)
      then t1
      else t2
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
    _ -> target

{-
(\n -> \m -> m n) u LambdaApp (LambdaAbs "n" (LambdaAbs "m" (LambdaApp (LambdaVar 0) (LambdaVar 1)))) u
(\m -> m u)

(\n -> (\m -> m) n) u
((\m -> m )u)
u
-}
{-
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
-}

typeOf : TypeContext -> Term -> Result String Type
typeOf list term =
  case term of
    TermTrue -> Ok TypeBool
    TermFalse -> Ok TypeBool
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
    Err "***" -> "---"
    Err str -> str

lambdaParser : TypeContext -> Parser Term
lambdaParser list =
  unitOr
    (\()->lambdaAppParser list)
    (exceptingApp list)

exceptingApp list =
  choice [(parenOrVarOrAbs list),(lazy (\() -> termIfParser () list)),boolParser]


boolParser : Parser Term
boolParser =
  or
    (string "true" |> map (always TermTrue))
    (string "false" |> map (always TermFalse))

lambdaParenParser : TypeContext -> Parser Term
lambdaParenParser list =
  intersperceConcat3 zeroOrMoreSpaceParser
    parenOpenParser (lambdaParser list) parenCloseParser
    (\_ exp _ -> exp)

parenOrVarOrAbs : TypeContext -> Parser Term
parenOrVarOrAbs list =
  unitOr
    (\() -> lambdaAbsParser (list |> log "abs"))
    (unitOr
      (\() -> lambdaParenParser list)
      (lambdaVarParser (list|> log "var"))
    )


lambdaAbsParser : TypeContext -> Parser Term
lambdaAbsParser list =
  concat
    (
      intersperceConcat4 zeroOrMoreSpaceParser
        bsParser variableParser typeAnnotationParser (string "->")
        (\_ var ty _ -> ( var, ty ):: list )
    )
    zeroOrMoreSpaceParser
    (\varList _ -> varList)
  |> fmap
    (\varList ->
      lambdaParser varList
        |> fmap
          (\exp ->
            case varList of
              [] -> fail
              ( hdVar, hdType ) :: tl ->
                return (LambdaAbs hdVar hdType exp)
          )
    )

typeAnnotationParser : Parser Type
typeAnnotationParser =
  intersperceConcat zeroOrMoreSpaceParser
    (charMatch ':') (typeParser ())
    (\_ ty -> ty)

typeParser : () -> Parser Type
typeParser =
  (\()->
    intersperceConcat zeroOrMoreSpaceParser
      (
        unitOr
          (\() ->
            (intersperceConcat3 zeroOrMoreSpaceParser
              parenOpenParser (lazy typeParser) parenCloseParser
              (\_ ty _ -> ty)
            )
          )
          (string "bool" |> map (always TypeBool))
      )
      (zeroOrMore (lazy arrowAndTypeParser))
      (\ty tylist ->
        List.foldl (\a b ->a b) ty tylist
      )
  )


arrowAndTypeParser : () -> Parser (Type -> Type)
arrowAndTypeParser =
  (\()->
    intersperceConcat zeroOrMoreSpaceParser
      (string "->") (lazy typeParser)
      (\_ ty -> (\arg -> TypeFunction arg ty))
  )

lambdaAppParser : TypeContext -> Parser Term
lambdaAppParser list =
  concat
    (exceptingApp list)
    (oneOrMore
      (concat
        zeroOrMoreSpaceParser
        (exceptingApp list)
        (\_ varOrAbsList -> varOrAbsList)
      )
    )
    (\hd tl ->
      List.foldl (\a b -> LambdaApp b a) hd tl
    )


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

varSearch : Variable -> TypeContext -> Int -> Maybe ( Int, Type )
varSearch var list cnt =
  case list of
    [] -> Nothing
    (hd,ty) :: tl ->
      if hd == var
      then (cnt, ty) |> Just
      else varSearch var tl (cnt+1)

termIfParser :() -> TypeContext -> Parser Term
termIfParser () list =
  intersperceConcat4 oneOrMoreSpaceParser
    (string "if") (lambdaParser list)
    (string "then")
    (intersperceConcat3 oneOrMoreSpaceParser
      (lambdaParser list)
      (string "else")
      (lambdaParser list)
      (\t1 _ t2 -> (t1,t2))
    )
    (\_ c _ (t1,t2) -> TermIf c t1 t2)

type alias NamingContext = List Variable
type alias TypeContext = List ( Variable, Type )

lambdaExpToString : Term -> NamingContext -> String
lambdaExpToString lambdaExp list =
  case lambdaExp of
    LambdaVar n _ ->
      if n == -1
      then "Something is wrong"
      else getName n (list |> log "varlist")
    LambdaAbs var _ exp ->
      let
        newVar = getNewVar var list
      in
        "(\\ "++newVar++" ->"++(lambdaExpToString exp (newVar::list) )++")"
    LambdaApp left right ->
      "("++(lambdaExpToString left list)++" "++(lambdaExpToString right list)++")"
    TermTrue -> "True"
    TermFalse-> "False"
    TermIf cond t1 t2 ->
      "If "++
      (lambdaExpToString cond list)++
      " Then "++
      (lambdaExpToString t1 list)++
      " Else "++
      (lambdaExpToString t2 list)

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

{-
expParser : Parser Exp
expParser = expressionParser

-}
zeroOrMoreSpaceParser =
  zeroOrMore (charMatch ' ')
    |> map (always ())

oneOrMoreSpaceParser =
  concat
    (charMatch ' ') zeroOrMoreSpaceParser
    (\_ _ -> ())
{-
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
{-
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

-}

parenOpenParser : Parser ()
parenOpenParser =
  charMatch '('

parenCloseParser : Parser ()
parenCloseParser =
  charMatch ')'

commaParser : Parser ()
commaParser =
  charMatch ','

{-
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

-}

bsParser =
  charMatch '\\'


view : Model -> Html Msg
view model =
  let
    lambdaStr = lambdaExpToString model.result []
  in
    div []
      [ input
        [ type_ "textbox"
        , onInput Input
        ][]
      , button [ onClick Pressed ][ text "Parse it !" ]
      , br[][]
      --, text <| resultToString model.strForCalc
      , text <| lambdaExpToString model.strForCalc []
      , br[][]
      , text <|
        case model.types of
          Ok _ -> lambdaExpToString model.result []
          Err _ -> "Type Mismatch"

      --, text <| resultToEvaluatedString model
      , br[][]
      , text <|
        case lambdaStr of
          "Something is wrong" -> "Parse Error"
          "Parse Error" -> "Parse Error"
          _ -> typeToString model.types
      ]

{-
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

-}

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
