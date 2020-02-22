module TestParser exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)
import IorinParser exposing (..)

var n =
  LambdaVar n TypeBool

func = TypeFunction TypeBool TypeBool

id =
  LambdaAbs
    "n"
    TypeBool
    (var 0)

app =
  LambdaApp
    id
    TermTrue

app2 =
  LambdaApp
    (LambdaApp
      ( LambdaAbs "n" func
        (LambdaVar 0 func)
      )
      id
    )
    TermTrue

not =
  LambdaAbs
    "n"
    TypeBool
    (TermIf
      (var 0)
      TermFalse
      TermTrue
    )

get str =
  str
    |> String.toLower
    |> lambdaParser []


suite : Test
suite =
  describe "t"
    [ describe "parse"
      [
        test "id"
          <| \_ ->
            ("\\n:Bool->n"
              |> get
            )
          |> Expect.equal
            (Success id "")
      , test "app"
          <| \_ ->
            ("(\\n:Bool->n) true"
              |> get
            )
          |> Expect.equal
            (Success app "")
      , test "not"
          <| \_ ->
            ("(\\n:Bool->if n then false else true)"
              |> get
            )
          |> Expect.equal
            (Success not "")
      , test "app2"
          <| \_ ->
            ("(\\n:bool->bool->n)(\\n:bool->n)true"
              |> get
            )
          |> Expect.equal
            (Success app2 "")
      , test "fail"
          <| \_ ->
            ("\\n->n"
              |> get
            )
          |> Expect.equal Failed
      ]
    {-
    , describe "toString"
      [
      ]
    -}
    ]