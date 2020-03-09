module TestParser exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)
import IorinParser exposing (..)

bvar n =
  LambdaVar n TypeBool

ivar n =
  LambdaVar n TypeInt

func = TypeFunction TypeBool TypeBool

id =
  LambdaAbs
    "n"
    TypeBool
    (bvar 0)

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
      (bvar 0)
      TermFalse
      TermTrue
    )

min =
  LambdaAbs
    "n"
    TypeInt
    ( LambdaAbs
      "m"
      TypeInt
      (TermIf
        (Compare LessThan
          (ivar 1)
          (ivar 0)
        )
        (ivar 1)
        (ivar 0)
      )
    )

get str =
  str
    |> String.toLower
    |> termParser []


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
            ("\\n:Bool->if n then false else true"
              |> get
            )
          |> Expect.equal
            (Success not "")
      , test "app2"
          <| \_ ->
            ("(\\n:bool->bool->n)(\\n:bool->n)true"
              |> get
              |> (\res ->
                  case res of
                    Success hd "" -> evaluate hd
                    _ -> LambdaVar -1 TypeBool
                ))
            |> Expect.equal TermTrue
      , test "fail"
          <| \_ ->
            ("\\n->n"
              |> get
            )
          |> Expect.equal Failed
      ]
    , describe "toString"
      [
        test "id"
          <| \_ ->
            ( id
              |> (\exp -> termToString exp [])
            )
          |> Expect.equal "(\\ n ->n)"
      ]
    , describe "choice"
      [
        test "A"
          <| \_ ->
            ""
              |> (choice
                  [ return "A"
                  , return "B"
                  , return "C" 
                  ]
                  )
              |> Expect.equal (Success "A" "")
      , test "B"
        <| \_ ->
            "P"
              |> (choice
                  [ charMatch 'Q' |> map (always "A")
                  , charMatch 'P' |> map (always "B")
                  , charMatch 'P' |> map (always "C") 
                  ]
                  )
              |> Expect.equal (Success "B" "")
      ]
    , describe "foldl"
      [
        test "testFoldl"
          <| \_ ->
            let
              nat =
                char Char.isDigit
                  |> fmap (\num ->
                    case String.toInt <| String.fromChar  num of
                      Just i -> return i
                      Nothing -> fail
                  )

              muldiv =
                nat
                  |> foldl (
                    choice
                    [ charMatch '*' |> map (always (*))
                    , charMatch '/' |> map (always (//))
                    ]
                  )

              addsub =
                muldiv
                  |> foldl (
                    choice
                    [ charMatch '+' |> map (always (+))
                    , charMatch '-' |> map (always (-))
                    ]
                  )

              parser = addsub
            in
              parser "1+2*4-5+6/2"
                |> Expect.equal (Success 7 "")
      ]
    ]