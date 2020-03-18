module TestParser exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Dict exposing (Dict)
import Test exposing (..)
import Main exposing (..)
import IorinParser exposing (..)

bvar n =
  LambdaVar n TypeBool

ivar n =
  LambdaVar n TypeInt

func = TypeFunction TypeBool TypeBool

t ty =
  if ty == TypeBool
  then bvar 0
  else ivar 0

id ty =
  LambdaAbs
    "n"
    ty
    (t ty)

app ty =
  LambdaApp
    (id ty)
    (if ty == TypeBool then TermTrue else TermInt 3)

app2 ty =
  LambdaApp
    (LambdaApp
      ( LambdaAbs "n" func
        (LambdaVar 0 func)
      )
      (id ty)
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
      [ describe "bool"
        [
          test "id"
            <| \_ ->
              ("\\n:Bool->n"
                |> get
              )
            |> Expect.equal
              (Success (id TypeBool) "")
        , test "app"
            <| \_ ->
              ("(\\n:Bool->n) true"
                |> get
              )
            |> Expect.equal
              (Success (app TypeBool) "")
        , test "not-abs"
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
                      Success hd "" -> evaluate Dict.empty hd
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
      , describe "int"
        [
          test "id"
            <| \_ ->
              ("\\n:Int->n"
                |> get
              )
            |> Expect.equal
              (Success (id TypeInt) "")
        , test "app"
            <| \_ ->
              ("(\\n:Int->n)3"
                |> get
              )
            |> Expect.equal
              (Success (app TypeInt) "")
        , test "fail"
            <| \_ ->
              ("(\\n:Int->Int)"
                |> get
              )
            |> Expect.equal Failed
        , test "app-fail"
            <| \_ ->
              ("2 3"
                |> get
                |> (\res ->
                    case res of
                      Success hd "" -> typeOf Dict.empty [] hd
                      _ -> Err "?"
                  )
              )
            |> Expect.equal (Err "Argument of LambdaApply is wrong(1)")
        ]
      , describe "let"
        [
          test "not"
            <| \_ ->
              ("let not=\\n:bool->if n then false else true in not true"
                |> get
                |> (\res ->
                    case res of
                      Success hd "" -> evaluate Dict.empty hd
                      _ -> LambdaVar -1 TypeBool
                  ) 
              )
            |> Expect.equal TermFalse

        ]
      ]
    , describe "toString"
      [
        test "id"
          <| \_ ->
            ( (id TypeBool)
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