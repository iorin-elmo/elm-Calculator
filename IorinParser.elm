module IorinParser exposing
  ( Parser
  , Res (..)
  , concat
  , or
  , unitOr
  , zeroOrMore
  , oneOrMore
  , map
  , fmap
  , zero
  , char
  )

type alias Parser a = String -> Res a

type Res a
  = Success a String
  | Failed

concat : Parser a -> Parser b -> ( a -> b -> c ) -> Parser c
concat pa pb f =
  (\str ->
    case pa str of
      Success hdA tlA ->
        case pb tlA of
          Success hdB tlB ->
            Success (f hdA hdB) tlB
          _ ->
            Failed
      _ ->
        Failed
  )

or : Parser a -> Parser a -> Parser a
or p1 p2 =
  (\str ->
    case p1 str of
      Success hd tl -> Success hd tl
      _ ->
        case p2 str of
          Success hd tl -> Success hd tl
          _ -> Failed
  )

unitOr : (() -> Parser a) -> Parser a -> Parser a
unitOr up p =
  (\str ->
    case up () str of
      Success hd tl -> Success hd tl
      _ ->
        case p str of
          Success hd tl -> Success hd tl
          _ -> Failed
  )

zeroOrMore : Parser a -> Parser (List a)
zeroOrMore p =
  or
    (oneOrMore p)
    (map (always []) zero)

oneOrMore : Parser a -> Parser (List a)
oneOrMore p =
  let
    parseHelper : List a -> Parser (List a)
    parseHelper list str =
      case p str of
        Success hd tl ->
          parseHelper (hd::list) tl
        _ ->
          Success (List.reverse list) str
  in
    (\str ->
      case p str of
        Success hd tl ->
          parseHelper [hd] tl
        _ -> Failed
    )

map : (a -> b) -> Parser a -> Parser b
map f pa =
  (\str ->
    case pa str of
      Success hd tl ->
        Success (f hd) tl
      _ -> Failed
  )

fmap : (a -> Parser b) -> Parser a -> Parser b
fmap f pa =
  (\str ->
    case pa str of
      Success hd tl ->
        f hd tl
      _ -> Failed
  )

zero : Parser ()
zero = (\str -> Success () str)

char : (Char -> Bool) -> Parser Char
char f =
  (\str ->
    case String.uncons str of
      Just ( hd, tl ) ->
        if f hd
        then Success hd tl
        else Failed
      _ -> Failed
  )