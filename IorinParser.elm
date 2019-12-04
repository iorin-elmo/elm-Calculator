module IorinParser exposing
  ( Parser
  , Res (..)
  , concat
  , concat3
  , concat4
  , concat5
  , or
  , unitOr
  , choice
  , unitChoice
  , zeroOrMore
  , oneOrMore
  , map
  , fmap
  , zero
  , return
  , fail
  , char
  , charMatch
  , string
  )

type alias Parser a = String -> Res a

type Res a
  = Success a String
  | Failed

concat : Parser a -> Parser b -> ( a -> b -> c ) -> Parser c
concat pa pb f =
  pa
    |> fmap (\a -> map (f a) pb)

concat3 : Parser a -> Parser b -> Parser c -> (a -> b -> c -> d) -> Parser d
concat3 pa pb pc f =
  pa
    |> fmap (\a -> concat pb pc (f a))

concat4 : Parser a -> Parser b -> Parser c -> Parser d -> (a -> b -> c -> d -> e) -> Parser e
concat4 pa pb pc pd f =
  pa
    |> fmap (\a -> concat3 pb pc pd (f a))

concat5 : Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> (a -> b -> c -> d -> e -> f) -> Parser f
concat5 pa pb pc pd pe f =
  pa
    |> fmap (\a -> concat4 pb pc pd pe (f a))


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

choice : List (Parser a) -> Parser a
choice list =
  List.foldl or fail list

unitChoice : (() -> Parser a) -> List (Parser a) -> Parser a
unitChoice p list =
  unitOr p (choice list)

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
  pa
    |> fmap (\a -> return (f a))

fmap : (a -> Parser b) -> Parser a -> Parser b
fmap f pa =
  (\str ->
    case pa str of
      Success hd tl ->
        f hd tl
      _ -> Failed
  )

zero : Parser ()
zero = return ()

return : a -> Parser a
return a = (\str -> Success a str)

fail : Parser a
fail = (\str -> Failed)

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

charMatch : Char -> Parser ()
charMatch c =
  char ((==) c)
    |> map (always ())

string : String -> Parser String
string str =
  forParser (String.length str) (char (always True))
    |> map String.fromList
    |> fmap
      (\parsedStr ->
        if parsedStr==str
        then return str
        else fail
      )

forParser : Int -> Parser a -> Parser (List a)
forParser n p =
  case n of
    0 -> return []
    _ ->
      concat
        p (forParser (n-1) p)
        (\a list -> a :: list)
