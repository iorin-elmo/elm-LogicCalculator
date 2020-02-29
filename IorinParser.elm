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
  , lazy
  , zero
  , return
  , fail
  , char
  , charMatch
  , string
  , intersperseConcat
  , intersperseConcat3
  , intersperseConcat4
  , intersperseConcat5
  )

import Debug exposing(log)

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

concat6 pa pb pc pd pe pf f =
  pa
    |> fmap (\a -> concat5 pb pc pd pe pf (f a))

concat7 pa pb pc pd pe pf pg f =
  pa
    |> fmap (\a -> concat6 pb pc pd pe pf pg (f a))

concat8 pa pb pc pd pe pf pg ph f =
  pa
    |> fmap (\a -> concat7 pb pc pd pe pf pg ph (f a))

concat9 pa pb pc pd pe pf pg ph pi f =
  pa
    |> fmap (\a -> concat8 pb pc pd pe pf pg ph pi (f a))


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

lazy : (() -> Parser a) -> Parser a
lazy a =
  fmap a (return ())

zero : Parser ()
zero = return ()

return : a -> Parser a
return a = (\str -> Success a str)

fail : Parser a
fail = (\str -> Failed |> log str)

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

intersperseConcat : Parser i -> Parser a -> Parser b -> (a -> b -> c) -> Parser c
intersperseConcat i p1 p2 f =
  concat3
    p1 i p2
    (\a _ b -> f a b)

intersperseConcat3 i p1 p2 p3 f =
  concat5
    p1 i p2 i p3
    (\a _ b _ c -> f a b c)

intersperseConcat4 i p1 p2 p3 p4 f =
  concat7
    p1 i p2 i p3 i p4
    (\a _ b _ c _ d -> f a b c d)

intersperseConcat5 i p1 p2 p3 p4 p5 f =
  concat9
    p1 i p2 i p3 i p4 i p5
    (\a _ b _ c _ d _ e -> f a b c d e)
