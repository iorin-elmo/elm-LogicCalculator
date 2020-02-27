module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import IorinParser exposing (..)

import Dict exposing (Dict)

import Debug exposing (log)

type Msg
  = Input String
  | Pressed String

type alias Model =
  { inputStr : String
  , result : Res LogExp
  , evalResult : Maybe Bool
  , tfDict : Dict Char Bool
  }

initialModel =
  { inputStr = ""
  , result = Failed
  , evalResult = Nothing
  , tfDict = Dict.empty
  }

-- ⇒ imp (implies)
-- ⇔ iff (if and only if)
-- ¬ not
-- ∧ and
-- ∨ or

type LogExp
  = Boolean Bool
  | Var Char
  | Not LogExp
  | And LogExp LogExp
  | Or  LogExp LogExp
  | Imp LogExp LogExp
  | Iff LogExp LogExp

zeroOrMoreSpaceParser =
  zeroOrMore (charMatch ' ')
    |> map (always ())

tfOrVarOrParen =
  choice
    [(lazy (\()-> parenParser))
    ,(lazy (\()-> notParser))
    ,boolParser
    ,varParser
    ] 

parenParser : Parser LogExp
parenParser =
  intersperceConcat3 zeroOrMoreSpaceParser
  openParen logicExpressionParser closeParen
  (\_ e _ -> e )

varParser : Parser LogExp
varParser =
  char Char.isAlpha
    |> map (\c -> Var c)

boolParser : Parser LogExp
boolParser =
  or
    ((charMatch '⊤') |> map (always (Boolean True )))
    ((charMatch '⊥') |> map (always (Boolean False)))

notParser =
  intersperceConcat zeroOrMoreSpaceParser
  pNot tfOrVarOrParen
  (\pnot e -> pnot e)

andAndtfOrVarOrParen =
  intersperceConcat zeroOrMoreSpaceParser
  pAnd tfOrVarOrParen
  (\pand n -> (\left -> pand left n))

andList =
  zeroOrMore
    (concat
      zeroOrMoreSpaceParser
      andAndtfOrVarOrParen
      (\_ e -> e)
    )

andParser =
  concat
    tfOrVarOrParen andList
    (\left list ->
      list
        |> List.foldl (\e l -> e l) left
    )

orAndandNot =
  intersperceConcat zeroOrMoreSpaceParser
  pOr andParser
  (\por a -> (\left -> por left a))

orAndandNotList =
  zeroOrMore
    (concat
      zeroOrMoreSpaceParser
      orAndandNot
      (\_ e -> e)
    )

orParser =
  concat
    andParser orAndandNotList
    (\left list ->
      list
        |> List.foldl (\e l -> e l) left
    )

impAndorAndNot =
  intersperceConcat zeroOrMoreSpaceParser
  pImp orParser
  (\pimp o -> (\left -> pimp left o))

impAndorAndNotList =
  zeroOrMore
    (concat
      zeroOrMoreSpaceParser
      impAndorAndNot
      (\_ e -> e)
    )

impParser =
  concat
    orParser impAndorAndNotList
    (\left list ->
      list
        |> List.foldl (\e l -> e l) left
    )

all =
  intersperceConcat zeroOrMoreSpaceParser
  pIff impParser
  (\piff a -> (\left -> piff left a))

allList =
  zeroOrMore
    (concat
      zeroOrMoreSpaceParser
      all
      (\_ e -> e)
    )

logExpParser : Parser LogExp
logExpParser =
  concat
    impParser allList
    (\left list ->
      list
        |> List.foldl (\e l -> e l) left
    )

logicExpressionParser : Parser LogExp
logicExpressionParser =
  logExpParser

openParen  = char ((==)'(') |> map (always ())
closeParen = char ((==)')') |> map (always ())
pNot = charMatch '¬' |> map (always Not)
pAnd = charMatch '∧' |> map (always And)
pOr  = charMatch '∨' |> map (always Or )
pImp = charMatch '⇒' |> map (always Imp)
pIff = charMatch '⇔' |> map (always Iff)

evaluate : Dict Char Bool -> LogExp -> Bool
evaluate dict logExp =
  case logExp of
    Var c -> Maybe.withDefault False (Dict.get c dict)
    Not e -> not (evaluate dict e)
    And e1 e2 -> (&&) (evaluate dict e1) (evaluate dict e2)
    Or  e1 e2 -> (||) (evaluate dict e1) (evaluate dict e2)
    Imp e1 e2 -> (||) (not (evaluate dict e1)) (evaluate dict e2)
    Iff e1 e2 -> (==) (evaluate dict e1) (evaluate dict e2)
    Boolean bool -> bool

resultToString : LogExp -> String
resultToString logExp =
  case logExp of
    Var c -> String.fromChar c
    Not e -> "(Not "++resultToString e++" )"
    And e1 e2 -> "(And "++resultToString e1++", "++resultToString e2++" )"
    Or  e1 e2 -> "(Or " ++resultToString e1++", "++resultToString e2++" )"
    Imp e1 e2 -> "(Imp "++resultToString e1++", "++resultToString e2++" )"
    Iff e1 e2 -> "(Iff "++resultToString e1++", "++resultToString e2++" )"
    Boolean True -> "True"
    Boolean False-> "False"

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input str -> {model|inputStr=str}
    Pressed str ->
      case str of
        "imp" -> {model|inputStr=model.inputStr++"⇒"}
        "iff" -> {model|inputStr=model.inputStr++"⇔"}
        "not" -> {model|inputStr=model.inputStr++"¬"}
        "and" -> {model|inputStr=model.inputStr++"∧"}
        "or"  -> {model|inputStr=model.inputStr++"∨"}
        "T"   -> {model|inputStr=model.inputStr++"⊤"}
        "F"   -> {model|inputStr=model.inputStr++"⊥"}
        "parse" ->
          { model |
            result = model.inputStr |> logicExpressionParser
          }
        "calc" ->
          { model |
            evalResult =
              case model.result of
                Success e "" ->
                  evaluate model.tfDict e
                    |> Just
                _ -> Nothing
          }
        _ -> model

view : Model -> Html Msg
view model =
  div []
    [ input
      [ type_ "textbox"
      , onInput Input
      , value model.inputStr
      ][]
    , button
      [ onClick <| Pressed "parse" ]
      [ text "Parse it !" ]
    , button
      [ onClick <| Pressed "calc" ]
      [ text "Calculate it !"]
    , br[][]
    , button
      [ onClick <| Pressed "not" ]
      [ text "¬" ]
    , button
      [ onClick <| Pressed "and" ]
      [ text "∧" ]
    , button
      [ onClick <| Pressed "or"  ]
      [ text "∨" ]
    , button
      [ onClick <| Pressed "imp" ]
      [ text "⇒" ]
    , button
      [ onClick <| Pressed "iff" ]
      [ text "⇔" ]
    , button
      [ onClick <| Pressed "T" ]
      [ text "⊤" ]
    , button
      [ onClick <| Pressed "F" ]
      [ text "⊥" ]
    , br[][]
    , text
      <| case model.result of
        Success str "" -> resultToString str
        _ -> "Error"
    , br[][]
    , text
      <| case model.evalResult of
        Just True -> "True"
        Just False-> "False"
        Nothing -> "Error"
    ]


main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }