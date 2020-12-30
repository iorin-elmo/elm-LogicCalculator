module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, br, span)
import Html.Attributes exposing (type_, value, checked)
import Html.Events exposing (onClick, onInput)
import IorinParser exposing (..)

import Dict exposing (Dict)

import Debug exposing (log)

type Msg
  = Input String
  | Check Char
  | Pressed String

type alias Model =
  { inputStr : String
  , result : Res LogExp
  , evalResult : Maybe Bool
  , varList : List (Char, Bool)
  , comment : String
  }

initialModel =
  { inputStr = ""
  , result = Failed
  , evalResult = Nothing
  , varList = []
  , comment = ""
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
    [ (lazy (\()-> parenParser))
    , (lazy (\()-> notParser))
    , boolParser
    , varParser
    ] 

parenParser : Parser LogExp
parenParser =
  intersperseConcat3 zeroOrMoreSpaceParser
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
  intersperseConcat zeroOrMoreSpaceParser
  pNot tfOrVarOrParen
  (\pnot e -> pnot e)

andParser =
  foldl
    ( concat
        zeroOrMoreSpaceParser
        pAnd
        (\_ x -> x)
    )
    tfOrVarOrParen

orParser =
  foldl
    ( concat
        zeroOrMoreSpaceParser
        pOr
        (\_ x -> x)
    )
    andParser

impParser =
  foldr
    ( concat
        zeroOrMoreSpaceParser
        pImp
        (\_ x -> x)
    )
    orParser

logExpParser : Parser LogExp
logExpParser =
  foldl
    ( concat
        zeroOrMoreSpaceParser
        pIff
        (\_ x -> x)
    )
    impParser

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

search : LogExp -> List (Char, Bool) -> List Char -> Maybe (List (Char, Bool))
search exp result rest =
  case rest of
    [] ->
      if evaluate (Dict.fromList result) exp
      then Just result
      else Nothing
    hd :: tl ->
      case search exp ((hd, True) :: result) tl of
        Just result_ ->
          Just result_
        Nothing ->
          search exp ((hd, False) :: result) tl

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

updateVarList model vl =
  { model | varList = vl }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input str -> {model|inputStr=str}
    Check c ->
      { model |
        varList =
          model.varList
            |> List.map 
              (\(char,bool)->
                if char == c
                then (char, not bool)
                else (char, bool)
              )
      }
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
          let
            newExpModel =
              { model |
                result = model.inputStr |> String.replace " " "" |> logicExpressionParser
              }
            newResModel =
              { newExpModel |
                evalResult =
                  case newExpModel.result of
                    Success e "" ->
                      evaluate (Dict.fromList newExpModel.varList) e
                        |> Just
                    _ -> Nothing
              , comment = ""
              }
            getVarListFromParseResult e =
              let
                getVarHelper exp li =
                  case exp of
                    And e1 e2 -> getVarHelper e2 (getVarHelper e1 li)
                    Or  e1 e2 -> getVarHelper e2 (getVarHelper e1 li)
                    Imp e1 e2 -> getVarHelper e2 (getVarHelper e1 li)
                    Iff e1 e2 -> getVarHelper e2 (getVarHelper e1 li)
                    Not ex -> getVarHelper ex li
                    Var c ->
                      if List.member c li
                      then
                        li
                      else
                        c :: li
                    _ -> li
              in
                getVarHelper e []
                  |> List.sort
          in
            case newResModel.result of
              Success e "" ->
                getVarListFromParseResult e
                  |> List.map (\c->(c,False))
                  |> updateVarList newResModel
              _ -> updateVarList newResModel []
        "calc" ->
          { model |
            evalResult =
              case model.result of
                Success e "" ->
                  evaluate (Dict.fromList model.varList) e
                    |> Just
                _ -> Nothing
          , comment = ""
          }
        "solve" ->
          case model.result of
            Success e "" ->
              let
                newModel = 
                  let
                    charList =
                      model.varList
                        |> List.unzip
                        |> Tuple.first
                  in
                    case search e [] (List.reverse charList) of
                      Just list -> updateVarList model list
                      Nothing -> { model | comment = "This LogicExp is unSAT." }
              in
                { newModel | 
                  evalResult =
                    case model.result of
                      Success exp "" ->
                        evaluate (Dict.fromList newModel.varList) exp
                          |> Just
                      _ -> Nothing
                }
            _ -> model
        _ -> model

view : Model -> Html Msg
view model =
  div []
    <| List.append
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
        [ text "Calculate it !" ]
      , button
        [ onClick <| Pressed "solve" ]
        [ text "Solve it !" ]
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
      , br[][]
      , text model.comment
      , br[][]
      ]
      (varSelectBoxList model.varList)

varSelectBoxList li =
  li
    |> List.map
      (\( c, bool ) -> makeSelectBox c bool )
    |> List.intersperse (text ", ")

makeSelectBox c bool =
  span []
    [ text <| (String.fromChar c)++" : "
    , input
      [ type_ "checkbox"
      , checked bool
      , onClick <| Check c
      ][]
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
