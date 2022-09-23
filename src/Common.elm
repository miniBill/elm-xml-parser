module Common exposing
    ( Parser, DeadEnd, Count
    , maybe, zeroOrMore, oneOrMore, repeat, ignore, keep, fail, symbol, keyword, end, attributeName, attributeValue, comment, decodeEscape, escape, escapedChar, isWhitespace, textString, toToken, whiteSpace, whiteSpace1, defaultEntities
    )

{-|


# Types

@docs Parser, DeadEnd, Count


# Utils

@docs maybe, zeroOrMore, oneOrMore, repeat, ignore, keep, fail, symbol, keyword, end, attributeName, attributeValue, comment, decodeEscape, escape, escapedChar, isWhitespace, textString, toToken, whiteSpace, whiteSpace1, defaultEntities

-}

import Dict exposing (Dict)
import Hex
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Step(..), andThen, chompUntil, chompWhile, getChompedString, inContext, loop, map, oneOf, problem, succeed, token)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


{-| A problem when parsing. See the elm/parser documentation for details.
-}
type alias DeadEnd =
    Advanced.DeadEnd String Parser.Problem


type Count
    = AtLeast Int


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]


zeroOrMore : Count
zeroOrMore =
    AtLeast 0


oneOrMore : Count
oneOrMore =
    AtLeast 1


repeat : Count -> Parser a -> Parser (List a)
repeat count parser =
    case count of
        AtLeast n ->
            loop []
                (\state ->
                    oneOf
                        [ map (\r -> Loop (List.append state [ r ])) parser
                        , map (\_ -> Done state) (succeed ())
                        ]
                )
                |> andThen
                    (\results ->
                        if n <= List.length results then
                            succeed results

                        else
                            problem Parser.BadRepeat
                    )


ignore : Count -> (Char -> Bool) -> Parser ()
ignore count predicate =
    map (\_ -> ()) (keep count predicate)


keep : Count -> (Char -> Bool) -> Parser String
keep count predicate =
    case count of
        AtLeast n ->
            getChompedString (succeed () |. chompWhile predicate)
                |> andThen
                    (\str ->
                        if n <= String.length str then
                            succeed str

                        else
                            problem Parser.BadRepeat
                    )


fail : String -> Parser a
fail str =
    problem (Parser.Problem str)


symbol : String -> Parser ()
symbol str =
    Advanced.symbol (Advanced.Token str (Parser.ExpectingSymbol str))


keyword : String -> Parser ()
keyword kwd =
    Advanced.keyword (Advanced.Token kwd (Parser.ExpectingKeyword kwd))


end : Parser ()
end =
    Advanced.end Parser.ExpectingEnd


escape : String -> String
escape s =
    List.foldl
        (\( escaped, original ) -> String.replace original ("&" ++ escaped ++ ";"))
        s
        (Dict.toList defaultEntities)


whiteSpace : Parser ()
whiteSpace =
    ignore zeroOrMore isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\u{000D}' || c == '\n' || c == '\t'


whiteSpace1 : Parser ()
whiteSpace1 =
    ignore oneOrMore isWhitespace


comment : Parser ()
comment =
    succeed ()
        |. token (toToken "<!--")
        |. chompUntil (toToken "-->")
        |. token (toToken "-->")


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


attributeValue : Dict String String -> Parser String
attributeValue entities =
    inContext "attributeValue" <|
        oneOf
            [ succeed identity
                |. symbol "\""
                |= textString entities '"'
                |. symbol "\""
            , succeed identity
                |. symbol "'"
                |= textString entities '\''
                |. symbol "'"
            ]


textString : Dict String String -> Char -> Parser String
textString entities end_ =
    inContext "textString" <|
        loop []
            (\acc ->
                oneOf
                    [ succeed (\c -> Advanced.Loop <| c :: acc)
                        |= escapedChar entities end_
                    , succeed (\s -> Advanced.Loop <| s :: acc)
                        |= keep oneOrMore (\c -> c /= end_ && c /= '&')
                    , succeed (Advanced.Done <| String.concat <| List.reverse acc)
                    ]
            )


escapedChar : Dict String String -> Char -> Parser String
escapedChar entities end_ =
    inContext "escapedChar" <|
        (succeed identity
            |. symbol "&"
            |= keep oneOrMore (\c -> c /= end_ && c /= ';')
            |> andThen
                (\s ->
                    oneOf
                        [ symbol ";"
                            |> andThen
                                (\_ ->
                                    case decodeEscape entities s of
                                        Ok c ->
                                            succeed c

                                        Err e ->
                                            problem e
                                )
                        , fail ("Entities must end_ with \";\": &" ++ s)
                        ]
                )
        )


decodeEscape : Dict String String -> String -> Result Parser.Problem String
decodeEscape entities s =
    if String.startsWith "#x" s then
        s
            |> String.dropLeft 2
            |> Hex.fromString
            |> Result.map (Char.fromCode >> String.fromChar)
            |> Result.mapError Parser.Problem

    else if String.startsWith "#" s then
        s
            |> String.dropLeft 1
            |> String.toInt
            |> Maybe.map (Char.fromCode >> String.fromChar)
            |> Result.fromMaybe (Parser.Problem <| "Invalid escaped charactor: " ++ s)

    else
        Dict.get s entities
            |> Result.fromMaybe (Parser.Problem <| "No entity named \"&" ++ s ++ ";\" found.")


defaultEntities : Dict String String
defaultEntities =
    Dict.fromList
        [ ( "amp", "&" )
        , ( "lt", "<" )
        , ( "gt", ">" )
        , ( "apos", "'" )
        , ( "quot", "\"" )
        ]


attributeName : Parser String
attributeName =
    inContext "attributeName" <|
        keep oneOrMore (\c -> not (isWhitespace c) && c /= '/' && c /= '<' && c /= '>' && c /= '"' && c /= '\'' && c /= '=')
