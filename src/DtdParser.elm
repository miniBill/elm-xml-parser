module DtdParser exposing
    ( Dtd, DocTypeDefinition(..), Element(..)
    , parse, DeadEnd, parser
    , format
    )

{-| The DTD Parser.


# Types

@docs Dtd, DocTypeDefinition, Element


# Parse

@docs parse, DeadEnd, parser


# Format

@docs format

-}

import Common exposing (Parser, attributeName, attributeValue, comment, defaultEntities, escape, keyword, repeat, symbol, toToken, whiteSpace, whiteSpace1, zeroOrMore)
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Step(..), chompUntil, getChompedString, inContext, oneOf, succeed)


{-| DTD (Doc Type Definition)

  - Public: `<!DOCTYPE root PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">`
  - System: `<!DOCTYPE root SYSTEM "foo.xml">`
  - Custom: `<!DOCTYPE root [ <!ELEMENT ...> ]>`

-}
type DocTypeDefinition
    = Public String String (Maybe Dtd)
    | System String (Maybe Dtd)
    | Custom Dtd


{-| A problem when parsing. See the elm/parser documentation for details.
-}
type alias DeadEnd =
    Advanced.DeadEnd String Parser.Problem


{-| Parse a DTD element.
-}
parse : String -> Result (List DeadEnd) Dtd
parse =
    Advanced.run parser


{-| Parser for a DTD element.
-}
parser : Parser Dtd
parser =
    inContext "dtd" <|
        succeed identity
            |= repeat zeroOrMore elementParser


elementParser : Parser Element
elementParser =
    inContext "element" <|
        succeed identity
            |. repeat zeroOrMore (oneOf [ whiteSpace1, comment ])
            |. symbol "<!"
            |= oneOf
                [ succeed identity
                    |. keyword "ENTITY"
                    |. whiteSpace1
                    |= entityParser
                , succeed Unimplemented
                    |= attributeName
                    |. whiteSpace1
                    |= getChompedString (chompUntil (toToken ">"))
                ]
            |. symbol ">"
            |. repeat zeroOrMore (oneOf [ whiteSpace1, comment ])


entityParser : Parser Element
entityParser =
    inContext "entity" <|
        succeed Entity
            |= attributeName
            |. inContext "spacer" whiteSpace1
            |= attributeValue defaultEntities
            |. whiteSpace


{-| A DTD is a list of DTD elements.
-}
type alias Dtd =
    List Element


{-| Only Entities are supported for now.
-}
type Element
    = Entity String String
    | Unimplemented String String


{-| Format a list of DTD elements.
-}
format : Dtd -> String
format elements =
    String.join "\n" (List.map formatElement elements)


formatElement : Element -> String
formatElement element =
    case element of
        Entity key value ->
            "<!ENTITY " ++ key ++ " \"" ++ escape value ++ "\">"

        Unimplemented k v ->
            "<!" ++ k ++ " " ++ v ++ ">"
