module XmlParser exposing
    ( Xml, ProcessingInstruction, DocType, DocTypeDefinition(..), Node(..), Attribute
    , parse, DeadEnd
    , format
    )

{-| The XML Parser.


# Types

@docs Xml, ProcessingInstruction, DocType, DocTypeDefinition, Node, Attribute


# Parse

@docs parse, DeadEnd


# Format

@docs format

-}

import Common exposing (Parser, attributeName, attributeValue, comment, defaultEntities, end, escape, escapedChar, fail, isWhitespace, keep, keyword, maybe, oneOrMore, repeat, symbol, toToken, whiteSpace, whiteSpace1, zeroOrMore)
import Dict exposing (Dict)
import DtdParser exposing (DocTypeDefinition(..), Dtd)
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Step(..), andThen, chompUntil, getChompedString, inContext, lazy, loop, map, oneOf, succeed)
import Set


{-| This represents the entire XML structure.

  - processingInstructions: `<?xml-stylesheet type="text/xsl" href="style.xsl"?>`
  - docType: `<!DOCTYPE root SYSTEM "foo.xml">`
  - root: `<root><foo/></root>`

-}
type alias Xml =
    { processingInstructions : List ProcessingInstruction
    , docType : Maybe DocType
    , root : Node
    }


{-| Processing Instruction such as `<?xml-stylesheet type="text/xsl" href="style.xsl"?>`.

The example above is parsed as `{ name = "xml-stylesheet", value = "type=\"text/xsl\" href=\"style.xsl\"" }`.
The value (presudo attributes) should be parsed by application.

-}
type alias ProcessingInstruction =
    { name : String
    , value : String
    }


{-| A Doc Type can be defined inline.
-}
type DocTypeDefinition
    = Public String String (Maybe String)
    | System String (Maybe String)
    | Custom String


{-| Doc Type Declaration starting with "<!DOCTYPE".

This contains root element name and rest of details as `DocTypeDefinition`.

-}
type alias DocType =
    { rootElementName : String
    , definition : DtdParser.DocTypeDefinition
    }


{-| Node is either a element such as `<a name="value">foo</a>` or text such as `foo`.
-}
type Node
    = Element String (List Attribute) (List Node)
    | Text String


{-| Attribute such as `name="value"`
-}
type alias Attribute =
    { name : String, value : String }


{-| A problem when parsing. See the elm/parser documentation for details.
-}
type alias DeadEnd =
    Advanced.DeadEnd String Parser.Problem


{-| Parse XML string.

`<?xml ... ?>` and `<!DOCTYPE ... >` is optional so you don't need to ensure them.

    > import XmlParser
    > XmlParser.parse """<a name="value">foo</a>"""
    Ok { processingInstructions = [], docType = Nothing, root = Element "a" ([{ name = "name", value = "value" }]) ([Text "foo"]) }

-}
parse : String -> Result (List DeadEnd) Xml
parse source =
    Advanced.run xml source


type alias Header =
    { processingInstructions : List ProcessingInstruction
    , maybeDocType : Maybe DocType
    }


xml : Parser Xml
xml =
    let
        headerParser : Parser Header
        headerParser =
            succeed
                (\processingInstructions maybeDocType ->
                    { processingInstructions = processingInstructions
                    , maybeDocType = maybeDocType
                    }
                )
                |= repeat zeroOrMore
                    (succeed identity
                        |= processingInstruction
                        |. whiteSpace
                    )
                |. repeat zeroOrMore (oneOf [ whiteSpace1, comment ])
                |= maybe docType
                |. repeat zeroOrMore (oneOf [ whiteSpace1, comment ])

        bodyParser : Header -> Parser Xml
        bodyParser { processingInstructions, maybeDocType } =
            let
                go : Dtd -> Dict String String
                go dtd =
                    dtd
                        |> List.filterMap
                            (\d ->
                                case d of
                                    DtdParser.Entity k v ->
                                        Just ( k, v )

                                    DtdParser.Unimplemented _ _ ->
                                        Nothing
                            )
                        |> List.foldl (\( k, v ) -> Dict.insert k v) defaultEntities

                entities : Dict String String
                entities =
                    case Maybe.map .definition maybeDocType of
                        Just (DtdParser.Custom dtd) ->
                            go dtd

                        Just (DtdParser.Public _ _ (Just dtd)) ->
                            go dtd

                        Just (DtdParser.System _ (Just dtd)) ->
                            go dtd

                        Just (DtdParser.Public _ _ Nothing) ->
                            defaultEntities

                        Just (DtdParser.System _ Nothing) ->
                            defaultEntities

                        Nothing ->
                            defaultEntities
            in
            succeed
                (\root ->
                    { processingInstructions = processingInstructions
                    , docType = maybeDocType
                    , root = root
                    }
                )
                |= element entities
    in
    inContext "xml"
        (succeed identity
            |. whiteSpace
            |= (headerParser
                    |> andThen
                        bodyParser
               )
            |. repeat zeroOrMore (oneOf [ whiteSpace1, comment ])
            |. end
        )


processingInstruction : Parser ProcessingInstruction
processingInstruction =
    inContext "processingInstruction" <|
        succeed ProcessingInstruction
            |. symbol "<?"
            |= processingInstructionName
            |. symbol " "
            |= processingInstructionValue


processingInstructionName : Parser String
processingInstructionName =
    inContext "processingInstructionName" <|
        keep oneOrMore
            (\c ->
                case c of
                    ' ' ->
                        False

                    _ ->
                        True
            )


processingInstructionValue : Parser String
processingInstructionValue =
    inContext "processingInstructionValue" <|
        oneOf
            [ succeed ""
                |. symbol "?>"
            , symbol "?"
                |> andThen
                    (\_ ->
                        processingInstructionValue
                            |> map (\tail -> "?" ++ tail)
                    )
            , succeed (++)
                |= keep zeroOrMore
                    (\c ->
                        case c of
                            '?' ->
                                False

                            _ ->
                                True
                    )
                |= lazy (\_ -> processingInstructionValue)
            ]


docType :
    Parser
        { rootElementName : String
        , definition : DtdParser.DocTypeDefinition
        }
docType =
    inContext "docType" <|
        succeed
            (\rootElementName definition ->
                { rootElementName = rootElementName
                , definition = definition
                }
            )
            |. symbol "<!DOCTYPE"
            |. whiteSpace
            |= tagName
            |. whiteSpace
            |= docTypeDefinition
            |. whiteSpace
            |. symbol ">"


docTypeDefinition : Parser DtdParser.DocTypeDefinition
docTypeDefinition =
    inContext "docTypeDefinition" <|
        oneOf
            [ succeed DtdParser.Public
                |. keyword "PUBLIC"
                |. whiteSpace
                |= publicIdentifier
                |. whiteSpace
                |= docTypeExternalSubset
                |. whiteSpace
                |= maybe docTypeInternalSubset
            , succeed DtdParser.System
                |. keyword "SYSTEM"
                |. whiteSpace
                |= docTypeExternalSubset
                |. whiteSpace
                |= maybe docTypeInternalSubset
            , succeed DtdParser.Custom
                |= docTypeInternalSubset
            ]


publicIdentifier : Parser String
publicIdentifier =
    inContext "publicIdentifier" quotedString


docTypeExternalSubset : Parser String
docTypeExternalSubset =
    inContext "docTypeExternalSubset" quotedString


quotedString : Parser String
quotedString =
    singleOrDoubleQuote
        |> andThen
            (\quoteChar ->
                succeed identity
                    |= keep zeroOrMore (\c -> c /= quoteChar)
                    |. symbol (String.fromChar quoteChar)
            )


singleOrDoubleQuote : Parser Char
singleOrDoubleQuote =
    inContext "singleOrDoubleQuote" <|
        oneOf
            [ succeed '"' |. symbol "\""
            , succeed '\'' |. symbol "'"
            ]


docTypeInternalSubset : Parser Dtd
docTypeInternalSubset =
    inContext "docTypeInternalSubset" <|
        succeed identity
            |. symbol "["
            |= DtdParser.parser
            |. symbol "]"


cdata : Parser String
cdata =
    inContext "cdata" <|
        succeed identity
            |. symbol "<![CDATA["
            |= (getChompedString <| chompUntil (toToken "]]>"))
            |. symbol "]]>"


element : Dict String String -> Parser Node
element entities =
    inContext "element" <|
        succeed identity
            |. symbol "<"
            |= (tagName
                    |> andThen
                        (\startTagName ->
                            succeed (Element startTagName)
                                |. whiteSpace
                                |= attributes entities
                                |. whiteSpace
                                |= oneOf
                                    [ succeed []
                                        |. symbol "/>"
                                    , succeed identity
                                        |. symbol ">"
                                        |= lazy (\_ -> children entities startTagName)
                                    ]
                        )
               )


tagName : Parser String
tagName =
    inContext "tagName" <|
        keep oneOrMore
            (\c ->
                case c of
                    '/' ->
                        False

                    '<' ->
                        False

                    '>' ->
                        False

                    '"' ->
                        False

                    '\'' ->
                        False

                    '=' ->
                        False

                    ' ' ->
                        False

                    '\u{000D}' ->
                        False

                    '\n' ->
                        False

                    '\t' ->
                        False

                    _ ->
                        True
            )


children : Dict String String -> String -> Parser (List Node)
children entities startTagName =
    inContext "children" <|
        loop []
            (\acc ->
                oneOf
                    [ succeed (Advanced.Done <| List.reverse acc)
                        |. closingTag startTagName
                    , textNodeString entities
                        |> andThen
                            (\maybeString ->
                                case maybeString of
                                    Just s ->
                                        succeed (Advanced.Loop <| Text s :: acc)

                                    Nothing ->
                                        succeed (Advanced.Done <| List.reverse acc)
                                            |. closingTag startTagName
                            )
                    , lazy
                        (\_ ->
                            succeed (\e -> Advanced.Loop <| e :: acc)
                                |= element entities
                        )
                    ]
            )


closingTag : String -> Parser ()
closingTag startTagName =
    inContext "closingTag" <|
        succeed ()
            |. symbol "</"
            |. whiteSpace
            |. (tagName
                    |> andThen
                        (\endTagName ->
                            if startTagName == endTagName then
                                succeed ()

                            else
                                fail ("tag name mismatch: " ++ startTagName ++ " and " ++ endTagName)
                        )
               )
            |. whiteSpace
            |. symbol ">"


textNodeString : Dict String String -> Parser (Maybe String)
textNodeString entities =
    inContext "textNodeString" <|
        loop Nothing
            (\acc ->
                oneOf
                    [ succeed
                        (\c ->
                            Advanced.Loop <| Just (c :: Maybe.withDefault [] acc)
                        )
                        |= escapedChar entities '<'
                    , succeed
                        (\s ->
                            Advanced.Loop <|
                                if String.isEmpty s then
                                    acc

                                else
                                    Just (s :: Maybe.withDefault [] acc)
                        )
                        |= cdata
                    , succeed (Advanced.Loop acc)
                        |. comment
                    , succeed
                        (\s ->
                            Advanced.Loop <| Just (s :: Maybe.withDefault [] acc)
                        )
                        |= keep oneOrMore
                            (\c ->
                                case c of
                                    '<' ->
                                        False

                                    '&' ->
                                        False

                                    _ ->
                                        True
                            )
                    , succeed <|
                        Advanced.Done <|
                            Maybe.map
                                (\segments ->
                                    segments
                                        |> List.reverse
                                        |> String.concat
                                )
                                acc
                    ]
            )


attributes : Dict String String -> Parser (List Attribute)
attributes entities =
    inContext "attributes" <|
        loop ( Set.empty, [] ) <|
            \( keys, acc ) ->
                oneOf
                    [ attribute entities
                        |> andThen
                            (\attr ->
                                if Set.member attr.name keys then
                                    fail ("attribute " ++ attr.name ++ " is duplicated")

                                else
                                    succeed
                                        (Advanced.Loop
                                            ( Set.insert attr.name keys, attr :: acc )
                                        )
                                        |. whiteSpace
                            )
                    , succeed <| Advanced.Done <| List.reverse acc
                    ]


attribute : Dict String String -> Parser Attribute
attribute entities =
    inContext "attribute" <|
        succeed Attribute
            |= attributeName
            |. whiteSpace
            |. symbol "="
            |. whiteSpace
            |= attributeValue entities



-- FORMAT


{-| Convert Xml into String.

This function does NOT insert line breaks or indents for readability.

-}
format : Xml -> String
format doc =
    let
        pi : String
        pi =
            doc.processingInstructions
                |> List.map formatProcessingInstruction
                |> String.concat

        dt : String
        dt =
            doc.docType
                |> Maybe.map formatDocType
                |> Maybe.withDefault ""

        node : String
        node =
            formatNode doc.root
    in
    pi ++ dt ++ node


formatProcessingInstruction : ProcessingInstruction -> String
formatProcessingInstruction processingInstruction_ =
    "<?" ++ escape processingInstruction_.name ++ " " ++ processingInstruction_.value ++ "?>"


formatDocType : DocType -> String
formatDocType docType_ =
    "<!DOCTYPE " ++ escape docType_.rootElementName ++ " " ++ formatDocTypeDefinition docType_.definition ++ ">"


formatDocTypeDefinition : DtdParser.DocTypeDefinition -> String
formatDocTypeDefinition def =
    case def of
        DtdParser.Public publicIdentifier_ internalSubsetRef maybeInternalSubset ->
            "PUBLIC \""
                ++ escape publicIdentifier_
                ++ "\" \""
                ++ escape internalSubsetRef
                ++ "\""
                ++ (case maybeInternalSubset of
                        Just internalSubset ->
                            " [" ++ DtdParser.format internalSubset ++ "]"

                        Nothing ->
                            ""
                   )

        DtdParser.System internalSubsetRef maybeInternalSubset ->
            "SYSTEM \""
                ++ escape internalSubsetRef
                ++ "\""
                ++ (case maybeInternalSubset of
                        Just internalSubset ->
                            " [" ++ DtdParser.format internalSubset ++ "]"

                        Nothing ->
                            ""
                   )

        DtdParser.Custom internalSubset ->
            "[" ++ DtdParser.format internalSubset ++ "]"


formatNode : Node -> String
formatNode node =
    case node of
        Element tagName_ attributes_ children_ ->
            "<"
                ++ escape tagName_
                ++ " "
                ++ (attributes_ |> List.map formatAttribute |> String.join " ")
                ++ (if children_ == [] then
                        "/>"

                    else
                        ">"
                            ++ (children_ |> List.map formatNode |> String.concat)
                            ++ "</"
                            ++ escape tagName_
                            ++ ">"
                   )

        Text s ->
            escape s


formatAttribute : Attribute -> String
formatAttribute attribute_ =
    escape attribute_.name ++ "=\"" ++ escape attribute_.value ++ "\""
