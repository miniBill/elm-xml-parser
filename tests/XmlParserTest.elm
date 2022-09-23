module XmlParserTest exposing (suite)

import Expect exposing (Expectation)
import Parser
import Parser.Advanced
import Test exposing (Test, describe, test)
import XmlParser exposing (Attribute, DocType, DocTypeDefinition(..), Node(..), ProcessingInstruction, Xml)


suite : Test
suite =
    describe "XmlParser"
        [ singleTagTests
        , attributeTests
        , childrenTests
        , test "no root" <| expectFail ""
        , test "many roots" <| expectFail "<a/><a/>"
        , processingInstructionTests
        , docTypeTests
        , cdataTests
        , whitespaceTests
        , commentTests
        , formatTests
        ]


singleTagTests : Test
singleTagTests =
    describe "Single tag parsing"
        [ test "a" <| expectSucceed "<a/>" (Element "a" [] [])
        , test "number" <| expectSucceed "<1/>" (Element "1" [] [])
        , test "unicode 1" <| expectSucceed "<あ/>" (Element "あ" [] [])
        , test "unicode 2" <| expectSucceed "<😄/>" (Element "😄" [] [])
        , test "surrogate pairs" <| expectSucceed "<𩸽/>" (Element "𩸽" [] [])
        , test "namespace" <| expectSucceed "<a:b/>" (Element "a:b" [] [])
        , describe "fail"
            [ test "No tag name" <| expectFail "</>"
            , test "No closing tag (a)" <| expectFail "<a>"
            , test "No closing tag (1)" <| expectFail "<1>"
            , test "Wrong closing tag" <| expectFail "<a></b>"
            ]
        ]


attributeTests : Test
attributeTests =
    describe "Attributes"
        [ test "1" <| expectSucceed """<a b=""/>""" (Element "a" [ Attribute "b" "" ] [])
        , test "2" <| expectSucceed """<a b="1=</>"/>""" (Element "a" [ Attribute "b" "1=</>" ] [])
        , test "quote 1" <| expectSucceed """<a b='""'/>""" (Element "a" [ Attribute "b" "\"\"" ] [])
        , test "quote 2" <| expectSucceed """<a b="''"/>""" (Element "a" [ Attribute "b" "''" ] [])
        , test "key number" <| expectSucceed """<a 1=""/>""" (Element "a" [ Attribute "1" "" ] [])
        , test "key unicode 1" <| expectSucceed """<a あ=""/>""" (Element "a" [ Attribute "あ" "" ] [])
        , test "key unicode 2" <| expectSucceed """<a 😄=""/>""" (Element "a" [ Attribute "😄" "" ] [])
        , test "key surrogate pairs" <| expectSucceed """<a 𩸽=""/>""" (Element "a" [ Attribute "𩸽" "" ] [])
        , test "key escape (issue #12, 13)" <| expectSucceed """<a attr="a&amp;b"></a>""" (Element "a" [ Attribute "attr" "a&b" ] [])
        , test "key namespace" <| expectSucceed """<a b:c=""/>""" (Element "a" [ Attribute "b:c" "" ] [])
        , test "value escape 1" <| expectSucceed """<a a="&quot;"/>""" (Element "a" [ Attribute "a" "\"" ] [])
        , describe "fail"
            [ test "No value" <| expectFail """<a a=/>"""
            , test "No equal before quotes" <| expectFail """<a a"="/>"""
            , test "No attribute name" <| expectFail """<a=""/>"""
            , test "Empty attribute" <| expectFail """<a b c=""/>"""
            , test "Misplaced equal after tag name" <| expectFail """<a= b=""/>"""
            , test "Same names" <| expectFail """<a b="" b=""/>"""
            ]
        ]


childrenTests : Test
childrenTests =
    describe "Children"
        [ test "text" <| expectSucceed "<a>1</a>" (Element "a" [] [ Text "1" ])
        , test "text escape 1" <| expectSucceed "<a>&amp;</a>" (Element "a" [] [ Text "&" ])
        , test "text escape 2" <| expectSucceed "<a>&#x41;</a>" (Element "a" [] [ Text "A" ])
        , test "text escape 3" <| expectSucceed "<a>&#65;</a>" (Element "a" [] [ Text "A" ])
        , test "text escape fail" <| expectFail "<a>&&;</a>"
        , test "element 1" <| expectSucceed "<a><b/></a>" (Element "a" [] [ Element "b" [] [] ])
        , test "element 2" <| expectSucceed "<a><b/><c></c></a>" (Element "a" [] [ Element "b" [] [], Element "c" [] [] ])
        , test "element 3" <|
            expectSucceed "<a>1<b/>2<c>3</c>4</a>"
                (Element "a" [] [ Text "1", Element "b" [] [], Text "2", Element "c" [] [ Text "3" ], Text "4" ])
        , test "element nested same tag" <| expectSucceed "<a><a></a></a>" (Element "a" [] [ Element "a" [] [] ])
        , test "element fail" <| expectFail "<a><b></a></a>"
        ]


processingInstructionTests : Test
processingInstructionTests =
    describe "Processing instruction"
        [ test "0" <| expectPI """<?xml ?><a/>""" [ ProcessingInstruction "xml" "" ]
        , test "1" <| expectPI """<?xml a?><a/>""" [ ProcessingInstruction "xml" "a" ]
        , test "2" <| expectPI """<?xml a="b" c="d"?><a/>""" [ ProcessingInstruction "xml" "a=\"b\" c=\"d\"" ]
        , test "3" <| expectPI """<?xml ??><a/>""" [ ProcessingInstruction "xml" "?" ]
        , test "4" <| expectPI """<?xml 1?2?><a/>""" [ ProcessingInstruction "xml" "1?2" ]
        , test "multiple 1" <| expectPI """<?xml ?><?xml ?><a/>""" [ ProcessingInstruction "xml" "", ProcessingInstruction "xml" "" ]
        , test "multiple 2" <| expectPI """<?xml ?>\u{000D}\t <?xml ?><a/>""" [ ProcessingInstruction "xml" "", ProcessingInstruction "xml" "" ]
        ]


docTypeTests : Test
docTypeTests =
    describe "DocType"
        [ test "public 1" <| expectDocType """<!DOCTYPE a PUBLIC "" ""><a/>""" (Just (DocType "a" (Public "" "" Nothing)))
        , test "public 2" <| expectDocType """<!DOCTYPE a PUBLIC "1" "2"><a/>""" (Just (DocType "a" (Public "1" "2" Nothing)))
        , test "public 3" <| expectDocType """<!DOCTYPE a PUBLIC "" ""[]><a/>""" (Just (DocType "a" (Public "" "" (Just ""))))
        , test "public 4" <| expectDocType """<!DOCTYPE a PUBLIC "" ""[a]><a/>""" (Just (DocType "a" (Public "" "" (Just "a"))))
        , test "public fail 1" <| expectFail """<!DOCTYPE a PUBLIC ""><a/>"""
        , test "public fail 2" <| expectFail """<!DOCTYPE PUBLIC "" ""><a/>"""
        , test "public fail 3" <| expectFail """<!DOCTYPEPUBLIC "" ""><a/>"""
        , test "system 1" <| expectDocType """<!DOCTYPE a SYSTEM ""><a/>""" (Just (DocType "a" (System "" Nothing)))
        , test "system 2" <| expectDocType """<!DOCTYPE a SYSTEM "1"><a/>""" (Just (DocType "a" (System "1" Nothing)))
        , test "system 3" <| expectDocType """<!DOCTYPE a SYSTEM "" []><a/>""" (Just (DocType "a" (System "" (Just ""))))
        , test "system 4" <| expectDocType """<!DOCTYPE a SYSTEM "" [a]><a/>""" (Just (DocType "a" (System "" (Just "a"))))
        , test "system fail 1" <| expectFail """<!DOCTYPE a SYSTEM []><a/>"""
        , test "system fail 2" <| expectFail """<!DOCTYPE SYSTEM "" []><a/>"""
        , test "system fail 3" <| expectFail """<!DOCTYPESYSTEM "" []><a/>"""
        , test "custom 1" <| expectDocType """<!DOCTYPE a []><a/>""" (Just (DocType "a" (Custom "")))
        , test "custom 2" <| expectDocType """<!DOCTYPE a [a]><a/>""" (Just (DocType "a" (Custom "a")))
        , test "custom fail 1" <| expectFail """<!DOCTYPE a "" []><a/>"""
        , test "custom fail 2" <| expectFail """<!DOCTYPE []><a/>"""
        , test "whitespace" <| expectDocType "<!DOCTYPE\na\nPUBLIC\n\"\"\n\"\"><a/>" (Just (DocType "a" (Public "" "" Nothing)))
        ]


cdataTests : Test
cdataTests =
    describe "Cdata"
        [ test "1" <| expectSucceed "<a><![CDATA[]]></a>" (Element "a" [] [])
        , test "2" <| expectSucceed "<a>a<![CDATA[]]></a>" (Element "a" [] [ Text "a" ])
        , test "3" <| expectSucceed "<a><![CDATA[b]]></a>" (Element "a" [] [ Text "b" ])
        , test "4" <| expectSucceed "<a><![CDATA[]]>c</a>" (Element "a" [] [ Text "c" ])
        , test "5" <| expectSucceed "<a>a<![CDATA[b]]></a>" (Element "a" [] [ Text "ab" ])
        , test "6" <| expectSucceed "<a><![CDATA[b]]>c</a>" (Element "a" [] [ Text "bc" ])
        , test "7" <| expectSucceed "<a>a<![CDATA[]]>c</a>" (Element "a" [] [ Text "ac" ])
        , test "8" <| expectSucceed "<a>a<![CDATA[b]]>c</a>" (Element "a" [] [ Text "abc" ])
        , test "9" <| expectSucceed "<a><![CDATA[a[b]c]]></a>" (Element "a" [] [ Text "a[b]c" ])
        , test "10" <| expectSucceed "<a><![CDATA[[b]c]]></a>" (Element "a" [] [ Text "[b]c" ])
        , test "11" <| expectSucceed "<a><![CDATA[a[b]]]></a>" (Element "a" [] [ Text "a[b]" ])
        , test "12" <| expectSucceed "<a><![CDATA[a[[b]]c]]></a>" (Element "a" [] [ Text "a[[b]]c" ])
        , test "13" <| expectSucceed "<a><![CDATA[ab<![CDATA[cd]]></a>" (Element "a" [] [ Text "ab<![CDATA[cd" ])
        , test "14" <| expectSucceed "<a>ab<![CDATA[cd<![CDATA[ef]]>gh]]></a>" (Element "a" [] [ Text "abcd<![CDATA[efgh]]>" ])
        , test "fail 1" <| expectFail "<a><![CDATA[</a>"
        , test "fail 2" <| expectFail "<a><![CDATA[]</a>"
        , test "fail 3" <| expectFail "<a><![CDATA[]]</a>"
        , test "fail 4" <| expectFail "<a><![CDATA[abc</a>"
        , test "fail 5" <| expectFail "<a>abc<![CDATA[</a>"
        , test "fail 6" <| expectFail "<a>abc<![CDATA[def</a>"
        ]



{-
   For referrence
   http://www.oracle.com/technetwork/articles/wang-whitespace-092897.html
-}


whitespaceTests : Test
whitespaceTests =
    describe "Whitespace"
        [ test "1" <| expectSucceed "\u{000D}\n\t <?xml ?>\u{000D}\n\t <!DOCTYPE a []>\u{000D}\n\t <a/>\u{000D}\n\t " (Element "a" [] [])
        , test "2" <|
            expectSucceed "<a\u{000D}\n\tb\u{000D}\n\t=\u{000D}\n\t\"c\"\u{000D}\n\td\u{000D}\n\t=\u{000D}\n\t\"e\"/>"
                (Element "a" [ Attribute "b" "c", Attribute "d" "e" ] [])
        , test "3" <| expectSucceed "<a></a>" (Element "a" [] [])
        , test "4" <| expectSucceed "<a> </a>" (Element "a" [] [ Text " " ])
        , test "5" <| expectSucceed "<a>\u{000D}\n\t</a>" (Element "a" [] [ Text "\u{000D}\n\t" ])
        , test "6" <| expectSucceed "<a><![CDATA[ ]]></a>" (Element "a" [] [ Text " " ])
        , test "7" <| expectSucceed "<a> <![CDATA[]]> </a>" (Element "a" [] [ Text "  " ])
        , test "8" <| expectSucceed "<a> <![CDATA[ ]]> </a>" (Element "a" [] [ Text "   " ])
        , test "9" <| expectSucceed "<a>\n<![CDATA[\n]]>\n</a>" (Element "a" [] [ Text "\n\n\n" ])
        ]


commentTests : Test
commentTests =
    describe "Comments"
        [ test "comment 1" <| expectSucceed "<a>a<!--b-->c</a>" (Element "a" [] [ Text "ac" ])
        , test "comment 2" <| expectSucceed "<a><!----></a>" (Element "a" [] [])
        , test "comment 3" <| expectFail "<a><!---></a>"
        , test "comment 4" <| expectSucceed "<a><!-----------></a>" (Element "a" [] [])
        , test "comment 5" <| expectSucceed "<!DOCTYPE a []><!----><a/><!---->" (Element "a" [] [])
        , test "comment 6" <| expectSucceed "<!----><!DOCTYPE a []><!----><a/><!---->" (Element "a" [] [])
        ]


formatTests : Test
formatTests =
    describe "Format"
        [ test "1" <|
            testFormat
                (Xml [] Nothing <|
                    Element "a" [ Attribute "b" "c", Attribute "d" "e" ] [ Element "f" [] [], Text "g", Element "h" [] [] ]
                )
        , test "2" <| testFormat (Xml [] Nothing <| Element "a" [] [])
        , test "3" <| testFormat (Xml [] Nothing <| Element "😄" [ Attribute "😄" "&><'\"" ] [ Text "&><'\"" ])
        , test "4" <| testFormat (Xml [] (Just (DocType "1" <| Public "a" "b" Nothing)) <| Element "a" [] [])
        , test "5" <| testFormat (Xml [] (Just (DocType "1" <| Public "a" "b" (Just "c"))) <| Element "a" [] [])
        , test "6" <| testFormat (Xml [] (Just (DocType "1" <| System "a" Nothing)) <| Element "a" [] [])
        , test "7" <| testFormat (Xml [] (Just (DocType "1" <| System "a" (Just "b"))) <| Element "a" [] [])
        , test "8" <| testFormat (Xml [] (Just (DocType "1" <| Custom "")) <| Element "a" [] [])
        , test "9" <|
            testFormat
                (Xml
                    [ ProcessingInstruction "a" "b"
                    , ProcessingInstruction "c" "d"
                    ]
                    Nothing
                    (Element "a" [] [])
                )
        , test "10" <|
            testFormat
                (Xml
                    [ ProcessingInstruction "xml" "version=\"1.0\"" ]
                    Nothing
                    (Element "a" [] [])
                )
        ]


expectPI : String -> List ProcessingInstruction -> (() -> Expectation)
expectPI source pis _ =
    case XmlParser.parse source of
        Ok ast ->
            Expect.equal ast.processingInstructions pis

        Err e ->
            Expect.fail (deadEndsToString e)


expectDocType : String -> Maybe DocType -> (() -> Expectation)
expectDocType source docType _ =
    case XmlParser.parse source of
        Ok ast ->
            Expect.equal ast.docType docType

        Err e ->
            Expect.fail (deadEndsToString e)


expectSucceed : String -> Node -> (() -> Expectation)
expectSucceed source node _ =
    case XmlParser.parse source of
        Ok ast ->
            Expect.equal ast.root node

        Err e ->
            Expect.fail (deadEndsToString e)


expectFail : String -> (() -> Expectation)
expectFail source _ =
    case XmlParser.parse source of
        Ok ast ->
            Expect.fail ("Unexpectedly succeeded: " ++ XmlParser.format ast)

        Err _ ->
            Expect.pass


testFormat : Xml -> (() -> Expectation)
testFormat xml _ =
    XmlParser.format xml
        |> XmlParser.parse
        |> (\result ->
                case result of
                    Ok xml2 ->
                        Expect.equal xml xml2

                    Err _ ->
                        Expect.fail ""
            --(Parser.deadEndsToString e)
           )


deadEndsToString : List (Parser.Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadends =
    String.join " " (List.map deadEndToString deadends)


deadEndToString : { row : Int, col : Int, problem : Parser.Problem, contextStack : List { row : Int, col : Int, context : String } } -> String
deadEndToString { row, col, problem } =
    "{ row = " ++ String.fromInt row ++ ", col = " ++ String.fromInt col ++ ", " ++ Debug.toString problem ++ "}"
