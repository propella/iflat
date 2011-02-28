import Test.HUnit
import Language

-- putStr . pprint . parse $ srcLet
srcLet =
  "f  = let\n\
  \  a = x\n\
  \in let\n\
  \  b = a\n\
  \in b\n"


-- putStr . pprint . parse $ exercise21
exercise21 = "f  = 3;\n\
             \g x y = let\n\
             \  z = x\n\
             \in z;\n\
             \h x = case let\n\
             \  y = x\n\
             \in y of\n\
             \  <1>  -> 2;\n\
             \  <2>  -> 5\n"

-- putStr . pprint . parse $ exercise22
exercise22 = "f x y = case x of\n\
             \  <1>  -> case y of\n\
             \    <1>  -> 1;\n\
             \    <2>  -> 2\n"

ipprExpr :: [(CoreExpr, [Token])] -> String
ipprExpr ((expr, []) :_)  = iDisplay . pprExpr $ expr

main = runTestTT allTest

parseAndPrintTest = test [
    "let" ~: (pprint . parse $ srcLet) ~?= srcLet
    ,"let and case" ~: (pprint . parse $ exercise21) ~?= exercise21
    ,"case" ~: (pprint . parse $ exercise22) ~?= exercise22
                    ]

allTest = test [
           "lexer" ~: lexTest
           ,"print" ~: printTest
           ,"parser" ~: parserTest
           ,"parse and print" ~: parseAndPrintTest
          ]

printTest = test [
           "str" ~: (iDisplay $ pprExpr $ EVar "hello") ~?= "hello"
           ,"num" ~: (iDisplay $ pprExpr $ ENum 7) ~?= "7"
           ,"app" ~: (iDisplay $ pprExpr $ (EAp (EVar "f") (EVar "x"))) ~?= "f x"
           ,"op" ~: (iDisplay $ pprExpr $ (EAp (EAp (EVar "+") (ENum 3)) (ENum 4))) ~?= "3 + 4"
--            ,"case" ~: (iDisplay $ pprExpr $ (ECase (EVar "a") [(1,[],EVar "a"),(2,["x"],EVar "x")])) ~?= ""
           ]

lexTest = test [
           "lex" ~: clex "1 234 hello_world <=" ~?= ["1","234","hello_world","<="]
           ,"binop" ~: clex "== ~= >= <= ->" ~?= ["==", "~=", ">=", "<=", "->"]
           ,"constr" ~: clex "Pack{1,2}" ~?= ["Pack", "{", "1", ",", "2", "}"]
          ]

parserTest = test [
             "pZeroOrMore" ~: pZeroOrMore pVar ["hello", "world"]
              ~?= [(["hello","world"],[]),(["hello"],["world"]),([],["hello","world"])]
             ,"pNum" ~: pENum ["123"] ~?= [(ENum 123, [])]
             ,"pEVar" ~: pEVar ["hello"] ~?= [(EVar "hello", [])]
             ,"pEConstr" ~: pEConstr ["Pack", "{", "1", ",", "2", "}"] ~?= [(EConstr 1 2, [])]
             ,"pParen" ~: pParenthesis ["(", "5", ")"] !! 0 ~?= (ENum 5, [])
             ,"pELet" ~: pELet (clex "let x = 1 in y") !! 0 ~?= (ELet False [("x", ENum 1)] (EVar "y"), [])

             ,"pAlternative" ~: pAlternative (clex "<1> x y -> x") !! 1 ~?= ((1,["x","y"],EVar "x"),[])
             ,"pAlternatives" ~: pAlternatives (clex "<1> x -> a; <2> -> b") !! 1
               ~?= ([(1,["x"],EVar "a"),(2,[],EVar "b")],[])
             ,"pECase" ~: pECase (clex "case a of <1> -> a; <2> x -> x") !! 0
               ~?= (ECase (EVar "a") [(1,[],EVar "a"),(2,["x"],EVar "x")],[])

             ,"pELam" ~: pELam (clex "\\x y z. z") !! 0 ~?= (ELam ["x", "y", "z"] (EVar "z"), [])

             ,"op" ~: pExpr4 (clex "3 + 4 + 5") !! 0 ~?=
                       (EAp (EAp
                             (EVar "+") (EAp (EAp (EVar "+") (ENum 3)) (ENum 4)))
                                  (ENum 5), [])
             ,"op" ~: pExpr4 (clex "3 + 4 * 5") !! 0 ~?=
                       (EAp (EAp (EVar "+") (ENum 3))
                                (EAp (EAp (EVar "*") (ENum 4)) (ENum 5)),[])
             ,"op" ~: (ipprExpr $ pExpr4 (clex "3 + 4 + 5")) ~?=
                       "(3 + 4) + 5"
             ,"op" ~: (ipprExpr $ pExpr4 (clex "3 + 4 + 5 * 6 * 7")) ~?=
                       "(3 + 4) + ((5 * 6) * 7)"
             ]
