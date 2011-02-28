import Test.HUnit
import Language

ipprExpr :: [(CoreExpr, [Token])] -> String
ipprExpr ((expr, []) :_)  = iDisplay . pprExpr $ expr

main = runTestTT allTest

allTest = test [
           "lexer" ~: lexTest
           ,"print" ~: printTest
           ,"parser" ~: parserTest
          ]

lexTest = test [
           "lex" ~: clex "1 234 hello_world <=" ~?= ["1","234","hello_world","<="]
           ,"binop" ~: clex "== ~= >= <= ->" ~?= ["==", "~=", ">=", "<=", "->"]
           ,"constr" ~: clex "Pack{1,2}" ~?= ["Pack", "{", "1", ",", "2", "}"]
          ]

printTest = test [
           "str" ~: (iDisplay $ pprExpr $ EVar "hello") ~?= "hello"
           ,"num" ~: (iDisplay $ pprExpr $ ENum 7) ~?= "7"
           ,"app" ~: (iDisplay $ pprExpr $ (EAp (EVar "f") (EVar "x"))) ~?= "f x"
           ,"op" ~: (iDisplay $ pprExpr $ (EAp (EAp (EVar "+") (ENum 3)) (ENum 4))) ~?= "3 + 4"
           ]

parserTest = test [
             "pZeroOrMore" ~: pZeroOrMore pVar ["hello", "world"]
              ~?= [(["hello","world"],[]),(["hello"],["world"]),([],["hello","world"])]
             ,"pNum" ~: pENum ["123"] ~?= [(ENum 123, [])]
             ,"pEVar" ~: pEVar ["hello"] ~?= [(EVar "hello", [])]
             ,"pEConstr" ~: pEConstr ["Pack", "{", "1", ",", "2", "}"] ~?= [(EConstr 1 2, [])]
             ,"pParen" ~: pParenthesis ["(", "5", ")"] !! 0 ~?= (ENum 5, [])
             ,"pELet" ~: pELet (clex "let x = 1 in y") !! 0 ~?= (ELet False [("x", ENum 1)] (EVar "y"), [])
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
