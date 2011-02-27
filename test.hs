import Test.HUnit
import Language

main = runTestTT allTest

allTest = test [
           "lexer" ~: lexTest
           ,"parser" ~: parserTest
          ]

lexTest = test [
           "lex" ~: clex "1 234 hello_world <=" ~?= ["1","234","hello_world","<="]
           ,"binop" ~: clex "== ~= >= <= ->" ~?= ["==", "~=", ">=", "<=", "->"]
           ,"constr" ~: clex "Pack{1,2}" ~?= ["Pack", "{", "1", ",", "2", "}"]
          ]

parserTest = test [
              "pNum" ~: pENum ["123"] ~?= [(ENum 123, [])]
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
             ]
