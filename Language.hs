module Language where
import Utils
import Char (isDigit, isAlpha)

-- 1.3 Data types for the Core language

data Expr a = EVar Name
            | ENum Int
            | EConstr Int Int
            | EAp (Expr a) (Expr a)
            | ELet
                IsRec
                [(a, Expr a)]
                (Expr a)
            | ECase
                (Expr a)
                [Alter a]
            | ELam [a] (Expr a)
              deriving (Show)

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive	= True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf	:: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e	= False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- 1.4 A small standard prelude
-- I x = x
-- K x y = x
-- K1 x y = y 
-- S f g x = f x (g x) 
-- compose f g x = f (g x) 
-- twice f = compose f f

preludeDefs :: CoreProgram
preludeDefs
    = [ ("I", ["x"], EVar "x"),
        ("K", ["x","y"], EVar "x"),
        ("K1",["x","y"], EVar "y"),
        ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                                 (EAp (EVar "g") (EVar "x"))), 
        ("compose", ["f","g","x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x"))), 
        ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

-- 1.5.1 Pretty-printing using strings
-- pprExpr (EAp (EVar "f") (EVar "x"))

-- pprExpr :: CoreExpr -> String
-- pprExpr (ENum n) = show n
-- pprExpr (EVar v) = v
-- pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2

-- pprAExpr :: CoreExpr -> String
-- pprAExpr e | isAtomicExpr e = pprExpr e
--            | otherwise = "(" ++ pprExpr e ++ ")"

-- mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
-- mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
--     where e2s = e2 : e2s

-- 1.5.2 An abstract data type for pretty-printing (p23)

iNil :: Iseq			-- The empty iseq
iStr :: String -> Iseq		-- Turn a string into an iseq
iAppend :: Iseq -> Iseq -> Iseq	-- Append two iseqs
iNewline :: Iseq		-- New line with indentation
iIndent :: Iseq -> Iseq		-- Indent an iseq
iDisplay :: Iseq -> String	-- Turn an iseq into a string

pprExpr :: CoreExpr -> Iseq

pprExpr (EVar v) = iStr v

pprExpr (EAp (EAp (EVar "+") e1) e2) -- (p27)
    = iConcat [ pprAExpr e1, iStr " + ", pprAExpr e2 ]

pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)

pprExpr (ELet isrec defns expr)
        = iConcat [ iStr keyword, iNewline,
                    iStr "  ", iIndent (pprDefns defns), iNewline,
                    iStr "in ", pprExpr expr ]
          where
            keyword | not isrec = "let"
                    | isrec = "letrec"

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where
                   sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

iConcat	:: [Iseq] -> Iseq
iInterleave :: Iseq -> [Iseq] -> Iseq

-- Exercise 1.2 (p25)

iConcat [] = iNil
iConcat (x:xs) = x `iAppend` iConcat xs

iInterleave _ [] = iNil
iInterleave _ [x] = x
iInterleave sep (x:xs) = x `iAppend` sep `iAppend` iInterleave sep xs

-- pprint prog = iDisplay (pprProgram prog)

-- Exercise 1.3 (p25)

pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

-- 1.5.3 Implementing iseq (p25)

-- data Iseq = INil
-- 	  | IStr String
-- 	  | IAppend Iseq Iseq

iNil			= INil
iAppend seq1 seq2	= IAppend seq1 seq2
iStr str		= IStr str

-- iIndent seq = seq
-- iNewline = IStr "\n"

-- flatten :: [Iseq] -> String

-- iDisplay seq = flatten [seq]

-- flatten [] = ""
-- flatten (INil : seqs) = flatten seqs
-- flatten (IStr s : seqs) = s ++ (flatten seqs)
-- flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)

-- > putStrLn $ iDisplay $ IStr "hello"
-- > putStrLn $ iDisplay $ pprExpr (EAp (EVar "func") (EVar "x"))
-- > putStrLn $ iDisplay $ pprExpr (ELet True [("x", (EVar "a")),("y", (EVar "b"))] (EVar "x"))

-- 1.5.4 Layout and indentation (p26)

data Iseq = INil
	  | IStr String
          | IAppend Iseq Iseq
	  | IIndent Iseq
          | INewline

iIndent seq	= IIndent seq
iNewline	= INewline

flatten :: Int			-- Current column; 0 for first column
	-> [(Iseq, Int)]	-- Work list
	-> String		-- Result

iDisplay seq = flatten 0 [(seq,0)]

flatten col ((INewline, indent) : seqs)
    = '\n' : (space indent) ++ (flatten indent seqs)

flatten col ((IIndent seq, indent) : seqs)
    = flatten col ((seq, col + 2) : seqs)

-- Exercise 1.6 (p27)

flatten _ [] = ""
flatten col ((INil, indent) : seqs)
    = flatten col seqs
flatten col ((IStr s, indent) : seqs)
    = s ++ (flatten col seqs)
flatten col ((IAppend seq1 seq2, indent) : seqs)
    = flatten col ((seq1, indent) : (seq2, indent) : seqs)

-- 1.5.6 Other useful functions on iseq (28)

iNum :: Int -> Iseq
iNum n = iStr (show n)

-- > iDisplay (iFWNum 5 100)
iFWNum :: Int -> Int -> Iseq
iFWNum width n
    = iStr (space (width - length digits) ++ digits)
      where
        digits = show n

-- > putStr . iDisplay . iLayn $ [iNum 10, iNum 20, iNum 30]
iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
    where lay_item (n, seq)
              = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]

-- 1.6 A parser for the Core language (p29)

clex :: String -> [Token]

syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
parse = syntax . clex

-- 1.6.1 Lexical analysis (p30)

type Token = String -- A token is never empty

-- > clex "1 234 hello_world <="
clex (c:cs) | isWhiteSpace c = clex cs

clex (c:cs) | isDigit c = num_token : clex rest_cs
            where
              num_token = c : takeWhile isDigit cs
              rest_cs	= dropWhile isDigit cs

clex (c:cs) | isAlpha c = var_tok : clex rest_cs
            where
              var_tok = c : takeWhile isIdChar cs
              rest_cs = dropWhile isIdChar cs

clex (c:cs) = [c] : clex cs

clex [] = []

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

-- 1.6.2 Basic tools for parsing (p32)

-- The parser takes tokens and returns a list of results (can be ambiguous).
-- It returns [] if it does not match.
type Parser a = [Token] -> [(a, [Token])]

-- > pLit "hello" ["hello", "John", "!"]
pLit :: String -> Parser String
-- pLit s (tok:toks) | s == tok    = [(s, toks)]
--                   | otherwise   = []
-- pLit s []                       = []

pVar :: Parser String
-- pVar []	= []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

-- > pHelloOrGoodbye . clex $ "goodbye"
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
    = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
                                 (v2,toks2) <- p2 toks1]

-- pGreeting :: Parser (String, String)
-- pGreeting = pThen mk_pair pHelloOrGoodbye pVar
--     where
--       mk_pair hg name = (hg, name)

-- 1.6.3 Sharpening the tools (p34)

-- pGreeting = pThen keep_first
--                   (pThen mk_pair pHelloOrGoodbye pVar)
--                   (pLit "!")
--     where
--       keep_first hg_name exclamation = hg_name
--       mk_pair hg name = (hg, name)

-- > pGreeting ["goodbye", "James", "!"]
pGreeting = pThen3 mk_greeting
                   pHelloOrGoodbye
                   pVar
                   (pLit "!")
    where
      mk_greeting hg name exclamation = (hg, name)

-- Exercise 1.12 (35). 

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
    = [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks,
                                    (v2, toks2) <- p2 toks1,
                                    (v3, toks3) <- p3 toks2]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4
    = pThen ($) (pThen ($) (pThen combine p1 p2) p3) p4

pZeroOrMore :: Parser a -> Parser [a]

-- > pGreeting ["goodbye", "James", "!"]
-- pGreetings :: Parser [(String, String)]
-- pGreetings = pZeroOrMore pGreeting

-- > pZeroOrMore pGreeting $ ["goodbye", "James", "!", "goodbye", "James", "!"]
-- -- [([("goodbye","James"),("goodbye","James")],[]),([("goodbye","James")],["goodbye","James","!"]),([],["goodbye","James","!","goodbye","James","!"])]
-- This returns all possible ambiguous results.
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pOneOrMore :: Parser a -> Parser [a]

-- Exercise 1.13 (p35). 

-- > pEmpty "nil" $ ["hello"] -- [("nil", ["hello"])]
pEmpty v toks = [(v, toks)]

pOneOrMore p = pThen (:) p (pZeroOrMore p)

-- > pGreetingsN $ ["goodbye", "James", "!", "goodbye", "James", "!"]
-- -- [(2,[]),(1,["goodbye","James","!"]),(0,["goodbye","James","!","goodbye","James","!"])]
pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

-- Exercise 1.14. (p36)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [ (f v, toks') | (v, toks') <- p toks ]

-- Exercise 1.15. (p36)

-- > (pOneOrMoreWithSep pVar (pLit ",")) $ ["apple", ",", "orange", ",", "banana"]
-- -- [(["apple","orange","banana"],[]),(["apple","orange"],[",","banana"]),(["apple"],[",","orange",",","banana"])]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

pOneOrMoreWithSep p1 p2
    = pThen (:) p1 (pOneOrMoreWithSep' p1 p2)

pOneOrMoreWithSep' p1 p2
    = pZeroOrMore (pThen (\v1 v2 -> v2) p2 p1)

-- > pSat (== "hello") ["hello", "world"] -- [("hello",["world"])]
-- > pSat (== "hello") ["world", "hello"] -- []
-- > pSat (== "hello") [] -- []
pSat :: (String -> Bool) -> Parser String
pSat p (tok:toks) | p tok = [(tok, toks)]
                  | otherwise = []
pSat p [] = []

-- Exercise 1.16. (p36)

pLit s = pSat (== s)

-- pVar = pSat isVar
--     where
--       isVar (c:cs) = isAlpha c && isVar' cs
--       isVar' (c:cs) = isIdChar c && isVar' cs
--       isVar' [] = True

-- Exercise 1.17. (p37)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- > pVar ["let", "world"] -- []
pVar = pSat isVar
    where
      isVar cs | elem cs keywords == True = False
      isVar (c:cs) = isAlpha c && isVar' cs
      isVar' (c:cs) = isIdChar c && isVar' cs
      isVar' [] = True

syntax = undefined
