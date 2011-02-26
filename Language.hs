module Language where
import Utils

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

