module Template where
import Language
import Utils

-- 2.2 State transition systems (p49)

type MultState = (Int, Int, Int, Int) -- (n, m, d, t)

evalMult :: MultState -> [MultState]
evalMult state = if multFinal state
                 then [state]
                 else state : evalMult (stepMult state)

stepMult (n, m, d, t) | d > 0  = (n,     m, d - 1, t + 1)
stepMult (n, m, d, t) | d == 0 = (n, m - 1,     n, t    )

-- Exercise 2.3. (p51)

multFinal :: MultState -> Bool
multFinal (_, 0, 0, _) = True
multFinal _ = False

-- 2.3.2 Structure of the implementation (p53)

runProg :: [Char] -> [Char]
-- parse :: [Char] -> CoreProgram
compile :: CoreProgram -> TiState
eval :: TiState -> [TiState]
showResults :: [TiState] -> [Char]

runProg = showResults . eval . compile . parse

eval = undefined
showResults = undefined

-- 2.3.4 The compiler (p54)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node


data Node = NAp Addr Addr			-- Application
          | NSupercomb Name [Name] CoreExpr	-- Supercombinator
          | NNum Int				-- A number

type TiGlobals = ASSOC Name Addr

tiStatInitial :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int

type TiStats = Int
tiStatInitial    = 0
tiStatIncSteps s = s + 1
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
    = (stack, dump, heap, sc_defs, stats_fun stats)

-- The compiler itself (p56)

compile program
    = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
      where sc_defs = program ++ preludeDefs ++ extraPreludeDefs
            (initial_heap, globals) = buildInitialHeap sc_defs
            initial_stack = [address_of_main]
            address_of_main = aLookup globals "main" (error "main is not defined")

extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)

buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
    = (heap', (name, addr))
      where
        (heap', addr) = hAlloc heap (NSupercomb name args body)

-- 2.3.5 The evaluator (p57)
