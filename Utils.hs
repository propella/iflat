-- Appendix A Utilities module (270)

module Utils where

-- A.1.2 Representation (p271)

type Heap a = (Int, [Int], [(Int, a)])
type Addr   = Int

hInitial :: (Int, [Int], [a])
hInitial                             = (0,       [1..], [])
hAlloc  (size, (next:free), cts) n   = ((size+1, free,  (next,n) : cts),next)
hUpdate (size, free,        cts) a n = (size,   free,   (a,n) : remove cts a)
hFree   (size, free,        cts) a   = (size-1, a:free, remove cts a)

hLookup (size,free,cts) a
    = aLookup cts a (error ("can’t find node " ++ showaddr a ++ " in heap"))

hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize (size, free, cts) = size

hNull      = 0
hIsnull a  = a == 0
showaddr a = "#" ++ show a   -- Print # to identify addresses

remove :: [(Int,a)] -> Int -> [(Int,a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++
                     show a)
remove ((a',n):cts) a | a == a' = cts
                      | a /= a' = (a',n) : remove cts a

-- A.2 The association list type (p272)

type ASSOC a b = [(a,b)]

aLookup [] k' def = def
aLookup ((k,v):bs) k' def | k == k' = v
                          | k /= k' = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key,val) <- alist]
aRange :: ASSOC a b -> [b]
aRange alist = [val | (key,val) <- alist]

aEmpty = []

-- A.5 Other useful function definitions (p275)

mapAccuml2, mapAccuml :: (a -> b -> (a, c)) -- Function of accumulator and element
                                -- input list, returning new
                                -- accumulator and element of result list
          -> a                  -- Initial accumulator
          -> [b]                -- Input list
          -> (a, [c])           -- Final accumulator and result list

mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x':xs')
    where (acc1, x') = f acc x
          (acc2, xs') = mapAccuml f acc1 xs

-- by foldl
-- mapAccuml2 (\x y -> (x + 10, y + x)) 10 [3, 2, 1] -- (40,[13,22,31])
mapAccuml2 f acc xs = (acc', reverse xs')
    where (acc', xs') = mapAccuml2' f acc xs

mapAccuml2' f acc xs = foldl g (acc, []) xs
    where g (acc', ys) y = (acc'', y':ys)
            where (acc'', y') = f acc' y

space n = take n (repeat ' ')
