{-  The Computer Language Benchmarks Game

    http://benchmarkgame.alioth.debian.org/

    contributed by Bryan O'Sullivan

    parallelized and modified to use search
    instead of indexing by Maxim Sokolov
-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import GHC.Word
import Data.Char

modulus = 139968

kWidth = 60
kLines = 1024
kBlockSize = kWidth * kLines
kNewLine = fromIntegral $ ord '\n'

data LookupTable = LookupTable
    { cumProb :: Ptr Int
    , vals :: Ptr Word8
    }

main = do
    n <- getArgs >>= readIO.head
    writeAlu ">ONE Homo sapiens alu" (L.take (fromIntegral n*2) (L.cycle alu))
    make ">TWO IUB ambiguity codes" (n*3) iub 42 >>=
      void . make ">THREE Homo sapiens frequency" (n*5) homosapiens

writeAlu name s0 = B.putStrLn name >> go s0
    where go s = L.putStrLn h >> unless (L.null t) (go t)
            where (h,t) = L.splitAt 60 s

make name n0 tbl seed0 = do
    B.putStrLn name
    lookupTable <- buildLookupTable tbl
    ready <- newMVar ()
    input <- newMVar (seed0, ready, n0)
    finished <- newEmptyMVar
    replicateM_ 4 . forkIO $ worker input finished lookupTable
    takeMVar finished

buildLookupTableOld tbl =
    let fill ((c,p):cps) j =
            let !k = min modulus (floor (fromIntegral modulus * (p::Float) + 1))
            in B.replicate (k - j) c : fill cps k
        fill _ _ = []
     in B.concat $ fill (scanl1 (\(_,p) (c,q) -> (c,p+q)) tbl) 0

buildLookupTable tbl = do
    cumProbsBuf <- mallocArray (length tbl)
    valuesBuf <- mallocArray (length tbl)
    let go !i !cum ((v, p):xs) = do
            let p1 = p * fromIntegral modulus
            let vw = fromIntegral $ ord v
            pokeElemOff cumProbsBuf i (floor $ cum+p1)
            pokeElemOff valuesBuf i vw
            go (i+1) (cum+p1) xs
        go _ _ [] = return ()
    go 0 0.0 tbl
    return $ LookupTable cumProbsBuf valuesBuf

worker input finished lookupTable = do
    rand <- mallocArray kBlockSize
    buf <- mallocArray (kBlockSize + kLines) :: IO (Ptr Word8)
    forever $ do
        (seed0, prevReady, count0) <- takeMVar input
        let !n = min kBlockSize count0
            !count1 = count0 - n
        seed1 <- fillRandomsBlock n rand seed0
        ready <- newEmptyMVar
        when (count1 > 0) $ do
            putMVar input (seed1, ready, count1)
        k <- fillBuf n lookupTable rand buf
        _ <- takeMVar prevReady
        hPutBuf stdout buf k
        if count1 == 0
            then putMVar finished seed1
            else putMVar ready ()

fillRandomsBlock !n0 !ptr !seed0 = do
    let go !j !seed
            | j < n0 = do
                    let newseed = rem (seed * 3877 + 29573) modulus
                    pokeElemOff ptr j newseed
                    go (j + 1) newseed
            | otherwise = return seed
    go 0 seed0

fillBuf !n0 !lt@(LookupTable !cum !vs) !rand !buf = go 0 0
    where
        go !i !j
            | i < n0 = do
                f <- peekElemOff rand i
                j1 <- if i > 0 && rem i 60 == 0
                        then do
                            pokeElemOff buf j kNewLine
                            return $ j+1
                        else do
                            return j
                update j1 f 0
                go (i+1) (j1+1)
            | otherwise = do
                pokeElemOff buf j kNewLine
                return (j+1)
        update !j1 !f !i = do
            p <- peekElemOff cum i
            if f <= p
                then peekElemOff vs i >>=
                        pokeElemOff buf j1
                else update j1 f (i+1)

alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
      ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
      ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens = [('a',0.3029549426680),('c',0.1979883004921)
              ,('g',0.1975473066391),('t',0.3015094502008)]

{-
notes, command-line, and program output

NOTES:
64-bit Ubuntu quad core
The Glorious Glasgow Haskell Compilation System, version 7.10.2


Mon, 21 Sep 2015 19:02:58 GMT

MAKE:
mv fasta.ghc-6.ghc fasta.ghc-6.hs
/usr/local/src/ghc-7.10.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts -XOverloadedStrings fasta.ghc-6.hs -o fasta.ghc-6.ghc_run
[1 of 1] Compiling Main             ( fasta.ghc-6.hs, fasta.ghc-6.o )
Linking fasta.ghc-6.ghc_run ...
rm fasta.ghc-6.hs
1.77s to complete and log all make actions

COMMAND LINE:
./fasta.ghc-6.ghc_run +RTS -N4 -RTS 25000000

(TRUNCATED) PROGRAM OUTPUT:
>ONE Homo sapiens alu
GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGA
TCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACT
AAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAG
GCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCG
CCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAGGCCGGGCGCGGT
GGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCA
-}
