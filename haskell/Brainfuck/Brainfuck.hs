import System.IO (stdin, Handle, hReady, hGetChar)
import System.Process (system)
import Control.Monad (when, unless)
import Data.Char (chr)
import Data.Word (Word8)

-- s               +[--------->++<]>+.
-- s               +++++++++++[>++++++++++<-]>+++++.
-- John            +++++ ++ [ > +++++ +++++ < -] >++++. [-] +++++ +++++ + [ > +++++ +++++ < - ] > + . --- --- - . > [-] +++ [< ++ > -] < .
-- Hello, World!   >++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+.
-- fibonacci       +++++++++++>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]
-- factorial       >++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]

----------------------------------------------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------------------------------------------

data Mem = Mem { ptr :: Int, mem :: [Word8] }
data Change = Dec | Inc deriving Eq

----------------------------------------------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------------------------------------------

_MEM_LEN = 30000

----------------------------------------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------------------------------------

cls :: IO ()
cls = do
    system "clear"
    return ()

input :: Read a => IO a
input = do
    x <- readLn
    flush stdin
    return x

flush :: Handle -> IO ()
flush h = do
    ready <- hReady h
    when ready $ do
        hGetChar h
        flush h

loopAround :: Int -> Int -> Int
loopAround limit x = x `mod` limit

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

----------------------------------------------------------------------------------------------------------------
-- Memory
----------------------------------------------------------------------------------------------------------------

initMem :: Mem
initMem = Mem { ptr = 0, mem = replicate _MEM_LEN 0 }

getPtr :: Mem -> Word8
getPtr m = mem m !! ptr m

editValue :: Change -> Mem -> Mem
editValue c m =
    let x = if c == Dec then getPtr m - 1 else getPtr m + 1
        xs = replace (ptr m) x (mem m)
    in  Mem { ptr = ptr m, mem = xs }

movPtr :: Change -> Mem -> Mem
movPtr Inc m = Mem { ptr = loopAround _MEM_LEN (ptr m + 1), mem = mem m }
movPtr Dec m = Mem { ptr = loopAround _MEM_LEN (ptr m - 1), mem = mem m }

printPtr :: Mem -> IO ()
printPtr m = putStr [chr $ fromEnum $ getPtr m]

readToPtr :: Mem -> IO Mem
readToPtr m = do
    x <- input
    let xs = replace (ptr m) x (mem m)
    return Mem { ptr = ptr m, mem = xs }

findLoopEnd :: String -> Int
findLoopEnd (']':_) = 0
findLoopEnd s = go 1 0 s
    where
        go i c [] = error "Invalid Input: missing `]`"
        go i c (x:xs)
            | x == ']' = if c == 0 then i else go (i+1) (c-1) xs
            | x == '[' = go (i+1) (c+1) xs
            | otherwise = go (i+1) c xs

splitLoop :: String -> (String, String)
splitLoop s = let i = findLoopEnd s in splitAt i s

handleLoop :: Mem -> String -> IO Mem
handleLoop m s
    | getPtr m /= 0 = runCode m s >>= (`handleLoop` s)
    | otherwise = return m

runLoop :: Mem -> String -> IO (Mem, String)
runLoop m s = do
    let (loop, s') = splitLoop s
    if not (null loop) then
        handleLoop m loop >>= \ m' -> return (m', s')
    else
        return (m, tail s)

runCode :: Mem -> String -> IO Mem
runCode m [] = return m
runCode m (x:xs) =
    case x of
        '+' -> runCode (editValue Inc m) xs
        '-' -> runCode (editValue Dec m) xs
        '>' -> runCode (movPtr Inc m) xs
        '<' -> runCode (movPtr Dec m) xs
        '.' -> printPtr m >> runCode m xs
        ',' -> readToPtr m >>= (`runCode` xs)
        '[' -> runLoop m xs >>= uncurry runCode
        _ -> runCode m xs

----------------------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------------------

runInterpreter :: IO ()
runInterpreter = do
    str <- getLine

    unless (null str) $ do
        runCode initMem str
        putStrLn ""
        runInterpreter

main :: IO ()
main = cls >> runInterpreter
