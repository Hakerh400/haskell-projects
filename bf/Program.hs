module Program
( run
) where

import Prelude hiding (Left, Right)
import Data.Char

data Inst =
  Left | Right |
  Inc | Dec |
  In | Out |
  Loop [Inst]

type Parsed = [Inst]
type MemoryPart = [Integer]
type Memory = (MemoryPart, MemoryPart)
type Byte = Integer

data Program = Program {
  insts :: Parsed,
  mem :: Memory,
  input :: String,
  output :: String
}

while :: a -> (a -> Bool) -> (a -> a) -> a
while val test func
  | test val = while (func val) test func
  | otherwise = val

memInit :: Memory
memInit = (memPartInit, memPartInit)

memPartInit :: MemoryPart
memPartInit = [0,0..]

run :: String -> String -> String
run src input = Program.output $ runProg $ createProg (parse src) input

createProg :: Parsed -> String -> Program
createProg insts input = Program {
  insts = insts,
  mem = memInit,
  input = input,
  output = []
}

runProg :: Program -> Program
runProg prog = case insts of
  [] -> prog
  (x:xs) -> case x of
    Left -> runProg Program {insts = xs, mem = (ys, y:z:zs), input = input, output = output}
    Right -> runProg Program {insts = xs, mem = (z:y:ys, zs), input = input, output = output}
    Inc -> runProg Program {insts = xs, mem = (y:ys, (inc z):zs), input = input, output = output}
    Dec -> runProg Program {insts = xs, mem = (y:ys, (dec z):zs), input = input, output = output}
    In -> runProg Program {insts = xs, mem = (y:ys, inputByte:zs), input = inputRest, output = output}
    Out -> runProg Program {insts = xs, mem = (y:ys, z:zs), input = input, output = output ++ [byte2char z]}
    Loop insts -> runWith (while prog nz (boundLoop insts)) xs
    where
      (inputChar, inputRest) = case input of
        [] -> ('\x00', [])
        (x:xs) -> (x, xs)
      inputByte = char2byte inputChar
  where
    Program {
      insts = insts,
      mem = (y:ys, z:zs),
      input = input,
      output = output
    } = prog
    nz :: Program -> Bool
    nz Program {mem = (_, (x:_))} = x /= 0
    boundLoop :: [Inst] -> Program -> Program
    boundLoop insts prog = runWith prog insts
    runWith :: Program -> [Inst] -> Program
    runWith prog insts = runProg Program {
      insts = insts,
      mem = Program.mem prog,
      input = Program.input prog,
      output = Program.output prog
    }

char2byte :: Char -> Byte
char2byte a = toInteger (ord a)

byte2char :: Byte -> Char
byte2char a = chr (fromIntegral a)

inc :: Byte -> Byte
inc 255 = 0
inc n = n + 1

dec :: Byte -> Byte
dec 0 = 255
dec n = n - 1

parse :: String -> Parsed
parse src = snd (parse1 src) where
  parse1 :: String -> (String, [Inst])
  parse1 [] = ([], [])
  parse1 (']':xs) = (xs, [])
  parse1 src = (rem, inst:insts) where
    (rem1, inst) = parseInst src
    (rem, insts) = parse1 rem1
  parseInst :: String -> (String, Inst)
  parseInst (x:xs) = case x of
    '<' -> (xs, Left)
    '>' -> (xs, Right)
    '+' -> (xs, Inc)
    '-' -> (xs, Dec)
    ',' -> (xs, In)
    '.' -> (xs, Out)
    '[' -> (loopRem, loopInst)
    where
      (loopRem, loopInsts) = parse1 xs
      loopInst = Loop loopInsts