{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (fst, snd)

main :: IO ()
main = putStrLn output

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c;

u :: a
u = undefined

----------------------------------------------------------------------------------------------------

b0 a b = b
b1 a b = a

pair a b c = c b a

fst pair = pair b1
snd pair = pair b0

unpair func pair = func (fst pair) (snd pair)

----------------------------------------------------------------------------------------------------

output :: String
output = "ok"