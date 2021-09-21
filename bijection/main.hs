import Program

type Input  = N
type Output = N

input :: Input
input = 123

main :: IO ()
main = do
  let list = map (run prog) ([0..] :: [N]) :: [N]
  print $ take 101 list

prog :: Prog
prog a = let
  b g = let
    i = nat_fold g :: (N -> N) -> N -> N
    j h = let
      in _ :: N
    k = i j :: N -> N
    l = _ :: N
    in k l
  c = bool_fold b :: (N -> N) -> N -> N
  d = _ :: N -> N
  e = c d :: N -> N
  f = _ :: N
  in e f :: N

-- 01. a
-- 02. g
-- 03. i