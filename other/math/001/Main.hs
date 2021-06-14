import qualified Data.Set as Set

import System

main :: IO ()
main = do
  let aa = Impl a' a'

  let step_1 = axProp1 a' a'
  let step_2 = axProp1 a' aa
  let step_3 = axProp2 a' aa a'
  let step_4 = ruleMp step_3 step_2
  let step_5 = ruleMp step_4 step_1

  let step_6 = axProp1 b' b'
  let x = proof2pred step_5
  let y = proof2pred step_6

  let step_7 = axProp1 x y
  let step_8 = ruleMp step_7 step_5
  let step_9 = ruleUniIntr a'p a'p x step_8
  let step_10 = ruleMp step_9 step_6

  print $ step_10

  return ()

ite :: Bool -> a -> a -> a
ite a b c = if a then b else c