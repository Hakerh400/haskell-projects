import Base
import Logic

main :: IO ()
main = do
  ctx <- pure $ initCtx
  ctx <- pure $ defType 2 ctx
  ctx <- pure $ defType 0 ctx
  ctx <- pure $ defType 0 ctx
  print ctx

u :: a
u = undefined