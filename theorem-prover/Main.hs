import Base
import Logic

main :: IO ()
main = do
  print $ apply initCtx
    [ id
    , fstIdent
    , fstIdent
    , nextIdent
    , ax3
    ]

u :: a
u = undefined