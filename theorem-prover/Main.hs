import Base
import Logic

main :: IO ()
main = do
  print $ apply initCtx
    [ id
    , identZero
    , identZero
    , incIdent
    , ax3
    ]

u :: a
u = undefined