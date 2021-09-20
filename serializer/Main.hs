import Serializer

import Prelude hiding (read)

type N = Integer

main :: IO ()
main = do
  let n = ser func_ser
  print n
  putStrLn ""
  deser func_deser n

func_ser :: Ser ()
func_ser = do
  write 10 5
  write 11 7
  write_nat 78
  write 23 9
  inc1
  write 123 45
  write_nat' 52

func_deser :: Ser (IO ())
func_deser = do
  vals <- sequence
    [ read 10
    , read 11
    , read_nat
    , read 23
    , nz >> read 123
    , read_nat'
    ]
  return $ mapM_ print vals