import Data.Bits

data Bit = B0 | B1 deriving Eq

len :: Integer
len = 100

main :: IO ()
main = putStrLn output

output :: String
output = join list "\n"

list :: [String]
list = ca len f1 where
  f1 :: Integer -> String
  f1 i = a ++ sep ++ b ++ sep ++ c where
    a = show i
    b = bin2str str
    c = if f str then "OK" else ""
    str = g i
    sep = " --- "

pf :: [Bit] -> Bool -> Bool
pf [] isf = True
pf (a:b) isf = pf b a False isf where
  pf :: [Bit] -> Bit -> Bool -> Bool -> Bool
  pf [] a b isf = if isf then True else b
  pf (bit:rest) prev samePrev isf
    | same || samePrev = pf rest bit same isf
    | otherwise = False
    where
      same = bit == prev

p :: [Bit] -> Bool
p a = pf a False

f :: [Bit] -> Bool
f a = pf a True

g :: Integer -> [Bit]
g n = g1 n [] True where
  g1 :: Integer -> [Bit] -> Bool -> [Bit]
  g1 n str allowEnd
    | end = ifEnd
    | append0 && append1 = ifBoth
    | append0 = next B0
    | append1 = next B1
    where
      end = allowEnd && (p str)
      append0 = f $ str ++ [B0]
      append1 = f $ str ++ [B1]
      ifEnd :: [Bit]
      ifEnd
        | n == 0 = str
        | n /= 0 = g1 (n - 1) str False
      ifBoth :: [Bit]
      ifBoth = g1 (n `div` 2) (str ++ [int2bit $ n .&. 1]) True
      next :: Bit -> [Bit]
      next a = g1 n (str ++ [a]) True

int2bin :: Integer -> [Bit]
int2bin 0 = []
int2bin a = (int2bit $ b .&. 1) : (int2bin $ b `div` 2) where
  b = a - 1

bin2str :: [Bit] -> String
bin2str [] = []
bin2str (a:b) = (bit2char a) : (bin2str b)

int2bit :: Integer -> Bit
int2bit 0 = B0
int2bit 1 = B1

bit2int :: Bit -> Integer
bit2int B0 = 0
bit2int B1 = 1

char2bit :: Char -> Bit
char2bit '0' = B0
char2bit '1' = B1

bit2char :: Bit -> Char
bit2char B0 = '0'
bit2char B1 = '1'

ca :: Integer -> (Integer -> a) -> [a]
ca 0 a = []
ca a b = (ca c b) ++ [b c] where
  c = a - 1

join :: [String] -> String -> String
join [] a = ""
join (a:[]) b = a
join (a:b:c) d = a ++ d ++ join ([b] ++ c) d