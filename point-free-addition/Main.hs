main = print $ (add 5 7 :: Integer)

t True = k
t False = k i

(!) = flip
infixr 9 !

k = const
s a b c = a c (b c)
i = s k k
r = s i r

add = r$s(s.((t.(0==))!)).(((1+).).).((!).(.)(.))!(-)!1