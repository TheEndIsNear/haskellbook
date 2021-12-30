module TypeQwonDo where

data A
data B
data C
data X
data Y
data Z

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h =  g . f

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ = fst . yToWZ . xToY
