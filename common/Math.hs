module Math where

data Vec3 = Vec3 !Float !Float !Float deriving (Eq, Show)

neg (Vec3 x y z) = Vec3 (-x) (-y) (-z)
(Vec3 x y z) $+ (Vec3 p q r) = Vec3 (x+p) (y+q) (z+r)
a $- b = a $+ (neg b)
(Vec3 x y z) $* s = Vec3 (x*s) (y*s) (z*s)
dot (Vec3 x y z) (Vec3 p q r) = x*p+y*q+z*r
len v = sqrt $ dot v v
norm v = v $* (1 / len v)