{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

data True
data False

class Not b1 b | b1 -> b
instance Not False True
instance Not True False

class Result r where
    result :: r

-- instance (Not False r) => Result r

data Zero
data S n

class LT a b t | a b -> t
instance LT Zero Zero False
instance LT (S x) Zero False
instance LT Zero (S x) True
instance (LT a b t) => LT (S a) (S b) t

instance (LT (S (S (S Zero))) (S (S (S (S Zero)))) t) => Result t