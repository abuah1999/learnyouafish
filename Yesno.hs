import Tree

class YesNo a where
    yesno :: a -> Bool

instance YesNo Bool where
    yesno = id

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf cond yes no = if yesno cond then yes else no
