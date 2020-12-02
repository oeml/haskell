import Data.List (find)

newtype VersionedMap k v = VersionedMap { toList :: [(k,v)] }
    deriving (Show)

instance Functor (VersionedMap k) where
    fmap f = VersionedMap . fmap (fmap f) . toList

hasKey :: Eq k => k -> VersionedMap k v -> Bool
hasKey k = elem k . fmap fst . toList

commit :: k -> v -> VersionedMap k v -> VersionedMap k v
commit k v = VersionedMap . ((k,v):) . toList

isSameKey :: Eq k => k -> (k,v) -> Bool
isSameKey k = (k==) . fst

get :: Eq k => k -> VersionedMap k v -> Maybe v
get k = fmap snd . find (isSameKey k) . toList

amend :: Eq k => k -> VersionedMap k v -> VersionedMap k v
amend k = VersionedMap . reunite . break (isSameKey k) . toList where
    reunite (xs,[]) = xs
    reunite (xs,(y:ys)) = xs ++ ys
