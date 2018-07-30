module Data.Tree.AVL.Map where

import           Data.Tree.AVL.Indexed (Tree(..))
import qualified Data.Tree.AVL.Indexed as Tree

import           GHC.Base (build)
import           Data.Functor.Classes
import           Data.Foldable (foldl')
import           Control.DeepSeq

data Map k v =
    forall h. Map !(Tree h k v)

deriving instance Functor     (Map k)
deriving instance Foldable    (Map k)
deriving instance Traversable (Map k)

insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map tr) =
    case Tree.insertWith f k v tr of
        Tree.Stay tr' -> Map tr'
        Tree.Incr tr' -> Map tr'

insert :: Ord k => k -> v -> Map k v -> Map k v
insert = insertWith const

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map tr) = Tree.lookup k tr

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map tr) =
    case Tree.delete k tr of
        Tree.Hold tr' -> Map tr'
        Tree.Decr tr' -> Map tr'

toList :: Map k v -> [(k, v)]
toList (Map tr :: Map k v) =
    build
        (\(c :: (k, v) -> b -> b) ->
              let go
                      :: forall h.
                         Tree h k v -> b -> b
                  go Leaf n = n
                  go (Node k v _ l r) n = go l (c (k, v) (go r n))
              in go tr)

empty :: Map k v
empty = Map Leaf

fromListWith :: Ord k => (v -> v -> v) -> [(k,v)] -> Map k v
fromListWith f = foldl' (\a (k,v) -> insertWith f k v a) empty

fromList :: Ord k => [(k,v)] -> Map k v
fromList = fromListWith const

instance Eq2 Map where
    liftEq2 x y xs ys = liftEq (liftEq2 x y) (toList xs) (toList ys)

instance Ord2 Map where
    liftCompare2 x y xs ys =
        liftCompare (liftCompare2 x y) (toList xs) (toList ys)

instance Show2 Map where
    liftShowsPrec2 xsp xsl ysp ysl _ = liftShowList2 xsp xsl ysp ysl . toList

instance Eq a => Eq1 (Map a) where
    liftEq = liftEq2 (==)

instance Ord a => Ord1 (Map a) where
    liftCompare = liftCompare2 compare

instance Show a => Show1 (Map a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq a, Eq b) => Eq (Map a b) where
    (==) = eq1

instance (Ord a, Ord b) => Ord (Map a b) where
    compare = compare1

instance (Show a, Show b) => Show (Map a b) where
    showsPrec = showsPrec1

instance (NFData k, NFData v) => NFData (Map k v) where
    rnf (Map tr) = rnf tr

alterF
    :: (Ord k, Functor f)
    => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
alterF k f (Map tr) = Tree.alterF k f tr $ \case
    Tree.Dn tr' -> Map tr'
    Tree.Sm tr' -> Map tr'
    Tree.Up tr' -> Map tr'

