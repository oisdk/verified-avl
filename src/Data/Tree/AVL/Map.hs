module Data.Tree.AVL.Map where

import           Data.Tree.AVL.Indexed (Tree)
import qualified Data.Tree.AVL.Indexed as Tree

data Map k v = forall h. Map !(Tree k v h)

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
