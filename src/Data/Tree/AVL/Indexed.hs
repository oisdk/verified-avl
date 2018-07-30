module Data.Tree.AVL.Indexed where

import Prelude hiding (lookup)
import Data.Kind
import Control.DeepSeq
import Data.Tree.AVL.Height

data Tree :: Height -> Type -> Type -> Type where
        Leaf :: Tree Z k v
        Node :: !k
             -> v
             -> !(Balance lh rh h)
             -> !(Tree lh k v)
             -> !(Tree rh k v)
             -> Tree (S h) k v

deriving instance Functor     (Tree h k)
deriving instance Foldable    (Tree h k)
deriving instance Traversable (Tree h k)
deriving instance (Show k, Show v) => Show (Tree h k v)

data Inserted :: Type -> Type -> Height -> Type where
        Stay :: !(Tree n k v) -> Inserted k v n
        Incr :: !(Tree (S n) k v) -> Inserted k v n

rotr :: k -> v -> Tree (S (S rh)) k v -> Tree rh k v -> Inserted k v (S (S rh))
rotr x xv (Node y yv L a b) c = Stay (Node y yv O a (Node x xv O b c))
rotr x xv (Node y yv O a b) c = Incr (Node y yv R a (Node x xv L b c))
rotr x xv (Node y yv R a (Node z zv bl b c)) d =
    Stay (Node z zv O (Node y yv (balr bl) a b) (Node x xv (ball bl) c d))

rotl :: k -> v -> Tree rh k v -> Tree (S (S rh)) k v -> Inserted k v (S (S rh))
rotl x xv c (Node y yv R b a) = Stay (Node y yv O (Node x xv O c b) a)
rotl x xv c (Node y yv O b a) = Incr (Node y yv L (Node x xv R c b) a)
rotl x xv d (Node y yv L (Node z zv bl c b) a) =
    Stay (Node z zv O (Node x xv (balr bl) d c) (Node y yv (ball bl) b a))

insertWith
    :: Ord k
    => (v -> v -> v) -> k -> v -> Tree h k v -> Inserted k v h
insertWith _ v vc Leaf = Incr (Node v vc O Leaf Leaf)
insertWith f v vc (Node k kc bl tl tr) =
    case compare v k of
        LT ->
            case insertWith f v vc tl of
                Stay tl' -> Stay (Node k kc bl tl' tr)
                Incr tl' ->
                    case bl of
                        L -> rotr k kc tl' tr
                        O -> Incr (Node k kc L tl' tr)
                        R -> Stay (Node k kc O tl' tr)
        EQ -> Stay (Node v (f vc kc) bl tl tr)
        GT ->
            case insertWith f v vc tr of
                Stay tr' -> Stay (Node k kc bl tl tr')
                Incr tr' ->
                    case bl of
                        L -> Stay (Node k kc O tl tr')
                        O -> Incr (Node k kc R tl tr')
                        R -> rotl k kc tl tr'

lookup :: Ord k => k -> Tree h k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Node v vc _ tl tr) =
    case compare k v of
        LT -> lookup k tl
        EQ -> Just vc
        GT -> lookup k tr

data Deleted :: Type -> Type -> Height -> Type where
        Hold :: !(Tree n k v) -> Deleted k v n
        Decr :: !(Tree n k v) -> Deleted k v (S n)

deleted :: Inserted k v n -> Deleted k v (S n)
deleted (Stay x) = Decr x
deleted (Incr x) = Hold x

uncons
    :: k
    -> v
    -> Balance lh rh h
    -> Tree lh k v
    -> Tree rh k v
    -> (k, v, Deleted k v (S h))
uncons (k' :: k) (v' :: v) bl' tl' tr' = go k' v' bl' tl' tr' id
  where
    go
        :: forall x lh rh h.
           k
        -> v
        -> Balance lh rh h
        -> Tree lh k v
        -> Tree rh k v
        -> (Deleted k v (S h) -> x)
        -> (k, v, x)
    go k v O Leaf tr c = (k, v, c (Decr tr))
    go k v O (Node kl vl bl tll trl) tr c =
        go kl vl bl tll trl $
        \case
            Decr ntl -> c (Hold (Node k v R ntl tr))
            Hold ntl -> c (Hold (Node k v O ntl tr))
    go k v R Leaf tr c = (k, v, c (Decr tr))
    go k v R (Node kl vl bl tll trl) tr c =
        go kl vl bl tll trl $
        \case
            Decr ntl -> c (deleted (rotl k v ntl tr))
            Hold ntl -> c (Hold (Node k v R ntl tr))
    go k v L (Node kl vl bl tll trl) tr c =
        go kl vl bl tll trl $
        \case
            Decr ntl -> c (Decr (Node k v O ntl tr))
            Hold ntl -> c (Hold (Node k v L ntl tr))

delete :: Ord k => k -> Tree h k v -> Deleted k v h
delete _ Leaf = Hold Leaf
delete k (Node k' v b tl tr) =
    case compare k k' of
        LT ->
            case delete k tl of
                Hold tl' -> Hold (Node k' v b tl' tr)
                Decr tl' ->
                    case b of
                        L -> Decr (Node k' v O tl' tr)
                        O -> Hold (Node k' v R tl' tr)
                        R -> deleted (rotl k' v tl' tr)
        GT ->
            case delete k tr of
                Hold tr' -> Hold (Node k' v b tl tr')
                Decr tr' ->
                    case b of
                        L -> deleted (rotr k' v tl tr')
                        O -> Hold (Node k' v L tl tr')
                        R -> Decr (Node k' v O tl tr')
        EQ ->
            case tr of
                Leaf ->
                    case b of
                        L -> Decr tl
                        O -> Decr tl
                Node kr vr br tlr trr ->
                    case b of
                        L ->
                            case uncons kr vr br tlr trr of
                                (k'',v',Decr tr') ->
                                    deleted (rotr k'' v' tl tr')
                                (k'',v',Hold tr') ->
                                    Hold (Node k'' v' L tl tr')
                        O ->
                            case uncons kr vr br tlr trr of
                                (k'',v',Decr tr') ->
                                    Hold (Node k'' v' L tl tr')
                                (k'',v',Hold tr') ->
                                    Hold (Node k'' v' O tl tr')
                        R ->
                            case uncons kr vr br tlr trr of
                                (k'',v',Decr tr') ->
                                    Decr (Node k'' v' O tl tr')
                                (k'',v',Hold tr') ->
                                    Hold (Node k'' v' R tl tr')

instance (NFData k, NFData v) =>
         NFData (Tree h k v) where
    rnf Leaf = ()
    rnf (Node k v b l r) =
        rnf k `seq` rnf v `seq` rnf b `seq` rnf l `seq` rnf r

data Altered :: Type -> Type -> Height -> Type where
        Up :: !(Tree (S n) k v) -> Altered k v n
        Sm :: !(Tree n k v) -> Altered k v n
        Dn :: !(Tree n k v) -> Altered k v (S n)

alteredUp :: Inserted k v h -> Altered k v (S h)
alteredUp (Stay tr) = Dn tr
alteredUp (Incr tr) = Sm tr

alteredDn :: Inserted k v h -> Altered k v h
alteredDn (Stay tr) = Sm tr
alteredDn (Incr tr) = Up tr

alterF :: forall k f v h. (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> Tree h k v -> f (Altered k v h)
alterF k f tr = go tr id
  where
    go :: forall ht. Tree ht k v -> (Altered k v ht -> Altered k v h) -> f (Altered k v h)
    go Leaf c = fmap (maybe (Sm tr) (\v -> c (Up (Node k v O Leaf Leaf)))) (f Nothing)
    go (Node k' v b l r) c =
        case compare k k' of
            LT ->
                go l $
                c .
                \case
                    Sm l' -> Sm (Node k' v b l' r)
                    Dn l' ->
                        case b of
                            L -> Dn (Node k' v O l' r)
                            O -> Sm (Node k' v R l' r)
                            R -> alteredUp (rotl k' v l' r)
                    Up l' ->
                        case b of
                            L -> alteredDn (rotr k' v l' r)
                            O -> Up (Node k' v L l' r)
                            R -> Sm (Node k' v O l' r)
            GT ->
                go r $
                c .
                \case
                    Sm r' -> Sm (Node k' v b l r')
                    Dn r' ->
                        case b of
                            L -> alteredUp (rotr k' v l r')
                            O -> Sm (Node k' v L l r')
                            R -> Dn (Node k' v O l r')
                    Up r' ->
                        case b of
                            L -> Sm (Node k' v O l r')
                            O -> Up (Node k' v R l r')
                            R -> alteredDn (rotl k' v l r')
            EQ ->
                flip fmap (f (Just v)) $
                c .
                \case
                    Just v' -> Sm (Node k' v' b l r)
                    Nothing ->
                        case r of
                            Leaf ->
                                case b of
                                    L -> Dn l
                                    O -> Dn l
                            Node kr vr br tlr trr ->
                                case b of
                                    L ->
                                        case uncons kr vr br tlr trr of
                                            (k'',v',Decr r') ->
                                                alteredUp (rotr k'' v' l r')
                                            (k'',v',Hold r') ->
                                                Sm (Node k'' v' L l r')
                                    O ->
                                        case uncons kr vr br tlr trr of
                                            (k'',v',Decr r') ->
                                                Sm (Node k'' v' L l r')
                                            (k'',v',Hold r') ->
                                                Sm (Node k'' v' O l r')
                                    R ->
                                        case uncons kr vr br tlr trr of
                                            (k'',v',Decr r') ->
                                                Dn (Node k'' v' O l r')
                                            (k'',v',Hold r') ->
                                                Sm (Node k'' v' R l r')
