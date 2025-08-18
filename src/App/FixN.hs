-- | A fixpoint wrapper which throws in a list of its wrapped functor. It's less
-- noisy than composing @f@ and the @[]@ functor or @NonEmpty@.
--
-- The idea is that one can define a functor type with some number of "holes",
-- as values of polymorphic type in covariant position. These holes normally
-- would be 'Int' or 'String' or whatever, but with 'FixN' it supplies a
-- recursive instantiation.
--
-- If you use 'Maybe' as the functor, then you could define a peano encoding of
-- naturals of no more than three by saying a value is of type @(Maybe (Maybe
-- (Maybe ())))@, so it can have values
--
--   * @Nothing@ (0)
--   * @Just Nothing@ (1)
--   * @Just (Just Nothing)@ (2)
--   * @Just (Just (Just ()))@ (3)
--
-- If we'd like to have naturals of any size in this way we would try to make a
-- type like @Maybe Maybe@ perhaps, but 'Maybe's kind is @* -> *@, so that
-- won't work. Instead, a @newtype@ wrapper gives us what we need.
--
-- > newtype Fix f = Fix (f (Fix f))
-- > type Peano = Fix Maybe
--
-- Now at the type level 'Maybe' wraps 'Maybe' forever, bottoming out at the
-- value level when a 'Nothing' is found.
--
-- This variation wraps each layer of the functor in a list as well to add
-- 'Semigroup' and 'Monoid' instances. It also provides a variation with
-- 'NonEmpty' instead of '[]' to maintain that invariant when possible.
--
--
-- * Why `Fix*` in this program?
--
-- These `Fix*` types can look like a lot of machinery, but their role is fairly
-- straightforward: they provide a recursive structure where each layer may
-- contain multiple child nodes, and it comes with a way of folding over that
-- structure using catamorphisms.
--
-- This matters because `gweb` needs to interpret the same tree of 'Literate'
-- blocks in multiple ways. For example, as:
--
--   - a graph of blocks and cross-references (for visualisation, dependency
--     checking, HTML annotations, etc.).
--   - a flattened tangle of source code lines, ready to be written out.
--
-- These interpretations are expressed as F-algebras:
--
-- App.Graph.alg :: AlgNE Literate (Gen (First (Int, Text)))
-- App.Tangle.alg :: (Abutting a) => AlgNE Literate a
--
-- Normally, you might traverse the tree for each interpretation, or combine the
-- two into a single complex function. With 'FixNE', you can instead combine
-- them with 'bothNE' and run them together:
--
-- let alg = bothNE Graph.alg Tangle.alg
-- let results = [(root, cataNE alg fixn) | (root, fixn) <- roots]
--
-- That means one pass over the tree builds both the graph and the tangled
-- output. This avoids repeated traversal, simplifies memory usage, and keeps
-- each algebra cleanly separated.
module App.FixN where

import Data.Functor.Classes (Show1 (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)

-- * FixN

newtype FixN f = FixN {unFixN :: [f (FixN f)]}

instance Semigroup (FixN f) where
  FixN xs <> FixN ys = FixN (xs ++ ys)

instance Monoid (FixN f) where
  mempty = FixN []
  mappend = (<>)

instance (Show1 f) => Show (FixN f) where
  showsPrec _d = wrap . liftShowsPrec sp sl (app_prec + 1) . unFixN
    where
      app_prec = 10
      -- Use this 'wrap' function if you want 'Read'-able strings.
      -- wrap p = showParen (d > app_prec) (showString "FixN " . p)
      wrap = id
      sp = liftShowsPrec showsPrec showList
      sl = liftShowList showsPrec showList

-- | Run a function under the 'FixN' constructor and wrap it up again.
underFixN ::
  ([f (FixN f)] -> [f (FixN f)]) ->
  (FixN f -> FixN f)
underFixN f = FixN . f . unFixN

-- | Opposite of 'cataN'. Given a seed value, generate one level of structure
-- then recursively generate again using the 'a' values in the previously
-- generated level, ad infinitum.
anaN :: (Functor f) => (a -> [f a]) -> a -> FixN f
anaN f = a where a = FixN . map (fmap a) . f

-- * FixNE

newtype FixNE f = FixNE {unFixNE :: NonEmpty (f (FixNE f))}

instance Semigroup (FixNE f) where
  FixNE xs <> FixNE ys = FixNE (xs <> ys)

instance (Show1 f) => Show (FixNE f) where
  showsPrec _d = wrap . liftShowsPrec sp sl (app_prec + 1) . unFixNE
    where
      app_prec = 10
      -- Use this 'wrap' function if you want 'Read'-able strings.
      -- wrap p = showParen (d > app_prec) (showString "FixN " . p)
      wrap = id
      sp = liftShowsPrec showsPrec showList
      sl = liftShowList showsPrec showList

-- | Run a function under the 'FixNE' constructor and wrap it up again.
underFixNE ::
  (NonEmpty (f (FixNE f)) -> NonEmpty (f (FixNE f))) ->
  (FixNE f -> FixNE f)
underFixNE f = FixNE . f . unFixNE

-- | Opposite of 'cataNE'. Given a seed value, generate one level of structure
-- then recursively generate again using the 'a' values in the previously
-- generated level, ad infinitum.
anaNE :: (Functor f) => (a -> NonEmpty (f a)) -> a -> FixNE f
anaNE f = a where a = FixNE . fmap (fmap a) . f

-- * Alg

-- | The normal F-Algebra, used for defining catamorphisms (folds) over fixed
-- points of structures.
type Alg f a = f a -> a

-- | A catamorphism on the fixpoint, it's a generalised fold which works from
-- the bottom up. There needs to be a Monoid instance on 'a' because we squash
-- the values together with foldMap. The algebra only gets access to each item
-- in the list of values, not the whole list.
cata :: (Functor f, Monoid a) => Alg f a -> FixN f -> a
cata f = foldMap (f . fmap (cata f)) . unFixN

-- * AlgN

-- | A variation on F-Algebras that exposes the whole list of values at each
-- level of the fixed point structure.
type AlgN f a = [f a] -> a

liftAlgAlgN :: (Monoid a) => Alg f a -> AlgN f a
liftAlgAlgN f = mconcat . map f

-- | Construct an algebra from two others. With this we can define several
-- catamorphisms independently but evaluate them all in one pass over the
-- structure, avoiding repeatedly processing the structure. This can also help
-- with memory usage by not holding on to the fixed point structure for longer
-- than necessary.
bothN :: (Functor f) => AlgN f a -> AlgN f b -> AlgN f (a, b)
bothN f g pairs = (f (fmap fst <$> pairs), g (fmap snd <$> pairs))

-- | A catamorphism on the fixpoint, it's a generalised fold which works from
-- the bottom up. The algebra gets access to the list of values at each level.
cataN :: (Functor f) => AlgN f a -> FixN f -> a
cataN f = f . map (fmap (cataN f)) . unFixN

-- * AlgNE

type AlgNE f a = NonEmpty (f a) -> a

liftAlgAlgNE :: (Semigroup a) => Alg f a -> AlgNE f a
liftAlgAlgNE f = sconcat . fmap f

-- | Construct an algebra from two others. With this we can define several
-- catamorphisms independently but evaluate them all in one pass over the
-- structure, avoiding repeatedly processing the structure. This can also help
-- with memory usage by not holding on to the fixed point structure for longer
-- than necessary.
bothNE :: (Functor f) => AlgNE f a -> AlgNE f b -> AlgNE f (a, b)
bothNE f g pairs = (f (fmap fst <$> pairs), g (fmap snd <$> pairs))

-- | A catamorphism on the fixpoint, it's a generalised fold which works from
-- the bottom up. The algebra gets access to the list of values at each level.
cataNE :: (Functor f) => AlgNE f a -> FixNE f -> a
cataNE f = f . fmap (fmap (cataNE f)) . unFixNE
