{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Annotate.Headers
  ( Header (..),

    -- * Headers
    Headers,
    getHeaders,
    getLastHeader,
    section,
    appendix,

    -- * Tests
    test_getHeaders,
    hprop_monoid,
    hprop_foldable,
    hprop_traversable,
    hprop_getLastHeader,
  )
where

import Control.Monad.State (State, modify, runState)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Dual (Dual, getDual))
import Data.Text (Text)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import qualified Hedgehog as Hh
import qualified Hedgehog.Gen as Hh
import qualified Hedgehog.Range as Hh
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Show (showListWith)

data Header a
  = Section a (NonEmpty Int)
  | Appendix a (NonEmpty Int)
  deriving (Eq, Show)

instance Functor Header where
  fmap f = \case
    Section x path -> Section (f x) path
    Appendix x path -> Appendix (f x) path

instance Foldable Header where
  foldMap f = \case
    Section x _path -> f x
    Appendix x _path -> f x

instance Traversable Header where
  traverse f = \case
    Section x path -> Section <$> f x <*> pure path
    Appendix x path -> Appendix <$> f x <*> pure path

-- * Headers

-- | Carries trees of headers. Each tree can be a normal section or an appendix,
-- and all headers beneath and following an appendix is also an appendix.
data Headers a = Headers
  { -- NB. the trees are stored in reverse order, as is each level in the tree.
    -- This makes it easy to append by consing.
    _headersSections :: [Tree (Maybe a)],
    _headersAppendices :: [Tree (Maybe a)]
  }

instance Semigroup (Headers a) where
  Headers rev_s rev_a <> Headers rev_s' rev_a' = case rev_a of
    [] -> Headers (mergeTrees rev_s rev_s') rev_a'
    _ -> Headers rev_s (mergeTrees rev_a (rev_a' ++ rev_s'))

mergeTrees :: [Tree (Maybe a)] -> [Tree (Maybe a)] -> [Tree (Maybe a)]
mergeTrees [] rs = rs
mergeTrees (l : ls) (unsnoc (: []) -> m_last) = case m_last of
  Nothing -> l : ls
  Just (rs, r) -> rs ++ go l r ++ ls
  where
    go (Tree.Node mx xs) = \case
      Tree.Node Nothing ys -> [Tree.Node mx (mergeTrees xs ys)]
      Tree.Node (Just y) ys -> [Tree.Node (Just y) ys, Tree.Node mx xs]

instance Monoid (Headers a) where
  mempty = Headers [] []
  mappend = (<>)

hprop_monoid :: Hh.Property
hprop_monoid = Hh.property $ do
  as <- Hh.forAll (fold <$> Hh.list (Hh.linear 0 10) genHeader)
  bs <- Hh.forAll (fold <$> Hh.list (Hh.linear 0 10) genHeader)
  cs <- Hh.forAll (fold <$> Hh.list (Hh.linear 0 10) genHeader)
  -- Associativity
  getHeaders ((as <> bs) <> cs) Hh.=== getHeaders (as <> (bs <> cs))
  -- Identity
  getHeaders (mempty <> as) Hh.=== getHeaders as
  getHeaders (as <> mempty) Hh.=== getHeaders as

instance (Show a) => Show (Headers a) where
  showsPrec d headers = showParen (d > 10) $ do
    case getHeaders headers of
      [] -> showString "mempty"
      [h] -> showH h
      hs -> showString "fold " . showListWith showH hs
    where
      showH = shows

instance Functor Headers where
  fmap f (Headers rev_s rev_a) =
    Headers (fmap (fmap f) <$> rev_s) (fmap (fmap f) <$> rev_a)

instance Foldable Headers where
  foldMap f (Headers rev_s rev_a) =
    foldNodes rev_s <> foldNodes rev_a
    where
      foldNodes = getDual . foldMap (Dual . foldNode)
      foldNode (Tree.Node mx ts) = foldMap f mx <> foldNodes ts

hprop_foldable :: Hh.Property
hprop_foldable = Hh.property $ do
  as <- Hh.forAll (Hh.list (Hh.linear 0 10) genHeader)
  bs <- Hh.forAll (Hh.list (Hh.linear 0 10) genHeader)
  -- Check mappend
  foldMap (: []) (fold as <> fold bs)
    Hh.=== (foldMap (: []) (fold as) ++ foldMap (: []) (fold bs))
  -- Check mempty
  foldMap (: []) (mempty :: Headers Int) Hh.=== []
  -- Sanity check length
  length (fold as <> fold bs) Hh.=== (length as + length bs)

instance Traversable Headers where
  traverse f (Headers rev_s rev_a) =
    Headers <$> traverseNodes rev_s <*> traverseNodes rev_a
    where
      traverseNodes =
        fmap reverse . traverse traverseNode . reverse
      traverseNode (Tree.Node mx ts) =
        Tree.Node <$> traverse f mx <*> traverseNodes ts

hprop_traversable :: Hh.Property
hprop_traversable = Hh.property $ do
  as <- Hh.forAll (Hh.list (Hh.linear 0 10) genHeader)
  bs <- Hh.forAll (Hh.list (Hh.linear 0 10) genHeader)
  let f x = x <$ modify (x :) :: State [Int] Int
  let run = flip runState []
  -- Check mappend
  let (u, xs) = run $ traverse f (fold as <> fold bs)
  let (v, ys) = run $ liftA2 (<>) (traverse f (fold as)) (traverse f (fold bs))
  getHeaders u Hh.=== getHeaders v
  xs Hh.=== ys
  -- Check mempty
  let (w, zs) = runState (traverse f (mempty :: Headers Int)) []
  getHeaders w Hh.=== []
  zs Hh.=== []
  -- Sanity check length
  length xs Hh.=== (length as + length bs)

section :: a -> Int -> Headers a
section name lvl = Headers [tree lvl] []
  where
    tree n
      | 1 < n = Tree.Node Nothing [tree (n - 1)]
      | otherwise = Tree.Node (Just name) []

appendix :: a -> Headers a
appendix name = Headers [] [tree]
  where
    tree = Tree.Node (Just name) []

-- * getHeaders

getHeaders :: Headers a -> [Header a]
getHeaders (Headers rev_s rev_a) =
  go Section 1 (reverse rev_s) ++ go Appendix 1 (reverse rev_a)
  where
    go con i = \case
      [] -> []
      t : ts -> goTree con (i :| []) t ++ go con (succ i) ts
    goTree con rev_path (Tree.Node mx ts) =
      [con x (NE.reverse rev_path) | Just x <- [mx]]
        ++ foldMap fn (zip [1 ..] (reverse ts))
      where
        fn (i, t) = goTree con (NE.cons i rev_path) t

-- ** Tests

test_getHeaders :: TestTree
test_getHeaders = testGroup "App.Annotate.Headers" $ do
  [ testCase "empty" $
      f [] @?= ([] :: [Header Text]),
    testCase "one section" $
      f [Right ("Foo", 1)]
        @?= [Section "Foo" (1 :| [])],
    testCase "one appendix" $
      f [Left "Foo"]
        @?= [Appendix "Foo" (1 :| [])],
    testCase "two sections, siblings" $
      f [Right ("Foo", 1), Right ("Bar", 1)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (2 :| [])
            ],
    testCase "two sections, siblings" $
      f [Right ("Foo", 1), Right ("Bar", 1)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (2 :| [])
            ],
    testCase "three sections, siblings" $
      f [Right ("Foo", 1), Right ("Bar", 1), Right ("Quux", 1)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (2 :| []),
              Section "Quux" (3 :| [])
            ],
    testCase "two sections, nested" $
      f [Right ("Foo", 1), Right ("Bar", 2)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (1 :| [1])
            ],
    testCase "two sections, skip level" $
      f [Right ("Foo", 1), Right ("Bar", 3)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (1 :| [1, 1])
            ],
    testCase "three sections, nested" $
      f [Right ("Foo", 1), Right ("Bar", 2), Right ("Quux", 3)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (1 :| [1]),
              Section "Quux" (1 :| [1, 1])
            ],
    testCase "three sections, nested siblings" $
      f [Right ("Foo", 1), Right ("Bar", 2), Right ("Quux", 2)]
        @?= [ Section "Foo" (1 :| []),
              Section "Bar" (1 :| [1]),
              Section "Quux" (1 :| [2])
            ],
    testCase "appendix first" $
      f [Left "Foo", Right ("Bar", 1)]
        @?= [ Appendix "Foo" (1 :| []),
              Appendix "Bar" (2 :| [])
            ],
    testCase "appendix nested" $
      f [Left "Foo", Right ("Bar", 2)]
        @?= [ Appendix "Foo" (1 :| []),
              Appendix "Bar" (1 :| [1])
            ],
    testCase "appendix complex" $
      f [Left "Foo", Left "Bar", Right ("Quux", 2)]
        @?= [ Appendix "Foo" (1 :| []),
              Appendix "Bar" (2 :| []),
              Appendix "Quux" (2 :| [1])
            ]
    ]
  where
    f :: [Either Text (Text, Int)] -> [Header Text]
    f = getHeaders . foldMap (either appendix (uncurry section))

-- * getLastHeader

getLastHeader :: (Show a) => Headers a -> Maybe (Header a)
getLastHeader (Headers rev_s rev_a)
  | t : _ <- rev_a = go Appendix (length rev_a :| []) t
  | t : _ <- rev_s = go Section (length rev_s + length rev_a :| []) t
  | otherwise = Nothing
  where
    go con rev_path = \case
      Tree.Node Nothing [] -> Nothing
      Tree.Node (Just x) [] -> Just (con x (NE.reverse rev_path))
      Tree.Node _ ts@(t : _) -> go con (NE.cons (length ts) rev_path) t

-- ** Tests

-- Check that getLastHeader always agrees with taking the last value returned by
-- getHeaders.
hprop_getLastHeader :: Hh.Property
hprop_getLastHeader = Hh.property $ do
  hs <- Hh.forAll (Hh.list (Hh.linear 0 10) genHeader)
  Hh.annotateShow (fold hs)
  getLastHeader (fold hs) Hh.=== lastMay (getHeaders (fold hs))

-- * Helpers

unsnoc :: (Monoid m) => (a -> m) -> [a] -> Maybe (m, a)
unsnoc f = go mempty
  where
    go _ [] = Nothing
    go m [x] = Just (m, x)
    go m (x : xs) = go (m <> f x) xs

lastMay :: [a] -> Maybe a
lastMay = fmap snd . unsnoc (const ())

-- * Generators

genHeader :: Hh.Gen (Headers Int)
genHeader = do
  let genSection = do
        label <- Hh.prune $ Hh.int (Hh.linear 0 100)
        level <- Hh.int (Hh.linear 1 10)
        pure (section label level)
  let genAppendix = do
        label <- Hh.prune $ Hh.int (Hh.linear 0 100)
        pure (appendix label)
  Hh.choice [genSection, genAppendix]
