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
    hprop_identity,
    hprop_associativity,
    hprop_getLastHeader,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
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
    Headers
      (fmap (fmap (fmap f)) $ rev_s)
      (fmap (fmap (fmap f)) $ rev_a)

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

-- ** Tests

genSingleHeader :: Hh.Gen (Headers Int)
genSingleHeader = do
  let genSection = do
        label <- Hh.prune $ Hh.int (Hh.linear 0 100)
        level <- Hh.int (Hh.linear 1 10)
        pure (section label level)
  let genAppendix = do
        label <- Hh.prune $ Hh.int (Hh.linear 0 100)
        pure (appendix label)
  Hh.choice [genSection, genAppendix]

hprop_identity :: Hh.Property
hprop_identity = Hh.property $ do
  as <- Hh.forAll (foldMap id <$> Hh.list (Hh.linear 0 10) genSingleHeader)
  getHeaders (mempty <> as) Hh.=== getHeaders as
  getHeaders (as <> mempty) Hh.=== getHeaders as

hprop_associativity :: Hh.Property
hprop_associativity = Hh.property $ do
  as <- Hh.forAll (foldMap id <$> Hh.list (Hh.linear 0 10) genSingleHeader)
  bs <- Hh.forAll (foldMap id <$> Hh.list (Hh.linear 0 10) genSingleHeader)
  cs <- Hh.forAll (foldMap id <$> Hh.list (Hh.linear 0 10) genSingleHeader)
  getHeaders ((as <> bs) <> cs) Hh.=== getHeaders (as <> (bs <> cs))

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
  hs <- Hh.forAll (Hh.list (Hh.linear 0 10) genSingleHeader)
  Hh.annotateShow (foldMap id hs)
  getLastHeader (foldMap id hs) Hh.=== lastMay (getHeaders (foldMap id hs))

-- * Helpers

unsnoc :: (Monoid m) => (a -> m) -> [a] -> Maybe (m, a)
unsnoc f = go mempty
  where
    go _ [] = Nothing
    go m [x] = Just (m, x)
    go m (x : xs) = go (m <> f x) xs

lastMay :: [a] -> Maybe a
lastMay = fmap snd . unsnoc (const ())
