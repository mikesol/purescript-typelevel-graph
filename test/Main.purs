module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Boolean (False, True)
import Prim.RowList (class RowToList)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Graph (class AllEdges, class Concat, class HasOrphanNodes, class HasSymbol, class IsConnected, class Lookup, class RemoveDuplicates, class Traversal, type (:/))

testHasSymbol :: BProxy True
testHasSymbol =
  BProxy ::
    forall b.
    HasSymbol ("a" :/ "b" :/ SNil) "a" b =>
    BProxy b

testHasSymbol' :: BProxy False
testHasSymbol' =
  BProxy ::
    forall b.
    HasSymbol ("a" :/ "b" :/ SNil) "q" b =>
    BProxy b

testHasSymbol'' :: BProxy True
testHasSymbol'' =
  BProxy ::
    forall b.
    HasSymbol ("a" :/ "b" :/ SNil) "b" b =>
    BProxy b

testHasSymbol''_ :: BProxy False
testHasSymbol''_ =
  BProxy ::
    forall b.
    HasSymbol (SNil) "b" b =>
    BProxy b

testLookup :: SLProxy ("a" :/ "b" :/ SNil)
testLookup =
  SLProxy ::
    forall rl sl.
    RowToList ( "q" :: (SLProxy ("a" :/ "b" :/ SNil)) ) rl =>
    Lookup "q" rl sl =>
    SLProxy sl

testLookup1 :: SLProxy ("a" :/ "b" :/ SNil)
testLookup1 =
  SLProxy ::
    forall rl sl.
    RowToList ( "r" :: (SLProxy ("x" :/ "b" :/ SNil)), "q" :: (SLProxy ("a" :/ "b" :/ SNil)) ) rl =>
    Lookup "q" rl sl =>
    SLProxy sl

testLookup2 :: SLProxy ("a" :/ "b" :/ SNil)
testLookup2 =
  SLProxy ::
    forall rl sl.
    RowToList ( "q" :: (SLProxy ("a" :/ "b" :/ SNil)), "r" :: (SLProxy ("x" :/ "b" :/ SNil)) ) rl =>
    Lookup "q" rl sl =>
    SLProxy sl

testLookup3 :: SLProxy SNil
testLookup3 =
  SLProxy ::
    forall rl sl.
    RowToList ( "q" :: (SLProxy ("a" :/ "b" :/ SNil)), "r" :: (SLProxy ("x" :/ "b" :/ SNil)) ) rl =>
    Lookup "f" rl sl =>
    SLProxy sl

testConcat :: SLProxy ("a" :/ "b" :/ "c" :/ SNil)
testConcat =
  SLProxy ::
    forall sl.
    Concat ("a" :/ "b" :/ SNil) ("c" :/ SNil) sl =>
    SLProxy sl

testRemoveDuplicates :: SLProxy ("a" :/ SNil)
testRemoveDuplicates =
  SLProxy ::
    forall sl.
    RemoveDuplicates ("a" :/ SNil) sl =>
    SLProxy sl

testRemoveDuplicates1 :: SLProxy ("a" :/ SNil)
testRemoveDuplicates1 =
  SLProxy ::
    forall sl.
    RemoveDuplicates ("a" :/ "a" :/ SNil) sl =>
    SLProxy sl

testRemoveDuplicates2 :: SLProxy ("b" :/ "a" :/ SNil)
testRemoveDuplicates2 =
  SLProxy ::
    forall sl.
    RemoveDuplicates ("a" :/ "b" :/ "a" :/ SNil) sl =>
    SLProxy sl

testTraversal0 :: SLProxy ("q" :/ SNil)
testTraversal0 =
  SLProxy ::
    forall sl.
    Traversal "q" ( "q" :: SLProxy SNil ) sl =>
    SLProxy sl

testTraversal1 :: SLProxy ("q" :/ "a" :/ SNil)
testTraversal1 =
  SLProxy ::
    forall sl.
    Traversal "q" ( "q" :: SLProxy ("a" :/ SNil) ) sl =>
    SLProxy sl

testEmptyGraph :: BProxy True
testEmptyGraph =
  BProxy ::
    forall b.
    IsConnected () b =>
    BProxy b

testSingletonGraph :: BProxy True
testSingletonGraph =
  BProxy ::
    forall b.
    IsConnected ( a :: (SLProxy SNil) ) b =>
    BProxy b

testLoop :: BProxy True
testLoop =
  BProxy ::
    forall b.
    IsConnected ( a :: (SLProxy ("a" :/ SNil)) ) b =>
    BProxy b

testDisconnected :: BProxy False
testDisconnected =
  BProxy ::
    forall b.
    IsConnected ( a :: (SLProxy ("a" :/ SNil)), b :: (SLProxy SNil) ) b =>
    BProxy b

testConnected1 :: BProxy True
testConnected1 =
  BProxy ::
    forall b.
    IsConnected ( a :: (SLProxy ("a" :/ SNil)), b :: (SLProxy ("a" :/ SNil)) ) b =>
    BProxy b

testConnected2 :: BProxy True
testConnected2 =
  BProxy ::
    forall b.
    IsConnected ( a :: (SLProxy ("b" :/ SNil)), b :: (SLProxy ("c" :/ SNil)) ) b =>
    BProxy b

testConnected3 :: BProxy True
testConnected3 =
  BProxy ::
    forall b.
    IsConnected ( b :: (SLProxy ("c" :/ SNil)), a :: (SLProxy ("b" :/ SNil)) ) b =>
    BProxy b

testConnected4 :: BProxy False
testConnected4 =
  BProxy ::
    forall b.
    IsConnected ( b :: (SLProxy ("c" :/ SNil)), a :: (SLProxy ("q" :/ SNil)) ) b =>
    BProxy b

testConnected :: BProxy True
testConnected =
  BProxy ::
    forall b.
    IsConnected ( a :: (SLProxy ("b" :/ SNil)), b :: (SLProxy SNil) ) b =>
    BProxy b

testAllEdges :: SLProxy ("a" :/ SNil)
testAllEdges =
  SLProxy ::
    forall sl.
    AllEdges ( "q" :: SLProxy ("a" :/ SNil) ) sl =>
    SLProxy sl

testAllEdges1 :: SLProxy ("r" :/ "p" :/ "a" :/ SNil)
testAllEdges1 =
  SLProxy ::
    forall sl.
    AllEdges ( "q" :: SLProxy ("a" :/ SNil), "f" :: SLProxy ("r" :/ "p" :/ SNil) ) sl =>
    SLProxy sl

testAllEdges2 :: SLProxy ("p" :/ "a" :/ "r" :/ SNil)
testAllEdges2 =
  SLProxy ::
    forall sl.
    AllEdges ( "q" :: SLProxy ("a" :/ "r" :/ SNil), "f" :: SLProxy ("r" :/ "p" :/ SNil) ) sl =>
    SLProxy sl

hasOprhanNodes0 :: BProxy True
hasOprhanNodes0 =
  BProxy ::
    forall b.
    HasOrphanNodes ( "q" :: SLProxy ("a" :/ "r" :/ SNil), "f" :: SLProxy ("r" :/ "p" :/ SNil) ) b =>
    BProxy b

hasOprhanNodes1 :: BProxy False
hasOprhanNodes1 =
  BProxy ::
    forall b.
    HasOrphanNodes ( "a" :: SLProxy SNil, "r" :: SLProxy SNil, "p" :: SLProxy SNil, "q" :: SLProxy ("a" :/ "r" :/ SNil), "f" :: SLProxy ("r" :/ "p" :/ SNil) ) b =>
    BProxy b

main :: Effect Unit
main = do
  log "I <3 types!"
