module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Boolean (False, True)
import Prim.RowList (class RowToList)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Graph (class AllEdges, class Concat, class Connected, class FlipDirection, class HasDuplicateEdges, class HasDuplicateNodes, class HasOrphanNodes, class HasSymbol, class HasUniqueTerminus, class IncomingNodes, class IsConnected, class Lookup, class NTermini, class NoDuplicateEdges, class NoDuplicateNodes, class NoOrphanedNodes, class RemoveDuplicates, class Traversal, class WithUniqueTerminus, type (:/))
import Type.Data.Peano (D3, NProxy(..), Z)
import Type.Data.Row (RProxy(..))

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

testTraversal1 :: SLProxy ("a" :/ "q" :/  SNil)
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

testConnected5 :: BProxy True
testConnected5 =
  BProxy ::
    forall b.
    IsConnected
      ( b :: (SLProxy ("c" :/ "d" :/ SNil))
      , d :: (SLProxy SNil)
      , a :: (SLProxy ("b" :/ SNil))
      )
      b =>
    BProxy b

connected :: forall (graph :: # Type). Connected graph => Record graph -> Unit
connected _ = unit

r =
  connected
    { b: SLProxy :: (SLProxy ("c" :/ "d" :/ SNil))
    , d: SLProxy :: (SLProxy SNil)
    , a: SLProxy :: (SLProxy ("b" :/ SNil))
    } ::
    Unit

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
    HasOrphanNodes
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    BProxy b

hasOprhanNodes1 :: BProxy False
hasOprhanNodes1 =
  BProxy ::
    forall b.
    HasOrphanNodes
      ( "a" :: SLProxy SNil
      , "r" :: SLProxy SNil
      , "p" :: SLProxy SNil
      , "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    BProxy b

noOrphanedNodes :: forall (graph :: # Type). NoOrphanedNodes graph => Record graph -> Unit
noOrphanedNodes _ = unit

z =
  noOrphanedNodes
    { a: SLProxy :: SLProxy SNil
    , r: SLProxy :: SLProxy SNil
    , p: SLProxy :: SLProxy SNil
    , q: SLProxy :: SLProxy ("a" :/ "r" :/ SNil)
    , f: SLProxy :: SLProxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

nTermini1 :: NProxy D3
nTermini1 =
  NProxy ::
    forall n.
    NTermini
      ( "a" :: SLProxy SNil
      , "r" :: SLProxy SNil
      , "p" :: SLProxy SNil
      , "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      n =>
    NProxy n

nTermini2 :: NProxy Z
nTermini2 =
  NProxy ::
    forall n.
    NTermini
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      n =>
    NProxy n

hasUniqueTerminus1 :: BProxy False
hasUniqueTerminus1 =
  BProxy ::
    forall b.
    HasUniqueTerminus
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    BProxy b

hasUniqueTerminus2 :: BProxy True
hasUniqueTerminus2 =
  BProxy ::
    forall b.
    HasUniqueTerminus
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    BProxy b

withUniqueTerminus :: forall (graph :: # Type). WithUniqueTerminus graph => Record graph -> Unit
withUniqueTerminus _ = unit

w =
  withUniqueTerminus
    { "q": SLProxy :: SLProxy ("a" :/ "r" :/ SNil)
    , "a": SLProxy :: SLProxy SNil
    , "f": SLProxy :: SLProxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

hasDuplicateNodes1 :: BProxy False
hasDuplicateNodes1 =
  BProxy ::
    forall b.
    HasDuplicateNodes
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    BProxy b

ndnTest :: forall (graph :: # Type). NoDuplicateNodes graph => Record graph -> Unit
ndnTest _ = unit

x =
  ndnTest
    { q: SLProxy :: SLProxy ("a" :/ "r" :/ SNil)
    , a: SLProxy :: SLProxy SNil
    , f: SLProxy :: SLProxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

hasDuplicateNodes2 :: BProxy True
hasDuplicateNodes2 =
  BProxy ::
    forall b.
    HasDuplicateNodes
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      , "q" :: SLProxy SNil
      )
      b =>
    BProxy b

hasDuplicateEdges1 :: BProxy False
hasDuplicateEdges1 =
  BProxy ::
    forall b.
    HasDuplicateEdges
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    BProxy b

ndeTest :: forall (graph :: # Type). NoDuplicateEdges graph => Record graph -> Unit
ndeTest _ = unit

o =
  ndeTest
    { q: SLProxy :: SLProxy ("a" :/ "r" :/ SNil)
    , a: SLProxy :: SLProxy SNil
    , f: SLProxy :: SLProxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

hasDuplicateEdges2 :: BProxy False
hasDuplicateEdges2 =
  BProxy ::
    forall b.
    HasDuplicateEdges
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      , "q" :: SLProxy SNil
      )
      b =>
    BProxy b

hasDuplicateEdges3 :: BProxy True
hasDuplicateEdges3 =
  BProxy ::
    forall b.
    HasDuplicateEdges
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "r" :/ "p" :/ SNil)
      , "q" :: SLProxy SNil
      )
      b =>
    BProxy b

incomingNodes1 :: SLProxy ("q" :/ SNil)
incomingNodes1 =
  SLProxy ::
    forall sl.
    IncomingNodes
      "a"
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      , "q" :: SLProxy SNil
      )
      sl =>
    SLProxy sl

incomingNodes2 :: SLProxy SNil
incomingNodes2 =
  SLProxy ::
    forall sl.
    IncomingNodes
      "q"
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      , "q" :: SLProxy SNil
      )
      sl =>
    SLProxy sl

incomingNodes3 :: SLProxy ("f" :/ "q" :/ SNil)
incomingNodes3 =
  SLProxy ::
    forall sl.
    IncomingNodes
      "a"
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("a" :/ "p" :/ SNil)
      )
      sl =>
    SLProxy sl

flipDirection ::
  RProxy
    ( "q" :: SLProxy SNil
    , "a" :: SLProxy ("f" :/ "q" :/ SNil)
    , "f" :: SLProxy SNil
    , "r" :: SLProxy ("q" :/ SNil)
    , "p" :: SLProxy ("f" :/ SNil)
    )
flipDirection =
  RProxy ::
    forall (r :: # Type).
    FlipDirection
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "a" :: SLProxy SNil
      , "f" :: SLProxy ("a" :/ "p" :/ SNil)
      , "r" :: SLProxy SNil
      , "p" :: SLProxy SNil
      )
      r =>
    RProxy r

---------------- from audio behaviors

a_flipDirection ::
  RProxy
    ( "combine" :: SLProxy SNil
      , "gain" :: SLProxy ("combine" :/ SNil)
      , "del" :: SLProxy ("gain" :/ SNil)
      , "filt" :: SLProxy ("del" :/ SNil)
      , "mic" :: SLProxy ("combine" :/ "filt" :/  SNil)
      )
a_flipDirection =
  RProxy ::
    forall (r :: # Type).
    FlipDirection
      ( "combine" :: SLProxy ("gain" :/ "mic" :/ SNil)
      , "gain" :: SLProxy ("del" :/ SNil)
      , "del" :: SLProxy ("filt" :/ SNil)
      , "filt" :: SLProxy ("mic" :/ SNil)
      , "mic" :: SLProxy SNil
      )
      r =>
    RProxy r

a_testConnectedWithFlip :: BProxy True
a_testConnectedWithFlip =
  BProxy ::
    forall (r :: # Type) b.
    FlipDirection
      ( "combine" :: SLProxy ("gain" :/ "mic" :/ SNil)
      , "gain" :: SLProxy ("del" :/ SNil)
      , "del" :: SLProxy ("filt" :/ SNil)
      , "filt" :: SLProxy ("mic" :/ SNil)
      , "mic" :: SLProxy SNil
      )
      r =>
    IsConnected r b =>
    BProxy b

main :: Effect Unit
main = do
  log "I <3 types!"
