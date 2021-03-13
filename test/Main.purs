module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Boolean (False, True)
import Prim.RowList (class RowToList)
import Type.Data.Graph (class AllEdges, SNil, class Concat, class Connected, class FlipDirection, class HasDuplicateEdges, class HasDuplicateNodes, class HasOrphanNodes, class HasSymbol, class HasUniqueTerminus, class IncomingNodes, class IsConnected, class Lookup, class NTermini, class NoDuplicateEdges, class NoDuplicateNodes, class NoOrphanedNodes, class RemoveDuplicates, class Traversal, class WithUniqueTerminus, type (:/))
import Type.Data.Peano (D3, Z)
import Type.Proxy (Proxy(..))

testHasSymbol :: Proxy True
testHasSymbol =
  Proxy ::
    forall b.
    HasSymbol ("a" :/ "b" :/ SNil) "a" b =>
    Proxy b

testHasSymbol' :: Proxy False
testHasSymbol' =
  Proxy ::
    forall b.
    HasSymbol ("a" :/ "b" :/ SNil) "q" b =>
    Proxy b

testHasSymbol'' :: Proxy True
testHasSymbol'' =
  Proxy ::
    forall b.
    HasSymbol ("a" :/ "b" :/ SNil) "b" b =>
    Proxy b

testHasSymbol''_ :: Proxy False
testHasSymbol''_ =
  Proxy ::
    forall b.
    HasSymbol (SNil) "b" b =>
    Proxy b

testLookup :: Proxy ("a" :/ "b" :/ SNil)
testLookup =
  Proxy ::
    forall rl sl.
    RowToList ( "q" :: (Proxy ("a" :/ "b" :/ SNil)) ) rl =>
    Lookup "q" rl sl =>
    Proxy sl

testLookup1 :: Proxy ("a" :/ "b" :/ SNil)
testLookup1 =
  Proxy ::
    forall rl sl.
    RowToList ( "r" :: (Proxy ("x" :/ "b" :/ SNil)), "q" :: (Proxy ("a" :/ "b" :/ SNil)) ) rl =>
    Lookup "q" rl sl =>
    Proxy sl

testLookup2 :: Proxy ("a" :/ "b" :/ SNil)
testLookup2 =
  Proxy ::
    forall rl sl.
    RowToList ( "q" :: (Proxy ("a" :/ "b" :/ SNil)), "r" :: (Proxy ("x" :/ "b" :/ SNil)) ) rl =>
    Lookup "q" rl sl =>
    Proxy sl

testLookup3 :: Proxy SNil
testLookup3 =
  Proxy ::
    forall rl sl.
    RowToList ( "q" :: (Proxy ("a" :/ "b" :/ SNil)), "r" :: (Proxy ("x" :/ "b" :/ SNil)) ) rl =>
    Lookup "f" rl sl =>
    Proxy sl

testConcat :: Proxy ("a" :/ "b" :/ "c" :/ SNil)
testConcat =
  Proxy ::
    forall sl.
    Concat ("a" :/ "b" :/ SNil) ("c" :/ SNil) sl =>
    Proxy sl

testRemoveDuplicates :: Proxy ("a" :/ SNil)
testRemoveDuplicates =
  Proxy ::
    forall sl.
    RemoveDuplicates ("a" :/ SNil) sl =>
    Proxy sl

testRemoveDuplicates1 :: Proxy ("a" :/ SNil)
testRemoveDuplicates1 =
  Proxy ::
    forall sl.
    RemoveDuplicates ("a" :/ "a" :/ SNil) sl =>
    Proxy sl

testRemoveDuplicates2 :: Proxy ("b" :/ "a" :/ SNil)
testRemoveDuplicates2 =
  Proxy ::
    forall sl.
    RemoveDuplicates ("a" :/ "b" :/ "a" :/ SNil) sl =>
    Proxy sl

testTraversal0 :: Proxy ("q" :/ SNil)
testTraversal0 =
  Proxy ::
    forall sl.
    Traversal "q" ( "q" :: Proxy SNil ) sl =>
    Proxy sl

testTraversal1 :: Proxy ("a" :/ "q" :/ SNil)
testTraversal1 =
  Proxy ::
    forall sl.
    Traversal "q" ( "q" :: Proxy ("a" :/ SNil) ) sl =>
    Proxy sl

testEmptyGraph :: Proxy True
testEmptyGraph =
  Proxy ::
    forall b.
    IsConnected () b =>
    Proxy b

testSingletonGraph :: Proxy True
testSingletonGraph =
  Proxy ::
    forall b.
    IsConnected ( a :: (Proxy SNil) ) b =>
    Proxy b

testLoop :: Proxy True
testLoop =
  Proxy ::
    forall b.
    IsConnected ( a :: (Proxy ("a" :/ SNil)) ) b =>
    Proxy b

testDisconnected :: Proxy False
testDisconnected =
  Proxy ::
    forall b.
    IsConnected ( a :: (Proxy ("a" :/ SNil)), b :: (Proxy SNil) ) b =>
    Proxy b

testConnected1 :: Proxy True
testConnected1 =
  Proxy ::
    forall b.
    IsConnected ( a :: (Proxy ("a" :/ SNil)), b :: (Proxy ("a" :/ SNil)) ) b =>
    Proxy b

testConnected2 :: Proxy True
testConnected2 =
  Proxy ::
    forall b.
    IsConnected ( a :: (Proxy ("b" :/ SNil)), b :: (Proxy ("c" :/ SNil)) ) b =>
    Proxy b

testConnected3 :: Proxy True
testConnected3 =
  Proxy ::
    forall b.
    IsConnected ( b :: (Proxy ("c" :/ SNil)), a :: (Proxy ("b" :/ SNil)) ) b =>
    Proxy b

testConnected4 :: Proxy False
testConnected4 =
  Proxy ::
    forall b.
    IsConnected ( b :: (Proxy ("c" :/ SNil)), a :: (Proxy ("q" :/ SNil)) ) b =>
    Proxy b

testConnected5 :: Proxy True
testConnected5 =
  Proxy ::
    forall b.
    IsConnected
      ( b :: (Proxy ("c" :/ "d" :/ SNil))
      , d :: (Proxy SNil)
      , a :: (Proxy ("b" :/ SNil))
      )
      b =>
    Proxy b

connected :: forall graph. Connected graph => Record graph -> Unit
connected _ = unit

r =
  connected
    { b: Proxy :: (Proxy ("c" :/ "d" :/ SNil))
    , d: Proxy :: (Proxy SNil)
    , a: Proxy :: (Proxy ("b" :/ SNil))
    } ::
    Unit

testConnected :: Proxy True
testConnected =
  Proxy ::
    forall b.
    IsConnected ( a :: (Proxy ("b" :/ SNil)), b :: (Proxy SNil) ) b =>
    Proxy b

testAllEdges :: Proxy ("a" :/ SNil)
testAllEdges =
  Proxy ::
    forall sl.
    AllEdges ( "q" :: Proxy ("a" :/ SNil) ) sl =>
    Proxy sl

testAllEdges1 :: Proxy ("r" :/ "p" :/ "a" :/ SNil)
testAllEdges1 =
  Proxy ::
    forall sl.
    AllEdges ( "q" :: Proxy ("a" :/ SNil), "f" :: Proxy ("r" :/ "p" :/ SNil) ) sl =>
    Proxy sl

testAllEdges2 :: Proxy ("p" :/ "a" :/ "r" :/ SNil)
testAllEdges2 =
  Proxy ::
    forall sl.
    AllEdges ( "q" :: Proxy ("a" :/ "r" :/ SNil), "f" :: Proxy ("r" :/ "p" :/ SNil) ) sl =>
    Proxy sl

hasOprhanNodes0 :: Proxy True
hasOprhanNodes0 =
  Proxy ::
    forall b.
    HasOrphanNodes
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

hasOprhanNodes1 :: Proxy False
hasOprhanNodes1 =
  Proxy ::
    forall b.
    HasOrphanNodes
      ( "a" :: Proxy SNil
      , "r" :: Proxy SNil
      , "p" :: Proxy SNil
      , "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

noOrphanedNodes :: forall graph. NoOrphanedNodes graph => Record graph -> Unit
noOrphanedNodes _ = unit

z =
  noOrphanedNodes
    { a: Proxy :: Proxy SNil
    , r: Proxy :: Proxy SNil
    , p: Proxy :: Proxy SNil
    , q: Proxy :: Proxy ("a" :/ "r" :/ SNil)
    , f: Proxy :: Proxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

nTermini1 :: Proxy D3
nTermini1 =
  Proxy ::
    forall n.
    NTermini
      ( "a" :: Proxy SNil
      , "r" :: Proxy SNil
      , "p" :: Proxy SNil
      , "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      n =>
    Proxy n

nTermini2 :: Proxy Z
nTermini2 =
  Proxy ::
    forall n.
    NTermini
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      n =>
    Proxy n

hasUniqueTerminus1 :: Proxy False
hasUniqueTerminus1 =
  Proxy ::
    forall b.
    HasUniqueTerminus
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

hasUniqueTerminus2 :: Proxy True
hasUniqueTerminus2 =
  Proxy ::
    forall b.
    HasUniqueTerminus
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

withUniqueTerminus :: forall graph. WithUniqueTerminus graph => Record graph -> Unit
withUniqueTerminus _ = unit

w =
  withUniqueTerminus
    { "q": Proxy :: Proxy ("a" :/ "r" :/ SNil)
    , "a": Proxy :: Proxy SNil
    , "f": Proxy :: Proxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

hasDuplicateNodes1 :: Proxy False
hasDuplicateNodes1 =
  Proxy ::
    forall b.
    HasDuplicateNodes
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

ndnTest :: forall graph. NoDuplicateNodes graph => Record graph -> Unit
ndnTest _ = unit

x =
  ndnTest
    { q: Proxy :: Proxy ("a" :/ "r" :/ SNil)
    , a: Proxy :: Proxy SNil
    , f: Proxy :: Proxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

hasDuplicateNodes2 :: Proxy True
hasDuplicateNodes2 =
  Proxy ::
    forall b.
    HasDuplicateNodes
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      , "q" :: Proxy SNil
      )
      b =>
    Proxy b

hasDuplicateEdges1 :: Proxy False
hasDuplicateEdges1 =
  Proxy ::
    forall b.
    HasDuplicateEdges
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

ndeTest :: forall graph. NoDuplicateEdges graph => Record graph -> Unit
ndeTest _ = unit

o =
  ndeTest
    { q: Proxy :: Proxy ("a" :/ "r" :/ SNil)
    , a: Proxy :: Proxy SNil
    , f: Proxy :: Proxy ("r" :/ "p" :/ SNil)
    } ::
    Unit

hasDuplicateEdges2 :: Proxy False
hasDuplicateEdges2 =
  Proxy ::
    forall b.
    HasDuplicateEdges
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      , "q" :: Proxy SNil
      )
      b =>
    Proxy b

hasDuplicateEdges3 :: Proxy True
hasDuplicateEdges3 =
  Proxy ::
    forall b.
    HasDuplicateEdges
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "r" :/ "p" :/ SNil)
      , "q" :: Proxy SNil
      )
      b =>
    Proxy b

incomingNodes1 :: Proxy ("q" :/ SNil)
incomingNodes1 =
  Proxy ::
    forall sl.
    IncomingNodes
      "a"
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      , "q" :: Proxy SNil
      )
      sl =>
    Proxy sl

incomingNodes2 :: Proxy SNil
incomingNodes2 =
  Proxy ::
    forall sl.
    IncomingNodes
      "q"
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("r" :/ "p" :/ SNil)
      , "q" :: Proxy SNil
      )
      sl =>
    Proxy sl

incomingNodes3 :: Proxy ("f" :/ "q" :/ SNil)
incomingNodes3 =
  Proxy ::
    forall sl.
    IncomingNodes
      "a"
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("a" :/ "p" :/ SNil)
      )
      sl =>
    Proxy sl

flipDirection ::
  Proxy
    ( "q" :: Proxy SNil
    , "a" :: Proxy ("f" :/ "q" :/ SNil)
    , "f" :: Proxy SNil
    , "r" :: Proxy ("q" :/ SNil)
    , "p" :: Proxy ("f" :/ SNil)
    )
flipDirection =
  Proxy ::
    forall r.
    FlipDirection
      ( "q" :: Proxy ("a" :/ "r" :/ SNil)
      , "a" :: Proxy SNil
      , "f" :: Proxy ("a" :/ "p" :/ SNil)
      , "r" :: Proxy SNil
      , "p" :: Proxy SNil
      )
      r =>
    Proxy r

---------------- from audio behaviors
a_flipDirection ::
  Proxy
    ( "combine" :: Proxy SNil
    , "gain" :: Proxy ("combine" :/ SNil)
    , "del" :: Proxy ("gain" :/ SNil)
    , "filt" :: Proxy ("del" :/ SNil)
    , "mic" :: Proxy ("combine" :/ "filt" :/ SNil)
    )
a_flipDirection =
  Proxy ::
    forall r.
    FlipDirection
      ( "combine" :: Proxy ("gain" :/ "mic" :/ SNil)
      , "gain" :: Proxy ("del" :/ SNil)
      , "del" :: Proxy ("filt" :/ SNil)
      , "filt" :: Proxy ("mic" :/ SNil)
      , "mic" :: Proxy SNil
      )
      r =>
    Proxy r

a_testConnectedWithFlip :: Proxy True
a_testConnectedWithFlip =
  Proxy ::
    forall r b.
    FlipDirection
      ( "combine" :: Proxy ("gain" :/ "mic" :/ SNil)
      , "gain" :: Proxy ("del" :/ SNil)
      , "del" :: Proxy ("filt" :/ SNil)
      , "filt" :: Proxy ("mic" :/ SNil)
      , "mic" :: Proxy SNil
      )
      r =>
    IsConnected r b =>
    Proxy b

main :: Effect Unit
main = do
  log "I <3 types!"
