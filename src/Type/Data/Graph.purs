module Type.Data.Graph where

import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (EQ, kind Ordering)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.Symbol (class Compare)
import Prim.TypeError (class Fail, Text)
import Record.Extra (SCons, SLProxy, SNil, kind SList)
import Type.Data.Boolean (class Not)
import Type.Data.Peano (class CompareNat, class SumNat, Succ, Z, kind Nat)

infixr 4 type SCons as :/

class IsEq (o :: Ordering) (b :: Boolean) | o -> b

instance isEqEq :: IsEq EQ True
else instance isEqOther :: IsEq a False

class HasSymbol (l :: SList) (s :: Symbol) (b :: Boolean) | l s -> b

class HasSymbolInternal (gate :: Boolean) (l :: SList) (s :: Symbol) (b :: Boolean) | gate l s -> b

instance hasSymbolInternalSConsFalse ::
  ( Compare h s o
  , IsEq o x
  , HasSymbolInternal x t s b
  ) =>
  HasSymbolInternal False (SCons h t) s b
else instance hasSymbolInternalSConsTrue :: HasSymbolInternal True (SCons h t) s True
else instance hasSymbolInternalNil :: HasSymbolInternal b SNil s b

instance hasSymbol :: (HasSymbolInternal False l s b) => HasSymbol l s b

class SListGate (b :: Boolean) (left :: SList) (right :: SList) (o :: SList) | b left right -> o

instance sListGateL :: SListGate True left right left
else instance sListGateR :: SListGate False left right right

class ReverseInternal (acc :: SList) (s :: SList) (r :: SList) | acc s -> r

class Reverse (s :: SList) (r :: SList) | s -> r, r -> s

instance reverse :: ReverseInternal SNil s r => Reverse s r

instance reverseInternalNil :: ReverseInternal acc SNil acc

instance reverseInternalNilCons ::
  ( ReverseInternal (SCons h acc) t v
    ) =>
  ReverseInternal acc (SCons h t) v

class Concat (l :: SList) (r :: SList) (t :: SList) | l r -> t

class ConcatInternal (l :: SList) (r :: SList) (t :: SList) | l r -> t

instance concatInternalNil :: ConcatInternal SNil r r

instance concatInternalCons :: (ConcatInternal t (SCons h r) v) => ConcatInternal (SCons h t) r v

instance concat :: (Reverse l rev, ConcatInternal rev r t) => Concat l r t

class RowListKeys (rl :: RowList) (sl :: SList) | rl -> sl

instance rowListKeysNil :: RowListKeys Nil SNil

instance rowListKeysCons :: (RowListKeys t x) => RowListKeys (Cons k v t) (SCons k x)

class TraversalWithMapInternal (gate :: Boolean) (acc :: SList) (s :: Symbol) (graph :: RowList) (nodes :: SList) | gate acc s graph -> nodes

class TraversalInternal (gate :: Boolean) (acc :: SList) (s :: Symbol) (graph :: RowList) (nodes :: SList) | gate acc s graph -> nodes

class LookupInternal (gate :: SList) (s :: Symbol) (g :: RowList) (r :: SList) | gate s g -> r

class TraversalWithFoldInternal (acc :: SList) (l :: SList) (graph :: RowList) (nodes :: SList) | acc l graph -> nodes

instance traversalWithFoldInternalNil :: TraversalWithFoldInternal acc SNil graph acc

instance traversalWithFoldInternalCons ::
  ( TraversalInternal True acc h graph ol
  , Concat acc ol allx
  , RemoveDuplicates allx all
  , TraversalWithFoldInternal all t graph nodes
  ) =>
  TraversalWithFoldInternal acc (SCons h t) graph nodes

instance lookupInternalDone :: LookupInternal gate s Nil gate
else instance lookupInternalFound :: LookupInternal (SCons h t) s g (SCons h t)
else instance lookupInternalCont ::
  ( Compare s k eq
  , IsEq eq b
  , SListGate b v SNil o
  , LookupInternal o s t res
  ) =>
  LookupInternal SNil s (Cons k (SLProxy v) t) res

class Lookup (s :: Symbol) (g :: RowList) (r :: SList) | s g -> r

instance lookup :: LookupInternal SNil s g r => Lookup s g r

instance traversalWithMapInternalTrue ::
  ( Lookup s graph res
  , Concat acc res accWithResx
  , RemoveDuplicates accWithResx accWithRes
  , TraversalWithFoldInternal accWithRes res graph nodes
  ) =>
  TraversalWithMapInternal True acc s graph nodes

instance traversalWithMapInternalFalse :: TraversalWithMapInternal False acc s graph acc

instance traversalInternalTrue ::
  ( HasSymbol acc s x
  , Not x y
  , Concat (SCons s SNil) acc accx
  , RemoveDuplicates accx accxx
  , TraversalWithMapInternal y accxx s graph nodes
  ) =>
  TraversalInternal True acc s graph nodes

instance traversalInternalFalse :: TraversalInternal False acc s graph acc

class Traversal (s :: Symbol) (graph :: # Type) (nodes :: SList) | s graph -> nodes

instance traversal :: (RowToList graph gl, TraversalInternal True SNil s gl nodes) => Traversal s graph nodes

class RemoveDuplicates (l :: SList) (s :: SList) | l -> s

instance removeDuplicatesNil :: RemoveDuplicates SNil SNil

instance removeDuplicatesCons ::
  ( HasSymbol t h b
  , SListGate b SNil (SCons h SNil) o
  , RemoveDuplicates t rest
  , Concat o rest v
  ) =>
  RemoveDuplicates (SCons h t) v

class ProoveConnectivity (nodes :: SList) (l :: RowList) (g :: # Type) (b :: Boolean) | nodes l g -> b

class ProoveConnectivityInternal (gate :: Boolean) (nodes :: SList) (l :: RowList) (g :: # Type) (b :: Boolean) | gate nodes l g -> b

class Length (s :: SList) (z :: Nat) | s -> z

instance lengthNil :: Length SNil Z

instance lengthCons :: (Length t x) => Length (SCons h t) (Succ x)

instance prooveConnectivityInternalFalse :: ProoveConnectivityInternal False nodes l g False
else instance prooveConnectivityInternalNil :: ProoveConnectivityInternal v nodes Nil g v
else instance prooveConnectivityInternalCons ::
  ( Traversal k g nodes2
  , Concat nodes nodes2 allNodesWithDup
  , RemoveDuplicates allNodesWithDup allNodes
  , Length nodes lNodes
  , Length nodes2 lNodes2
  , Length allNodes lAllNodes
  , SumNat lNodes lNodes2 lCombined
  , CompareNat lCombined lAllNodes natC
  , IsEq natC setsWereDisjoint
  , Not setsWereDisjoint setsWereConnected
  , ProoveConnectivityInternal setsWereConnected allNodes t g b
  ) =>
  ProoveConnectivityInternal True nodes (Cons k v t) g b

instance prooveConnectivity :: ProoveConnectivityInternal True nodes l g b => ProoveConnectivity nodes l g b

instance isConnectedInternalNil :: IsConnectedInternal Nil g True

instance isConnectedInternalCons ::
  ( Traversal k g nodes
  , ProoveConnectivity nodes t g b
  ) =>
  IsConnectedInternal (Cons k v t) g b

class IsConnectedInternal (graph :: RowList) (wholeGraph :: # Type) (b :: Boolean) | graph wholeGraph -> b

class IsConnected (graph :: # Type) (b :: Boolean) | graph -> b

instance isConnected :: (RowToList graph gl, IsConnectedInternal gl graph b) => IsConnected graph b

class Connected (graph :: # Type)

instance connected :: (IsConnected graph True) => Connected graph
else instance connectedFail :: (Fail (Text "Graph is not connected"), IsConnected graph True) => Connected graph

class AllEdgesInternal (acc :: SList) (graph :: RowList) (edges :: SList) | acc graph -> edges

instance allEdgesInternalNil :: AllEdgesInternal acc Nil acc

instance allEdgesInternalCons ::
  ( Concat acc v oacc
  , AllEdgesInternal oacc t edges
  ) =>
  AllEdgesInternal acc (Cons k (SLProxy v) t) edges

class AllEdges (graph :: # Type) (edges :: SList) | graph -> edges

instance allEdges :: (RowToList graph gl, AllEdgesInternal SNil gl o, RemoveDuplicates o edges) => AllEdges graph edges

class HasOrphanNodes (graph :: # Type) (b :: Boolean) | graph -> b

instance hasOrphanedNodes ::
  ( RowToList graph gl
  , RowListKeys gl nodes
  , AllEdges graph edges
  , RemoveDuplicates nodes nodesx
  , RemoveDuplicates edges edgesx
  , Concat nodesx edgesx merged
  , RemoveDuplicates merged mergedx
  , Length mergedx nWhenMerged
  , Length nodesx nNodes
  , CompareNat nNodes nWhenMerged cmp
  , IsEq cmp allNodesAccountedFor
  , Not allNodesAccountedFor b
  ) =>
  HasOrphanNodes graph b

class NoOrphanedNodes (graph :: # Type)

instance noOrphanedNodes :: (HasOrphanNodes graph False) => NoOrphanedNodes graph
else instance noOrphanedNodesFail :: (Fail (Text "This graph has orphaned nodes"), HasOrphanNodes graph True) => NoOrphanedNodes graph
