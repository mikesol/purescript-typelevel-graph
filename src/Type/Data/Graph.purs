module Type.Data.Graph where

import Prim.Boolean (False, True)
import Prim.Ordering (EQ,  Ordering)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Compare)
import Prim.TypeError (class Fail, Text)
import Type.Data.Boolean (class Not)
import Type.Proxy (Proxy)
import Type.Data.Peano (class CompareNat, class SumNat, Succ, Z, Nat)
import Type.RowList (class ListToRow)

data SList

foreign import data SCons :: Symbol -> SList -> SList

foreign import data SNil :: SList

infixr 4 type SCons as :/

class IsEq :: Ordering -> Boolean -> Constraint
class IsEq o b | o -> b

instance isEqEq :: IsEq EQ True
else instance isEqOther :: IsEq a False

class HasSymbol :: SList -> Symbol -> Boolean -> Constraint
class HasSymbol l s b | l s -> b

class HasSymbolInternal :: Boolean -> SList -> Symbol -> Boolean -> Constraint
class HasSymbolInternal gate l s b | gate l s -> b

instance hasSymbolInternalSConsFalse ::
  ( Compare h s o
  , IsEq o x
  , HasSymbolInternal x t s b
  ) =>
  HasSymbolInternal False (SCons h t) s b
else instance hasSymbolInternalSConsTrue :: HasSymbolInternal True (SCons h t) s True
else instance hasSymbolInternalNil :: HasSymbolInternal b SNil s b

instance hasSymbol :: (HasSymbolInternal False l s b) => HasSymbol l s b

class SListGate :: Boolean -> SList -> SList -> SList -> Constraint
class SListGate b left right o | b left right -> o

instance sListGateL :: SListGate True left right left
else instance sListGateR :: SListGate False left right right
class NatGate :: Boolean -> Nat -> Nat -> Nat -> Constraint
class NatGate b left right o | b left right -> o

instance natGateL :: NatGate True left right left
else instance natGateR :: NatGate False left right right

class ReverseInternal :: SList -> SList -> SList -> Constraint 
class ReverseInternal acc s r | acc s -> r

class Reverse :: SList -> SList -> Constraint
class Reverse (s :: SList) (r :: SList) | s -> r, r -> s

instance reverse :: ReverseInternal SNil s r => Reverse s r

instance reverseInternalNil :: ReverseInternal acc SNil acc

instance reverseInternalNilCons ::
  ( ReverseInternal (SCons h acc) t v
    ) =>
  ReverseInternal acc (SCons h t) v

class Concat :: SList -> SList -> SList -> Constraint
class Concat l r t | l r -> t

class ConcatInternal :: SList -> SList -> SList -> Constraint
class ConcatInternal l r t | l r -> t

instance concatInternalNil :: ConcatInternal SNil r r

instance concatInternalCons :: (ConcatInternal t (SCons h r) v) => ConcatInternal (SCons h t) r v

instance concat :: (Reverse l rev, ConcatInternal rev r t) => Concat l r t

class RowListKeys :: forall k. RowList k -> SList -> Constraint
class RowListKeys rl sl | rl -> sl

instance rowListKeysNil :: RowListKeys Nil SNil

instance rowListKeysCons :: (RowListKeys t x) => RowListKeys (Cons k v t) (SCons k x)

class  TraversalWithMapInternal :: forall k. Boolean -> SList -> Symbol -> RowList k -> SList -> Constraint
class TraversalWithMapInternal gate acc s graph nodes | gate acc s graph -> nodes

class TraversalInternal :: forall k. Boolean -> SList -> Symbol -> RowList k -> SList -> Constraint
class TraversalInternal gate acc s graph nodes | gate acc s graph -> nodes

class LookupInternal :: forall k. SList -> Symbol -> RowList k -> SList -> Constraint
class LookupInternal gate  s g r | gate s g -> r

class TraversalWithFoldInternal :: forall k. SList -> SList -> RowList k -> SList -> Constraint
class TraversalWithFoldInternal acc l graph nodes | acc l graph -> nodes

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
  LookupInternal SNil s (Cons k (Proxy v) t) res

class Lookup :: forall k. Symbol -> RowList k -> SList -> Constraint
class Lookup s g r | s g -> r

instance lookup :: LookupInternal SNil s g r => Lookup s g r

instance traversalWithMapInternalTrue ::
  ( Lookup s graph res
  , TraversalWithFoldInternal acc res graph nodes
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

class Traversal :: forall k. Symbol -> Row k -> SList -> Constraint
class Traversal s graph nodes | s graph -> nodes

instance traversal :: (RowToList graph gl, TraversalInternal True SNil s gl nodes) => Traversal s graph nodes

class RemoveDuplicates :: SList -> SList -> Constraint
class RemoveDuplicates l s | l -> s

instance removeDuplicatesNil :: RemoveDuplicates SNil SNil

instance removeDuplicatesCons ::
  ( HasSymbol t h b
  , SListGate b SNil (SCons h SNil) o
  , RemoveDuplicates t rest
  , Concat o rest v
  ) =>
  RemoveDuplicates (SCons h t) v

class ProoveConnectivity :: forall k1 k2. SList -> RowList k1 -> Row k2 -> Boolean -> Constraint
class ProoveConnectivity nodes l g b | nodes l g -> b

class ProoveConnectivityInternal :: forall k1 k2. Boolean -> SList -> RowList k1 -> Row k2 -> Boolean -> Constraint
class ProoveConnectivityInternal gate nodes l g b | gate nodes l g -> b

class Length :: SList -> Nat -> Constraint
class Length s z | s -> z

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

class IsConnectedInternal :: forall k1 k2. RowList k1 -> Row k2 -> Boolean -> Constraint
class IsConnectedInternal graph wholeGraph b | graph wholeGraph -> b

class IsConnected :: forall k. Row k -> Boolean -> Constraint
class IsConnected graph  b | graph -> b

instance isConnected :: (RowToList graph gl, IsConnectedInternal gl graph b) => IsConnected graph b

class Connected :: forall k. Row k -> Constraint
class Connected graph

instance connected :: (IsConnected graph True) => Connected graph
else instance connectedFail :: (Fail (Text "Graph is not connected"), IsConnected graph True) => Connected graph

class AllNodes :: forall k. Row k -> SList -> Constraint
class AllNodes graph nodes | graph -> nodes

class AllNodesInternal :: forall k. RowList k -> SList -> Constraint
class AllNodesInternal graph nodes | graph -> nodes

instance allNodes :: (RowToList graph gl, AllNodesInternal gl nodes) => AllNodes graph nodes

instance allNodesInternalNil :: AllNodesInternal Nil SNil

instance allNodesInternalCons :: AllNodesInternal tail nodes => AllNodesInternal (Cons k v tail) (SCons k nodes)

class AllEdgesInternal :: forall k. SList -> RowList k -> SList -> Constraint
class AllEdgesInternal acc graph edges | acc graph -> edges

instance allEdgesInternalNil :: AllEdgesInternal acc Nil acc

instance allEdgesInternalCons ::
  ( Concat acc v oacc
  , AllEdgesInternal oacc t edges
  ) =>
  AllEdgesInternal acc (Cons k (Proxy v) t) edges

class AllEdges :: forall k. Row k -> SList -> Constraint
class AllEdges graph edges| graph -> edges

instance allEdges :: (RowToList graph gl, AllEdgesInternal SNil gl o, RemoveDuplicates o edges) => AllEdges graph edges

class IsTerminus :: forall k. Symbol -> Row k -> Boolean -> Constraint
class IsTerminus k graph b| k graph -> b

instance isTerminus ::
  ( RowToList graph gl, Lookup k gl r, Length r nedges, CompareNat nedges Z e, IsEq e b
  ) =>
  IsTerminus k graph b

class NTerminiInternal :: forall k1 k2. RowList k1 -> Row k2 -> Nat -> Constraint
class NTerminiInternal gl graph n | gl -> n

instance nTerminiInternalNil :: NTerminiInternal Nil graph Z

instance nTerminiInternalCons ::
  ( IsTerminus h graph it
  , NatGate it (Succ Z) Z toAdd
  , NTerminiInternal t graph rest
  , SumNat toAdd rest o
  ) =>
  NTerminiInternal (Cons h (Proxy v) t) graph o

class NTermini :: forall k. Row k -> Nat -> Constraint
class NTermini graph n | graph -> n

instance nTermini :: (RowToList graph gl, NTerminiInternal gl graph n) => NTermini graph n

class HasUniqueTerminus :: forall k. Row k -> Boolean -> Constraint
class HasUniqueTerminus graph b | graph -> b

instance hasUniqueTerminus ::
  ( NTermini graph n, CompareNat n (Succ Z) e, IsEq e b
  ) =>
  HasUniqueTerminus graph b

class WithUniqueTerminus :: forall k. Row k -> Constraint
class WithUniqueTerminus graph

instance withUniqueTerminus :: (HasUniqueTerminus graph True) => WithUniqueTerminus graph
else instance withUniqueTerminusFail :: (Fail (Text "This graph either has no terminus or has multiple termini"), HasUniqueTerminus graph False) => WithUniqueTerminus graph

class HasOrphanNodes :: forall k. Row k -> Boolean -> Constraint
class HasOrphanNodes graph b | graph -> b

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

class NoOrphanedNodes :: forall k. Row k -> Constraint
class NoOrphanedNodes graph

instance noOrphanedNodes :: (HasOrphanNodes graph False) => NoOrphanedNodes graph
else instance noOrphanedNodesFail :: (Fail (Text "This graph has orphaned nodes"), HasOrphanNodes graph True) => NoOrphanedNodes graph

class HasDuplicateNodes :: forall k. Row k -> Boolean -> Constraint
class HasDuplicateNodes graph b |  graph -> b

instance hasDuplicateNodes ::
  ( AllNodes graph allNodes
  , RemoveDuplicates allNodes allNodesx
  , Length allNodes lAllNodes
  , Length allNodesx lAllNodesx
  , CompareNat lAllNodes lAllNodesx e
  , IsEq e bNot
  , Not bNot b
  ) =>
  HasDuplicateNodes graph b

class NoDuplicateNodes :: forall k. Row k -> Constraint
class NoDuplicateNodes graph

instance noDuplicateNodes :: (HasDuplicateNodes graph False) => NoDuplicateNodes graph
else instance noDuplicateNodesFail :: (Fail (Text "This graph has duplicate nodes"), HasDuplicateNodes graph True) => NoDuplicateNodes graph

instance hasDuplicateEdgesInternalTrue :: HasDuplicateEdgesInternal True graph True
else instance hasDuplicateEdgesInternalNil :: HasDuplicateEdgesInternal v Nil v
else instance hasDuplicateEdgesInternal ::
  ( RemoveDuplicates v d
  , Length v lv
  , Length d ld
  , CompareNat lv ld e
  , IsEq e bNot
  , Not bNot toLoop
  , HasDuplicateEdgesInternal toLoop t b
  ) =>
  HasDuplicateEdgesInternal False (Cons k (Proxy v) t) b

class HasDuplicateEdgesInternal :: forall k. Boolean -> RowList k -> Boolean -> Constraint
class HasDuplicateEdgesInternal gate graph b | gate graph -> b

class HasDuplicateEdges :: forall k. Row k -> Boolean -> Constraint
class HasDuplicateEdges graph b | graph -> b

instance hasDuplicateEdges :: (RowToList graph gl, HasDuplicateEdgesInternal False gl b) => HasDuplicateEdges graph b

class NoDuplicateEdges :: forall k. Row k -> Constraint
class NoDuplicateEdges graph

instance noDuplicateEdges :: (HasDuplicateEdges graph False) => NoDuplicateEdges graph
else instance noDuplicateEdgesFail ::
  ( Fail (Text "This graph has duplicate nodes"), HasDuplicateEdges graph True
  ) =>
  NoDuplicateEdges graph

class IncomingNodesInternal :: forall k. Symbol -> RowList k -> SList -> Constraint
class IncomingNodesInternal s graph l | s graph -> l

instance incomingNodesInternalNil :: IncomingNodesInternal s Nil SNil

instance incomingNodesInternalCons ::
  ( HasSymbol v s b
  , SListGate b (SCons k SNil) SNil o
  , IncomingNodesInternal s t oo
  , Concat o oo l
  ) =>
  IncomingNodesInternal s (Cons k (Proxy v) t) l

class IncomingNodes :: forall k. Symbol -> Row k -> SList -> Constraint
class IncomingNodes s graph l | s graph -> l

instance incomingNodes ::
  ( RowToList graph gl
  , IncomingNodesInternal s gl l
  ) =>
  IncomingNodes s graph l

instance flipDirectionInternalNil :: FlipDirectionInternal Nil g Nil

instance flipDirectionInternalCons ::
  ( IncomingNodes k g res
  , FlipDirectionInternal t g rest
  ) =>
  FlipDirectionInternal (Cons k v t) g (Cons k (Proxy res) rest)

class FlipDirectionInternal :: forall k1 k2 k3. RowList k1 -> Row k2 -> RowList k3 -> Constraint
class FlipDirectionInternal graphA graph graphB | graphA graph -> graphB

class FlipDirection :: forall k1 k2. Row k1 -> Row k2 -> Constraint
class FlipDirection graphA graphB | graphA -> graphB

instance flipDirection ::
  ( RowToList graphA glA
  , FlipDirectionInternal glA graphA glB
  , ListToRow glB graphB
  ) =>
  FlipDirection graphA graphB
