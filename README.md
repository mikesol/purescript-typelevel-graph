# purescript-typelevel-graph

Basic typelevel assertions about graphs.

```haskell

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
```
