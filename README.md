# purescript-typelevel-graph

Basic typelevel assertions about graphs.

```haskell

testConnected3 :: Proxy True
testConnected3 =
  Proxy ::
    forall b.
    IsConnected ( b :: (SLProxy ("c" :/ SNil)), a :: (SLProxy ("b" :/ SNil)) ) b =>
    Proxy b

testConnected4 :: Proxy False
testConnected4 =
  Proxy ::
    forall b.
    IsConnected ( b :: (SLProxy ("c" :/ SNil)), a :: (SLProxy ("q" :/ SNil)) ) b =>
    Proxy b

hasOprhanNodes0 :: Proxy True
hasOprhanNodes0 =
  Proxy ::
    forall b.
    HasOrphanNodes
      ( "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b

hasOprhanNodes1 :: Proxy False
hasOprhanNodes1 =
  Proxy ::
    forall b.
    HasOrphanNodes
      ( "a" :: SLProxy SNil
      , "r" :: SLProxy SNil
      , "p" :: SLProxy SNil
      , "q" :: SLProxy ("a" :/ "r" :/ SNil)
      , "f" :: SLProxy ("r" :/ "p" :/ SNil)
      )
      b =>
    Proxy b
```
