0.6
---
* On GHC 7.7, we now still export the instances we used to for `Data.Proxy.Proxy` as orphans if need be.

0.5
---
* On GHC 7.7 we now simply export `Data.Typeable.Proxy` rather than make our own type. We still re-export it.

0.4.5
-----
* Added `witness`

0.4.4
-----
* Actually working polymorphic kind support

0.4.3
-----
* Added polymorphic kind support
