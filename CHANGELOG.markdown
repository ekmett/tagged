0.7
---
* `Data.Proxy` has moved into base as of GHC 7.7 for use in the new `Data.Typeable`. We no longer export
  it for GHC >= 7.7. The most notable change in the module from the migration into base is the loss of
  the `reproxy` function.

0.6.2
-----
* Allowed polymorphic arguments where possible.

0.6.1
-----
* Needlessly claim that this entirely pure package is `Trustworthy`!

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
