# servant-serialization

Servant content types and instances for common serialization formats.

This package has instances for:

* [`Show`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Show)/[`Read`](https://hackage.haskell.org/package/base/docs/Text-Read.html#t:Read) (for prototyping)
* [`binary`](https://hackage.haskell.org/package/binary)
* [`cereal`](https://hackage.haskell.org/package/cereal)
* [`serialise` (CBOR)](https://hackage.haskell.org/package/serialise)
* [`persist`](https://hackage.haskell.org/package/persist)
* [`flat`](https://hackage.haskell.org/package/flat)

The example executable in `Main.hs` shows how to use all of these,
though you probably only need one.
