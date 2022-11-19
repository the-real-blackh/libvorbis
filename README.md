# libogg, libvorbis

Raw bindings to the libogg (version 1.3.5) and libvorbis (version 1.3.7),
available at [xiph.org](https://www.xiph.org).

These packages reexport the
[Foreign.Storable.Offset](https://hackage.haskell.org/package/storable-offset-0.1.0.0/docs/Foreign-Storable-Offset.html)
module and implement the `Offset` instance for all the datatypes.
Alas Hackage currently does not show this (as per [haddock#563](https://github.com/haskell/haddock/issues/563)).

Caveats of this library:
- `DuplicateRecordFields` (since GHC 9.2 also `NoFieldSelectors`) are turned on in all modules with datatype
  definitions, record field names are the same as in the C library.
  You should use `GHC.Records.getField` or record dot syntax to access datatype fields;

- `data` record field is replaced with`data_`. `GHC.Records.HasField`
  and `Foreign.Storable.Offset.Offset` instances are defined over both variants;

- Functions that use `ov_callbacks` are wrapped to transfer that value using a pointer.

## Maintenance
Some bindings are bound to have errors in them, feel free to report them through Github.
