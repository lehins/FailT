# Changelog for `FailT`

## 0.1.2.0

* Compatiblity with mtl-2.3
* Add `failManyT`

## 0.1.1.0

* Remove unnecessary `IsString` constraint from instance definitions for GHC >= 8.10
* Add `MonadFix` instance.
* Add `MonadRWS` instance.
* Add `MonadAccum` and `MonadSelect` instances, whenever mtl-2.3 or newer is used.
* Add `MonadThrow` instance.
* Fix a bug in `Applicative` instance. Implementation for `*>` did not short circuit
  mondic computation.
* Add `throwErrorFailT` that uses `MonadError` interface.

## 0.1.0.0

* Initial release
