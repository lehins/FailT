# FailT

This package solves a fairly simple, but very common problem of gracefully converting a
monadic computation that uses `MonadFail` into either a result or a string failure
message(s).

## Motivation

When we have a function that can fail in a `MonadFail`, there is no instance in `base`
that would allow us to get the failure message without a runtime exception. It is best to
demonstrate the problem with an example.

Here is
[`formatParseM`](https://hackage.haskell.org/package/time/docs/Data-Time-Format-ISO8601.html#v:formatParseM)
function from the [`time`](https://hackage.haskell.org/package/time) package, which is
designed to parse time:

```haskell
formatParseM :: MonadFail m => Format t -> String -> m t
```

We can use it out of the box with various packages like `aeson`, `attoparsec`, `binary`,
etc. Here is how it could be used to successfully parse a string with time in the `IO`
monad:

```haskell
λ> import Data.Time (UTCTime)
λ> import Data.Time.Format.ISO8601 (formatParseM, iso8601Format)
λ> formatParseM iso8601Format "2023-01-08T00:29:00Z" :: IO UTCTime
2023-01-08 00:29:00 UTC
```

However, when it comes to bad input, there is not a single monad in `base` or other
package that is wired with GHC that has a `MonadFail` instance, which would allow us to
gracefully fail and retrieve the error message. Below are all the instances from `base`:

```haskell
λ> formatParseM iso8601Format "Bad time" :: IO UTCTime
*** Exception: user error (no parse of "Bad time")
λ> formatParseM iso8601Format "Bad time" :: Maybe UTCTime
Nothing
λ> formatParseM iso8601Format "Bad time" :: [UTCTime]
[]
```

## Solution

This is where `FailT` package comes to help:

```haskell
λ> import Control.Monad.Trans.Fail.String
λ> runFail $ formatParseM iso8601Format "Bad time" :: Either String UTCTime
Left "no parse of \"Bad time\""
λ> runFail $ formatParseM iso8601Format "2023-01-08T00:29:00Z" :: Either String UTCTime
Right 2023-01-08 00:29:00 UTC
```

## Features

### Monad transformer

Naturally, as the package name suggests, it provides a `FailT` monad transformer.

The example above used the `Fail` type synonym, whcih is restricts the underlying monad to
`Identity`. Below is the example of running `FailT` with `IO`:

```haskell
λ> import Control.Monad.IO.Class
λ> runFailT (liftIO . print . utctDayTime =<< formatParseM iso8601Format "2023-01-08T00:29:00Z")
1740s
Right ()
λ> runFailT (liftIO . print . utctDayTime =<< formatParseM iso8601Format "Bad input")
Left "no parse of \"Bad input\""
```

### Polymorphic failure

Thus far examples only showed using `String` type for failure messages, but that is not a
requirement. This library was designed to be agnostic with respect to failure message type
with restriction to `IsString` type class. Reason for the constraint is because the
failure message normally originates with the `fail` function and is string like by its
nature:

```haskell
fail :: MonadFail m => String -> m ()
```

The more general implementation is located in `import Control.Monad.Trans.Fail`, which
contains polymorphic implementation that allows the user to choose a more specific type
for the failure type `e`:

```
runFailT :: (IsString e, Semigroup e, Functor m) => FailT e m a -> m (Either e a)
```

This package implements convenience modules:

* `Control.Monad.Trans.Fail.String`
* `Control.Monad.Trans.Fail.Text`

which provide type synonyms and functions with more restricted failure types: `String` and
`Text` respectfully. Modules were designed to be drop-in replacements of each other.

### Convenient instances

There are many type class instances for `FailT` monad. Such as instances for type classes
from `mtl` package, `MonadIO` instance, etc. Most of them rely on the instances of the
underlying monad by lifting the functionality. However, here are some of the more notable
and useful instances that do not require corresponding instances from the underlying
monad:

* `MonadFail` with `Monad`ic sequencing, which allow to stop the computation upon the first
  invocation of `fail`.

  ```haskell
  λ> runFailT (fail "Failure!?" >> pure "Success!!")
  Left "Failure!?"
  ```

* `Alternative`, which will continue until the first successful computation is encountered.

  ```haskell
  λ> runFailT (fail "Failure!?" <|> pure "Success!!")
  Right "Success!!"
  ```

* `Semigroup` and `Monoid`, which will not stop until **all** of the actions are executed.
  Produced at the end are either all of the failures or the results of all of the
  successful cases combined with `<>`

  ```haskell
  λ> runFailT (pure ["Success!!"] <> fail "Failure!?" <> pure ["At", "Last!"])
  Right ["Success!!","At","Last!"]
  λ> runFailT mempty :: IO (Either String ())
  Left "No failure reason given"
  ```
