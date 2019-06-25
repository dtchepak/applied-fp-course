
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype AppM a = AppM {
    runAppM :: ExceptT Error IO a
  } deriving (
        Functor,
        Applicative,
        Monad,
        MonadIO,
        MonadError Error)
```

Good, safe way to build Haskell apps. Almost always good enough. Efficient, performant. All newtypes, so they disappear at compile time. Works well with the rest of the ecosystem. Can get into crazy effects tracking and everything if you like (polysemy?), but very rarely necessary.
