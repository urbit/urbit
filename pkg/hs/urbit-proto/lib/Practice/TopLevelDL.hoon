module Practice.TopLevelDL where

data Err
  = ErrRead Text
  | ErrOpen Text
  | ErrFree [Term]
  | ErrType ([Act], Fail)

instance Rolling Err where
  roll = \case
    ErrRead t -> roll t
    ErrOpen t -> roll t
    ErrFree t -> leaf $ "free variables: " <> tshow t
    ErrType t -> roll t

ride = (cst, cod, val, typ)
 where
  cst = first ErrRead $ parse vest baseName txt
  cod = do
    c <- cst
    o <- first ErrOpen $ open c
    maybe (Left $ ErrFree $ F.toList o) Right $ closed o
  val = eval absurd <$> cod
  typ = do
    c <- cod
    first ErrType $ runReaderT (play (Con absurd absurd absurd) c) []
