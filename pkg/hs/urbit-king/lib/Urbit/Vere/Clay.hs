{-|
    UNIX Filesystem Driver
-}

module Urbit.Vere.Clay
  ( clay
  , clay'
  )
where

import Urbit.Arvo
import Urbit.King.App
import Urbit.Prelude
import Urbit.Vere.Pier.Types

import Conduit
import RIO.Directory
import RIO.FilePath

import qualified Data.Conduit.Combinators as CC
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S


--------------------------------------------------------------------------------

data ClayDrv = ClayDrv
  { cdMountPoints :: TVar (Map Desk (Map FilePath Int))
  }

deskToPath :: Desk -> FilePath
deskToPath (Desk (Cord t)) = unpack t

-- | The hard coded mime type of every file.
textPlain :: Path
textPlain = Path [(MkKnot "text"), (MkKnot "plain")]

-- | Filter for dotfiles, tempfiles and backup files.
validClaySyncPath :: FilePath -> Bool
validClaySyncPath fp = hasPeriod && notTildeFile && notDotHash && notDoubleHash
  where
    fileName = takeFileName fp
    hasPeriod = elem '.' fileName
    notTildeFile = not $ "~" `isSuffixOf` fileName
    notDotHash = not $ ".#" `isPrefixOf` fileName
    notDoubleHash =
      not $ ("#" `isPrefixOf` fileName) && ("#" `isSuffixOf` fileName)

{-|
    Returns a list of the result of running a function on each valid
    file in the directory fp. Runnable in IO.
-}
foreachFileIn :: (MonadUnliftIO m)
              => FilePath -> (FilePath -> (ResourceT m) a) -> m [a]
foreachFileIn fp fun =
  runConduitRes $ (sourceDirectoryDeep False fp)
               .| filterC validClaySyncPath
               .| CC.mapM fun
               .| sinkList

{-|
    Note: Vere just reuses +mug, but since the actual hash function is
    an implementation detail which doesn't leave the io driver, we just
    use the standard hash.
-}
getHashOfFile :: (MonadIO m) => FilePath -> m (FilePath, Int)
getHashOfFile fp = do
  bs <- readFile fp
  let !h = hash bs
  pure (fp, h)

{-|
   Takes an initial snapshot of the filesystem, recording what files exist and
   what their hashes are.
-}
takeFilesystemSnapshot :: FilePath -> RIO e (Map FilePath Int)
takeFilesystemSnapshot fp = do
  exists <- doesDirectoryExist fp
  if not exists then
    pure M.empty
  else
    M.fromList <$> foreachFileIn fp getHashOfFile

{-|
    Check an existing filepath against a snapshot of files that existed on disk
    the last time we checked. Returns Either (unchanged) (new file data).
-}
checkFileForUpdates :: (MonadIO m)
                    => Map FilePath Int -> FilePath
                    -> m (Either FilePath (FilePath, Mime, Int))
checkFileForUpdates snapshot fp = do
  bs <- readFile fp
  let !newHash = hash bs
  pure $ case lookup fp snapshot of
    -- text/plain is the hardcoded mime type of every file sent to clay.
    Nothing -> Right (fp, (Mime textPlain (File (Octs bs))), newHash)
    Just i -> if i == newHash then Left fp
              else Right (fp, (Mime textPlain (File (Octs bs))), newHash)

{-|
    Given a previous snapshot of the filesystem, produces a list of changes
-}
buildActionListFromDifferences :: FilePath -> Map FilePath Int
                               -> RIO e [(FilePath, Maybe (Mime, Int))]
buildActionListFromDifferences fp snapshot = do
  checks <- foreachFileIn fp (checkFileForUpdates snapshot)

  let changedItems = rights checks <&> \(fp, m, i) -> (fp, Just (m, i))

  let existsSet = S.fromList $ flip map checks $ \case
        Left fp          -> fp
        Right (fp, _, _) -> fp
  let deletedSet = S.difference (M.keysSet snapshot) existsSet
  let deletedItems = (toList deletedSet) <&> \x -> (x, Nothing)

  pure $ sort (deletedItems ++ changedItems)

--------------------------------------------------------------------------------

_boatFailed :: e -> WorkError -> IO ()
_boatFailed env _ = runRIO env $ do
  pure () -- TODO What can we do?

clay'
  :: HasPierEnv e
  => RIO e ([Ev], RAcquire e (DriverApi SyncEf))
clay' = do
  ventQ :: TQueue EvErr <- newTQueueIO
  env <- ask

  let (bornEvs, startDriver) = clay env (writeTQueue ventQ)

  let runDriver = do
        diOnEffect <- startDriver
        let diEventSource = fmap RRWork <$> tryReadTQueue ventQ
        pure (DriverApi {..})

  pure (bornEvs, runDriver)

clay
  :: forall e
   . (HasPierConfig e, HasLogFunc e, HasKingId e)
  => e
  -> (EvErr -> STM ())
  -> ([Ev], RAcquire e (SyncEf -> IO ()))
clay env plan =
    (initialEvents, runSync)
  where
    king = fromIntegral (env ^. kingIdL)

    boatEv = EvBlip $ BlipEvBoat $ BoatEvBoat () ()

    -- TODO: In the case of -A, we need to read all the data from the
    -- specified directory and shove it into an %into event.
    initialEvents = [boatEv]

    runSync :: RAcquire e (SyncEf -> IO ())
    runSync = handleEffect <$> mkRAcquire start stop

    start :: RIO e ClayDrv
    start = ClayDrv <$> newTVarIO mempty
    stop c = pure ()

    handleEffect :: ClayDrv -> SyncEf -> IO ()
    handleEffect cd = runRIO env . \case
      SyncEfHill _ mountPoints -> do
        logInfo $ displayShow ("(clay) known mount points:", mountPoints)
        pierPath <- view pierPathL
        mountPairs <- flip mapM mountPoints $ \desk -> do
          ss <- takeFilesystemSnapshot (pierPath </> (deskToPath desk))
          pure (desk, ss)
        atomically $ writeTVar (cdMountPoints cd) (M.fromList mountPairs)

      SyncEfDirk p desk -> do
        logInfo $ displayShow ("(clay) dirk:", p, desk)
        m <- atomically $ readTVar (cdMountPoints cd)
        let snapshot = M.findWithDefault M.empty desk m
        pierPath <- view pierPathL
        let dir = pierPath </> deskToPath desk
        actions <- buildActionListFromDifferences dir snapshot

        logInfo $ displayShow ("(clay) dirk actions: ", actions)

        let !intoList = map (actionsToInto dir) actions

        let syncEv = EvBlip
                   $ BlipEvSync
                   $ SyncEvInto (Some (king, ())) desk False intoList

        let syncFailed _ = pure ()

        atomically $ plan (EvErr syncEv syncFailed)


        atomically $ modifyTVar'
            (cdMountPoints cd)
            (applyActionsToMountPoints desk actions)

      SyncEfErgo p desk actions -> do
        logInfo $ displayShow ("(clay) ergo:", p, desk, actions)

        m <- atomically $ readTVar (cdMountPoints cd)
        let mountPoint = M.findWithDefault M.empty desk m

        pierPath <- view pierPathL
        let dir = pierPath </> deskToPath desk
        let hashedActions = map (calculateActionHash dir) actions
        for_ hashedActions (performAction mountPoint)

        atomically $ modifyTVar'
            (cdMountPoints cd)
            (applyActionsToMountPoints desk hashedActions)

      SyncEfOgre p desk -> do
        logInfo $ displayShow ("(clay) ogre:", p, desk)
        pierPath <- view pierPathL
        removeDirectoryRecursive $ pierPath </> deskToPath desk
        atomically $ modifyTVar' (cdMountPoints cd) (M.delete desk)


    -- Change the structures off of the event into something we can work with
    -- in Unix.
    calculateActionHash :: FilePath -> (Path, Maybe Mime)
                        -> (FilePath, Maybe (Mime, Int))
    calculateActionHash base (p, Nothing) = (base </> pathToFilePath p, Nothing)
    calculateActionHash base (p, Just (Mime t f)) =
      (base </> pathToFilePath p, Just ((Mime t f), (hash $ unOcts $ unFile f)))

    -- Performs the actions on the actual filesystem
    performAction :: (Map FilePath Int) -> (FilePath, Maybe (Mime, Int))
                  -> RIO e ()
    performAction m (fp, Nothing) = do
      logInfo $ displayShow ("(clay) deleting file ", fp)
      removeFile fp
    performAction m (fp, Just ((Mime _ (File (Octs bs)), hash)))
        | skip = logInfo $
                 displayShow ("(clay) skipping unchanged file update " , fp)
        | otherwise = do
            logInfo $ displayShow ("(clay) updating file " , fp)
            createDirectoryIfMissing True $ takeDirectory fp
            writeFile fp bs
      where
        skip = case M.lookup fp m of
          Nothing -> False
          Just i  -> i == hash

    -- Apply the actions to our internal snapshots
    applyActionsToMountPoints :: Desk
                              -> [(FilePath, Maybe (Mime, Int))]
                              -> (Map Desk (Map FilePath Int))
                              -> (Map Desk (Map FilePath Int))
    applyActionsToMountPoints desk actions m = M.alter change desk m
      where
        change (Just fileMap) = Just (foldl' applySyncAction fileMap actions)
        change Nothing        = change (Just M.empty)

        -- Applies the sync mutations specified.
        applySyncAction :: (Map FilePath Int)
                        -> (FilePath, Maybe (Mime, Int))
                        -> (Map FilePath Int)
        applySyncAction m (fp, Nothing)       = M.delete fp m
        applySyncAction m (fp, (Just (_, h))) = M.insert fp h m

    -- Changes an action list item into a form injectable into Urbit
    actionsToInto :: FilePath -> (FilePath, Maybe (Mime, Int))
                  -> (Path, Maybe Mime)
    actionsToInto prefix (fp, mybData) = (p, mybOutData)
      where
        p = filePathToPath strippedFp
        strippedFp = case stripPrefix prefix fp of
          Nothing -> error "Impossible missing prefix"
          Just x  -> x
        mybOutData = case mybData of
          Nothing     -> Nothing
          Just (m, i) -> Just m
