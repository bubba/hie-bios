{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- | These functions are for conveniently implementing the simple CLI
module HIE.Bios.Ghc.Api (
    initializeFlagsWithCradle
  , initializeFlagsWithCradleWithMessage
  , G.SuccessFlag(..)
  -- * Utility functions for running the GHC monad and implementing internal utilities
  , withGHC
  , withGhcT
  , getGhcLibDir
  , withDynFlags
  ) where

import CoreMonad (liftIO)
import Exception (ghandle, SomeException(..), ExceptionMonad(..))
import GHC (Ghc, LoadHowMuch(..), GhcMonad, GhcT)
import DynFlags

import qualified GHC as G
import qualified MonadUtils as G
import qualified HscMain as G
import qualified GhcMake as G

import Control.Monad (void)
import System.Exit (exitSuccess)
import HIE.Bios.Types
import qualified HIE.Bios.Internal.Log as Log
import HIE.Bios.Environment
import HIE.Bios.Flags

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad. All exceptions are ignored and logged.
withGHC :: Cradle c  -- ^ The cradle to use for resolving the lib dir
        -> FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC cradle file body = do
    libDir <- getGhcLibDir cradle
    ghandle ignore (G.runGhc libDir body)
  where
    ignore :: SomeException -> IO a
    ignore e = do
        Log.logm $ file ++ ":0:0:Error:"
        Log.logm (show e)
        exitSuccess

withGhcT :: (Exception.ExceptionMonad m, G.MonadIO m, Monad m)
         => Cradle c -> GhcT m a -> m a
withGhcT cradle body = do
  libDir <- liftIO $ getGhcLibDir cradle
  G.runGhcT libDir body

----------------------------------------------------------------

-- | Initialize a GHC session by loading a given file into a given cradle.
initializeFlagsWithCradle ::
    GhcMonad m
    => FilePath -- ^ The file we are loading the 'Cradle' because of
    -> Cradle a   -- ^ The cradle we want to load
    -> m (CradleLoadResult (m G.SuccessFlag, ComponentOptions))
initializeFlagsWithCradle = initializeFlagsWithCradleWithMessage (Just G.batchMsg)

-- | The same as 'initializeFlagsWithCradle' but with an additional argument to control
-- how the loading progress messages are displayed to the user. In @haskell-ide-engine@
-- the module loading progress is displayed in the UI by using a progress notification.
initializeFlagsWithCradleWithMessage ::
  GhcMonad m
  => Maybe G.Messager
  -> FilePath -- ^ The file we are loading the 'Cradle' because of
  -> Cradle a  -- ^ The cradle we want to load
  -> m (CradleLoadResult (m G.SuccessFlag, ComponentOptions)) -- ^ Whether we actually loaded the cradle or not.
initializeFlagsWithCradleWithMessage msg fp cradle =
    fmap (initSessionWithMessage msg) <$> liftIO (getCompilerOptions fp cradle)

-- | Actually perform the initialisation of the session. Initialising the session corresponds to
-- parsing the command line flags, setting the targets for the session and then attempting to load
-- all the targets.
initSessionWithMessage :: (GhcMonad m)
            => Maybe G.Messager
            -> ComponentOptions
            -> (m G.SuccessFlag, ComponentOptions)
initSessionWithMessage msg compOpts = (do
    targets <- initSession compOpts
    G.setTargets targets
    -- Get the module graph using the function `getModuleGraph`
    mod_graph <- G.depanal [] True
    G.load' LoadAllTargets msg mod_graph, compOpts)

----------------------------------------------------------------

withDynFlags ::
  (GhcMonad m)
  => (DynFlags -> DynFlags) -> m a -> m a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------
