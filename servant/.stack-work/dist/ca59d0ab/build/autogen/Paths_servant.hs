{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_servant (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Rory\\Desktop\\Haskell\\Restful Cyclo\\servant\\.stack-work\\install\\3573363d\\bin"
libdir     = "C:\\Users\\Rory\\Desktop\\Haskell\\Restful Cyclo\\servant\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2\\servant-0.1.0.0-JLcZKfTrAn97houkqDLF44"
dynlibdir  = "C:\\Users\\Rory\\Desktop\\Haskell\\Restful Cyclo\\servant\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Rory\\Desktop\\Haskell\\Restful Cyclo\\servant\\.stack-work\\install\\3573363d\\share\\x86_64-windows-ghc-8.0.2\\servant-0.1.0.0"
libexecdir = "C:\\Users\\Rory\\Desktop\\Haskell\\Restful Cyclo\\servant\\.stack-work\\install\\3573363d\\libexec"
sysconfdir = "C:\\Users\\Rory\\Desktop\\Haskell\\Restful Cyclo\\servant\\.stack-work\\install\\3573363d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "servant_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "servant_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "servant_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "servant_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servant_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servant_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
