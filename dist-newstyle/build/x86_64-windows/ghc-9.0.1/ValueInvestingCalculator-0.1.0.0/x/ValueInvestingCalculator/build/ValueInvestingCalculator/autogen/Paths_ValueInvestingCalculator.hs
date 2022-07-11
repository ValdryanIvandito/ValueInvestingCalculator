{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ValueInvestingCalculator (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\valdryan.ivandito\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\valdryan.ivandito\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\ValueInvestingCalculator-0.1.0.0-inplace-ValueInvestingCalculator"
dynlibdir  = "C:\\Users\\valdryan.ivandito\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\valdryan.ivandito\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\ValueInvestingCalculator-0.1.0.0"
libexecdir = "C:\\Users\\valdryan.ivandito\\AppData\\Roaming\\cabal\\ValueInvestingCalculator-0.1.0.0-inplace-ValueInvestingCalculator\\x86_64-windows-ghc-9.0.1\\ValueInvestingCalculator-0.1.0.0"
sysconfdir = "C:\\Users\\valdryan.ivandito\\AppData\\Roaming\\cabal\\etc"

getBinDir     = catchIO (getEnv "ValueInvestingCalculator_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ValueInvestingCalculator_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ValueInvestingCalculator_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ValueInvestingCalculator_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ValueInvestingCalculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ValueInvestingCalculator_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
