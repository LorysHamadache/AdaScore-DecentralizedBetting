{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_small_steps (
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
bindir     = "/home/lorys/.cabal/store/ghc-8.10.7/small-steps-0.1.0.0-e8b4f374a24b7773d6f701620c56f3e631d94d87ee3876b5f5a35e8b38a28d23/bin"
libdir     = "/home/lorys/.cabal/store/ghc-8.10.7/small-steps-0.1.0.0-e8b4f374a24b7773d6f701620c56f3e631d94d87ee3876b5f5a35e8b38a28d23/lib"
dynlibdir  = "/home/lorys/.cabal/store/ghc-8.10.7/small-steps-0.1.0.0-e8b4f374a24b7773d6f701620c56f3e631d94d87ee3876b5f5a35e8b38a28d23/lib"
datadir    = "/home/lorys/.cabal/store/ghc-8.10.7/small-steps-0.1.0.0-e8b4f374a24b7773d6f701620c56f3e631d94d87ee3876b5f5a35e8b38a28d23/share"
libexecdir = "/home/lorys/.cabal/store/ghc-8.10.7/small-steps-0.1.0.0-e8b4f374a24b7773d6f701620c56f3e631d94d87ee3876b5f5a35e8b38a28d23/libexec"
sysconfdir = "/home/lorys/.cabal/store/ghc-8.10.7/small-steps-0.1.0.0-e8b4f374a24b7773d6f701620c56f3e631d94d87ee3876b5f5a35e8b38a28d23/etc"

getBinDir     = catchIO (getEnv "small_steps_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "small_steps_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "small_steps_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "small_steps_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "small_steps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "small_steps_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
