{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cardano_crypto_wrapper (
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
version = Version [1,3,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/lorys/.cabal/store/ghc-8.10.7/cardano-crypto-wrapper-1.3.0-10908def7a6cd2b75ed3cfdf281314c88aef497ff824c53b395d2021c71ec6a4/bin"
libdir     = "/home/lorys/.cabal/store/ghc-8.10.7/cardano-crypto-wrapper-1.3.0-10908def7a6cd2b75ed3cfdf281314c88aef497ff824c53b395d2021c71ec6a4/lib"
dynlibdir  = "/home/lorys/.cabal/store/ghc-8.10.7/cardano-crypto-wrapper-1.3.0-10908def7a6cd2b75ed3cfdf281314c88aef497ff824c53b395d2021c71ec6a4/lib"
datadir    = "/home/lorys/.cabal/store/ghc-8.10.7/cardano-crypto-wrapper-1.3.0-10908def7a6cd2b75ed3cfdf281314c88aef497ff824c53b395d2021c71ec6a4/share"
libexecdir = "/home/lorys/.cabal/store/ghc-8.10.7/cardano-crypto-wrapper-1.3.0-10908def7a6cd2b75ed3cfdf281314c88aef497ff824c53b395d2021c71ec6a4/libexec"
sysconfdir = "/home/lorys/.cabal/store/ghc-8.10.7/cardano-crypto-wrapper-1.3.0-10908def7a6cd2b75ed3cfdf281314c88aef497ff824c53b395d2021c71ec6a4/etc"

getBinDir     = catchIO (getEnv "cardano_crypto_wrapper_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cardano_crypto_wrapper_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cardano_crypto_wrapper_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cardano_crypto_wrapper_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cardano_crypto_wrapper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cardano_crypto_wrapper_sysconfdir") (\_ -> return sysconfdir)




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
