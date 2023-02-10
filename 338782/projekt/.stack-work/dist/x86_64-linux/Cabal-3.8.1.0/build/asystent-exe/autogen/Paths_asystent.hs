{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_asystent (
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
bindir     = "/home/marek/prog/FP2022/338782/projekt/.stack-work/install/x86_64-linux/0ef0ce5e46256bd7c7e39b37d8c7cb5f590afab72e30469b1370f6d24088e886/9.4.4/bin"
libdir     = "/home/marek/prog/FP2022/338782/projekt/.stack-work/install/x86_64-linux/0ef0ce5e46256bd7c7e39b37d8c7cb5f590afab72e30469b1370f6d24088e886/9.4.4/lib/x86_64-linux-ghc-9.4.4/asystent-0.1.0.0-KesBVHfoSvjJUNl2TQLJ2V-asystent-exe"
dynlibdir  = "/home/marek/prog/FP2022/338782/projekt/.stack-work/install/x86_64-linux/0ef0ce5e46256bd7c7e39b37d8c7cb5f590afab72e30469b1370f6d24088e886/9.4.4/lib/x86_64-linux-ghc-9.4.4"
datadir    = "/home/marek/prog/FP2022/338782/projekt/.stack-work/install/x86_64-linux/0ef0ce5e46256bd7c7e39b37d8c7cb5f590afab72e30469b1370f6d24088e886/9.4.4/share/x86_64-linux-ghc-9.4.4/asystent-0.1.0.0"
libexecdir = "/home/marek/prog/FP2022/338782/projekt/.stack-work/install/x86_64-linux/0ef0ce5e46256bd7c7e39b37d8c7cb5f590afab72e30469b1370f6d24088e886/9.4.4/libexec/x86_64-linux-ghc-9.4.4/asystent-0.1.0.0"
sysconfdir = "/home/marek/prog/FP2022/338782/projekt/.stack-work/install/x86_64-linux/0ef0ce5e46256bd7c7e39b37d8c7cb5f590afab72e30469b1370f6d24088e886/9.4.4/etc"

getBinDir     = catchIO (getEnv "asystent_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "asystent_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "asystent_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "asystent_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "asystent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "asystent_sysconfdir") (\_ -> return sysconfdir)




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
