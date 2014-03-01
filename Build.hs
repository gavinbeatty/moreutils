#!/usr/bin/env runhaskell
module Main (main) where
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (lookupDefault, HashMap)
import System.Directory
import Control.Monad (when)
import Control.Concurrent.Async (withAsync, waitCatch)
import qualified System.Info as Sys

getDefaultConfig :: String -> String -> Action String
getDefaultConfig key def = fromMaybe def <$> getConfig key

lookupBindir, lookupMan1dir :: Config -> String
lookupBindir = lookupDefault "/usr/local/bin" "bindir"
lookupMan1dir = lookupDefault "/usr/local/share/man/man1" "man1dir"

getCC, getDocbook2man, getPod2man :: Action String
getCC = getDefaultConfig "CC" "cc"
getDocbook2man = getDefaultConfig "docbook2man" "docbook2man"
getPod2man = getDefaultConfig "pod2man" "pod2man"

getCFlags :: Action [String]
getCFlags = do
  getDefaultConfig "CFLAGS" def >>= (return . read)
 where
  def = show $ words "-O2 -g -Wall"

main :: IO ()
main = readConfigFile configPath >>= build

type Config = HashMap String String

build :: Config -> IO ()
build conf = do
  let man1dir = lookupMan1dir conf
  let bindir = lookupBindir conf
  shakeArgs shakeOptions $ do
  usingConfig conf
  let installedMan1s = map (man1dir </>) (doc1s ++ pod1s)
  let installedApps = map (bindir </>) (apps ++ perls)
  want apps
  apps **> \out -> do
    let cfile = out <.> ".c"
    need [cfile]
    cc <- getCC
    cflags <- getCFlags
    command [] cc $ cflags ++ ["-o", out, cfile]
  doc1s **> \out -> do
    let docbook = out -<.> ".docbook"
    need [docbook]
    docbook2man <- getDocbook2man
    command [] docbook2man [docbook]
  pod1s **> \out -> do
    let perl = dropExtension out
    pod2man <- getPod2man
    Stdout output <- command [] pod2man ["--center= ","--release=moreutils",perl]
    writeFileChanged out output
  installedMan1s **> \out -> do
    let src = takeFileName out
    need [src]
    traced "" $ installData src out
  installedApps **> \out -> do
    let src = takeFileName out
    need [src]
    traced "" $ installBin src out
  phony "all" $ (need apps >> need doc1s >> need pod1s)
  phony "clean" $ do
    removeFilesAfter "." apps
    removeFilesAfter "." doc1s
    removeFilesAfter "." pod1s
    when (Sys.os == "darwin") . traced "rm dSYM" . mapM_ forceRemoveDirectoryRecursive $ map (++ ".dSYM") apps
  phony "man" $ (need doc1s >> need pod1s)
  phony "install" $ (need installedApps >> need installedMan1s)
  phony "install-bin" $ need installedApps
  phony "install-man" $ need installedMan1s

configPath :: FilePath
configPath = "Build.config"

osapps :: String -> [FilePath]
osapps os
  | os `elem` ["linux","freebsd","dragonfly"] = ["ifdata"]
  | otherwise = []

apps, doc1s, perls, pod1s :: [FilePath]
apps = words "ifne isutf8 lckdo mispipe parallel pee sponge" ++ osapps Sys.os
doc1s = map (<.> ".1") apps
perls = words "chronic combine ts vidir vipe zrun"
pod1s = map (<.> ".1") perls

modifyPermissions :: FilePath -> (Permissions -> Permissions) -> IO ()
modifyPermissions path f = getPermissions path >>= (return . f) >>= setPermissions path

installExecutable :: Bool -> FilePath -> FilePath -> IO ()
installExecutable b old new = copyFile old new >> modifyPermissions new (setOwnerExecutable b)

installBin, installData :: FilePath -> FilePath -> IO ()
installBin = installExecutable True
installData = installExecutable False

ignoreExcept :: IO a -> IO ()
ignoreExcept io = do
  _ <- withAsync io waitCatch
  return ()

forceRemoveDirectoryRecursive :: FilePath -> IO ()
forceRemoveDirectoryRecursive = ignoreExcept . removeDirectoryRecursive
