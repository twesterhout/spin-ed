import Control.Monad (unless)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
  ( InstallDirs (..),
    LocalBuildInfo (..),
    absoluteInstallDirs,
    localPkgDescr,
  )
import Distribution.Simple.Setup
import Distribution.Simple.Utils
  ( installMaybeExecutableFile,
    notice,
    rawSystemExit,
  )
import Distribution.System
import qualified Distribution.Verbosity as Verbosity
import System.Directory (getCurrentDirectory)

main = defaultMainWithHooks hooks
  where
    hooks =
      simpleUserHooks
        { preConf = buildLibLatticeSymmetries,
          confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs f,
          postCopy = copyLibLatticeSymmetries,
          postClean = cleanLibLatticeSymmetries
        }

buildLibLatticeSymmetries :: Args -> ConfigFlags -> IO HookedBuildInfo
buildLibLatticeSymmetries _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
      buildShared = getCabalFlag "shared" flags
  notice verbosity "Building liblattice_symmetries.a C library..."
  rawSystemExit verbosity "bash" $
    ["build_lattice_symmetries.sh"] <> (if buildShared then ["--shared"] else [])
  return emptyHookedBuildInfo

updateExtraLibDirs :: ConfigFlags -> LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs flags localBuildInfo = do
  dir <- getCurrentDirectory
  let lib' =
        lib
          { libBuildInfo =
              libBuild
                { extraLibDirs = (dir <> "/third_party/lattice_symmetries/lib") : extraLibDirs libBuild,
                  includeDirs = (dir <> "/third_party/lattice_symmetries/include") : includeDirs libBuild
                }
          }
  return localBuildInfo {localPkgDescr = packageDescription {library = Just $ lib'}}
  where
    packageDescription = localPkgDescr localBuildInfo
    lib = case library packageDescription of
      Just x -> x
      Nothing -> error "this should not have happened; did you remove the library target?"
    libBuild = libBuildInfo lib

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib flags _ libPref =
  do
    notice verbosity $ "Installing lattice_symmetries C library..."
    installMaybeExecutableFile verbosity ("third_party/lattice_symmetries/lib" <> libName) (libPref <> libName)
  where
    verbosity = fromFlag $ configVerbosity flags
    libName = case getCabalFlag "shared" flags of
      True -> "/liblattice_symmetries.so"
      False -> "/liblattice_symmetries.a"

copyLibLatticeSymmetries :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibLatticeSymmetries _ flags packageDescription localBuildInfo = copyLib config localBuildInfo libPref
  where
    libPref = libdir . absoluteInstallDirs packageDescription localBuildInfo . fromFlag . copyDest $ flags
    config = configFlags localBuildInfo

cleanLibLatticeSymmetries :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibLatticeSymmetries _ flags _ _ = rawSystemExit verbosity "./build_lattice_symmetries.sh" ["--clean"]
  where
    verbosity = fromFlag $ cleanVerbosity flags

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
  where
    allFlags = configConfigurationsFlags flags
    name' = map toLower name
