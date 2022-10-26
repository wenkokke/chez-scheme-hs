{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}

import Control.Monad                         (forM, unless)
import Data.Foldable                         (asum)
import Data.IORef                            (newIORef, writeIORef)
import Data.List                             (isPrefixOf, isSuffixOf)
import Data.Maybe                            (catMaybes, fromMaybe)
import Distribution.Compat.Semigroup         (Last' (..), Option' (..))
import Distribution.PackageDescription       (HookedBuildInfo,
                                              PackageDescription)
import Distribution.Pretty                   (prettyShow)
import Distribution.Simple                   (Args, UserHooks (..),
                                              defaultMainWithHooks,
                                              simpleUserHooks)
import Distribution.Simple.Compiler          (PackageDBStack)
import Distribution.Simple.Flag              (fromFlagOrDefault)
import Distribution.Simple.LocalBuildInfo    (LocalBuildInfo (..))
import Distribution.Simple.Program           (ConfiguredProgram (programLocation),
                                              ProgArg, Program (programName),
                                              ProgramDb,
                                              ProgramInvocation (progInvokeCwd, progInvokeEnv),
                                              ProgramLocation (locationPath),
                                              arProgram, configureProgram,
                                              emptyProgramDb, gccProgram,
                                              ldProgram, lookupProgram,
                                              programInvocation, requireProgram,
                                              runProgram, runProgramInvocation,
                                              simpleProgram)
import Distribution.Simple.Setup             (BuildFlags (buildVerbosity),
                                              ConfigFlags (configPackageDBs, configVerbosity),
                                              configPrograms)
import Distribution.Simple.Utils             (debug, die', dieNoVerbosity,
                                              notice, printRawCommandAndArgs,
                                              rawSystemExit, rawSystemIOWithEnv,
                                              rewriteFileEx, safeHead,
                                              setFileExecutable, warn,
                                              withFileContents)
import Distribution.System                   (Arch (..), OS (..),
                                              Platform (Platform))
import Distribution.Types.PackageDescription (PackageDescription (PackageDescription, extraSrcFiles))
import Distribution.Verbosity                (Verbosity)
import Distribution.Verbosity                qualified as Verbosity (normal)
import System.Exit                           (ExitCode (ExitSuccess), exitWith)
import System.FilePath                       (joinPath, makeRelative, splitPath,
                                              takeDirectory, (</>))
import System.IO                             (hFlush, stdout)
import Text.Printf                           (printf)

-- TODO: add extra-source-files
-- TODO: get paths from extra-source-files
-- TODO: copy boot files to ./include and ./boot

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms =
          [arProgram, gccProgram, ldProgram, makeProgram, shProgram] <> hookedPrograms simpleUserHooks,
        postConf = \args configFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          configureChezScheme verbosity packageDescription localBuildInfo
          postConf simpleUserHooks args configFlags packageDescription localBuildInfo,
        buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
          let verbosity = fromFlagOrDefault Verbosity.normal (buildVerbosity buildFlags)
          let LocalBuildInfo{withPrograms} = localBuildInfo
          buildChezScheme verbosity withPrograms
          buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
      }

-- postClean

makeProgram :: Program
makeProgram = simpleProgram "make"

shProgram :: Program
shProgram = simpleProgram "sh"

configureChezScheme :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
configureChezScheme verbosity packageDescription localBuildInfo  = do
  notice verbosity "Configuring ChezScheme"
  let LocalBuildInfo{buildDir, hostPlatform, withPrograms} = localBuildInfo
  let PackageDescription{extraSrcFiles} = packageDescription

  -- Find the ChezScheme root (defaults to "vendor/ChezScheme"):
  let chezSchemeRoot = findChezSchemeRoot extraSrcFiles

  -- Mark ./configure and other scripts to be executable:
  setFileExecutable $ chezSchemeRoot </> "configure"
  setFileExecutable $ chezSchemeRoot </> "workarea"
  setFileExecutable $ chezSchemeRoot </> "s" </> "update-revision"
  setFileExecutable $ chezSchemeRoot </> "zlib" </> "configure"
  setFileExecutable $ chezSchemeRoot </> "zlib" </> "configure"

  -- Determine the arguments to ./configure:
  let enableX11 = False
  let enableCurses = False
  let threaded = True
  machineType <- targetPlatformForChezScheme verbosity hostPlatform threaded
  let args =
        concat
          [ ["." </> "configure"],
            ["--machine=" <> machineType],
            ["--threads" | threaded],
            ["--disable-x11" | not enableX11],
            ["--disable-curses" | not enableCurses],
            ["--libkernel"],
            ["--workarea=" <> machineType]
          ]
  let arLocation = locationPath . programLocation <$> lookupProgram arProgram withPrograms
  let ccLocation = locationPath . programLocation <$> lookupProgram gccProgram withPrograms
  let ldLocation = locationPath . programLocation <$> lookupProgram ldProgram withPrograms
  let shLocation = locationPath . programLocation <$> lookupProgram shProgram withPrograms

  -- Run ./configure:
  shInvocation <- dbProgramInvocation verbosity shProgram withPrograms args
  runProgramInvocation verbosity shInvocation {
    progInvokeCwd = Just chezSchemeRoot,
    progInvokeEnv = progInvokeEnv shInvocation <> [
      ("AR", arLocation),
      ("CC", ccLocation),
      ("LD", ldLocation),
      ("SH", shLocation)
    ]
  }

findChezSchemeRoot :: [FilePath] -> FilePath
findChezSchemeRoot =
  joinPath
    . fromMaybe ["vendor", "ChezScheme"]
    . safeHead
    . filter (chezSchemeConfigureFile `isSuffixOf`)
    . map splitPath
  where
    chezSchemeConfigureFile :: [FilePath]
    chezSchemeConfigureFile = ["vendor", "ChezScheme", "configure"]

buildChezScheme :: Verbosity -> ProgramDb -> IO ()
buildChezScheme verbosity programDb = do
  notice verbosity "Building ChezScheme"
  let workingDirectory = "vendor" </> "ChezScheme"
  printRawCommandAndArgs verbosity "make" []
  makeInvocation <- dbProgramInvocation verbosity makeProgram programDb []
  runProgramInvocation verbosity makeInvocation {
    progInvokeCwd = Just workingDirectory
  }

targetPlatformForChezScheme :: Verbosity -> Platform -> Bool -> IO String
targetPlatformForChezScheme verbosity (Platform I386 Linux) False = return "i3le"
targetPlatformForChezScheme verbosity (Platform I386 Windows) False = return "i3nt"
targetPlatformForChezScheme verbosity (Platform I386 OSX) False = return "i3osx"
targetPlatformForChezScheme verbosity (Platform X86_64 Linux) False = return "a6le"
targetPlatformForChezScheme verbosity (Platform X86_64 Windows) False = return "a6nt"
targetPlatformForChezScheme verbosity (Platform X86_64 OSX) False = return "a6osx"
targetPlatformForChezScheme verbosity (Platform I386 Linux) True = return "ti3le"
targetPlatformForChezScheme verbosity (Platform I386 Windows) True = return "ti3nt"
targetPlatformForChezScheme verbosity (Platform I386 OSX) True = return "ti3osx"
targetPlatformForChezScheme verbosity (Platform X86_64 Linux) True = return "ta6le"
targetPlatformForChezScheme verbosity (Platform X86_64 Windows) True = return "ta6nt"
targetPlatformForChezScheme verbosity (Platform X86_64 OSX) True = return "ta6osx"
targetPlatformForChezScheme verbosity (Platform Arm Linux) False = return "arm32le"
targetPlatformForChezScheme verbosity (Platform arch os) threaded =
  die' verbosity $
    printf
      "Unsupported target platform %s-%s (%s)"
      (prettyShow arch)
      (prettyShow os)
      (if threaded then "threaded" else "nonthreaded")

-- From vendor/ChezScheme/configure --help:
--
-- Purpose:
--   ./configure determines the machine type and constructs a custom Makefile
--   and Mf-install, taking into account the options below.
--
-- Options (defaults shown in parens):
--   --machine=<machine type>          explicitly specify machine type (a6osx)
--   -m=<machine type>                 same as --machine <machine type> (a6osx)
--   --threads                         specify threaded version (no)
--   --32|--64                         specify 32/64-bit version (64)
--   --disable-x11                     disable X11 support
--   --disable-curses                  disable [n]curses support
--   --libkernel                       build libkernel.a instead of kernel.o
--   --kernelobj                       build kernel.o (the default)
--   --installprefix=<pathname>        final installation root (/usr/local)
--   --installbin=<pathname>           bin directory (/usr/local/bin)
--   --installlib=<pathname>           lib directory (/usr/local/lib)
--   --installman=<pathname>           manpage directory (/usr/local/share/man)
--   --installdoc=<pathname>           documentation root (/usr/local/share/doc)
--   --installcsug=<pathname>          guide directory (/usr/local/share/doc/csug9.5)
--   --installreleasenotes=<pathname>  notes directory (/usr/local/share/doc/csv9)
--   --temproot=<pathname>             staging root ()
--   --installowner=<ownername>        install with owner ()
--   --installgroup=<groupname>        install with group ()
--   --installschemename=<schemename>  install scheme as (scheme)
--   --installpetitename=<petitename>  install petite as (petite)
--   --installscriptname=<scriptname>  install scheme-script as (scheme-script)
--   --toolprefix=<prefix>             prefix tool (compiler, linker, ...) names
--   --[no]gzip-man-pages              compress manual pages (yes)
--   --workarea=<pathname>             build directory (a6osx)
--   CC=<C compiler>                   C compiler
--   CPPFLAGS=<C preprocessor flags>   additional C preprocessor flags ()
--   CFLAGS=<C compiler flags>         additional C compiler flags ()
--   LD=<linker>                       linker
--   LDFLAGS=<linker flags>            additional linker flags ()
--   AR=<archiver>                     archiver
--   ARFLAGS=<archiver flgs>           archiver flags
--   RANLIB=<archive indexer>          archive indexer
--   WINDRES=<resource compiler>       resource compiler
--   ZLIB=<lib>                        link to <lib> instead of own zlib
--   LZ4=<lib>                         link to <lib> instead of own LZ4
--
-- Available machine types: a6le, a6nt, a6osx, arm32le, i3le, i3nt, i3osx, ta6le, ta6nt, ta6osx, ti3le, ti3nt, and ti3osx
--
-- Examples:
--   ./configure --machine=i3le
--
--   set machine-type to i3le rather than to determined type
--
--   ./configure --threads --installprefix=/usr/local
--
--   specify threaded version and set installation directory to /usr/local.
--
--   ./configure --installprefix=/usr/local --temproot=/tmp
--
--   declare the final destination to be /usr/local but staging area
--   to be /tmp/usr/local.  Make will record the final destination in the
--   installed manual pages but actually install the system and manual
--   pages in the staging area.

-- Helper to run ProgramDb programs in a certain directory:

dbProgramInvocation :: Verbosity -> Program -> ProgramDb -> [ProgArg] -> IO ProgramInvocation
dbProgramInvocation verbosity prog progDb args =
  case lookupProgram prog progDb of
    Nothing             -> die' verbosity notFound
    Just configuredProg -> return $ programInvocation configuredProg args
  where
    notFound = printf "The program '%s' is required but it could not be found in %s" (programName prog) (show progDb)
