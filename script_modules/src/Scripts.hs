{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Scripts (run) where 

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, mapConcurrently, withAsync)
import Control.Exception (Exception(..), throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Console.ANSI
import System.Exit (ExitCode(..))
import System.FilePath (replaceExtension)
import qualified System.IO as SYS
import qualified Turtle
import Scripts.Class (Concurrent, ShellCommands, Std, cargo, npm, rm)
import Scripts.Utils (for, for_)

newtype App a = App (Turtle.Shell a) deriving (Functor, Applicative, Monad, MonadIO)
data Command = Build | BuildRelease | Test | Fmt | Install | Clean | TestScripts deriving (Read, Show, Eq)

instance ShellCommands App where
    cargo args = liftIO $ Turtle.procs "cargo" args empty
    rm args = liftIO $ Turtle.procs "rm" args empty
    npm args = liftIO . Turtle.view $ Turtle.procs "cargo" args empty

newtype ScriptException = ScriptException ByteString deriving (Show)
instance Exception ScriptException

newtype TestingException = TestingException Text deriving (Show)
instance Exception TestingException

-- exported entry-point to the scripts application
run :: IO ()
run = do
    cmd <- customExecParser (prefs showHelpOnEmpty) parser
    runApp (App $ pure cmd)

runApp :: (MonadIO m, ShellCommands m, Std m) => App Command -> m ()
runApp (App shell) = do
    cmd <- shell
    case cmd of
        Build -> build
        BuildRelease -> buildRelease
        Test -> cargo ["test"]
        Fmt -> cargo ["fmt"]
        Install -> npm [ "install" ]
        Clean -> traverse_ (\dir -> rm [ "-rf", T.pack dir ] ) builtPaths

build :: (ShellCommands m, Std m) => m ()
build = do
    runApp $ pure Install
    cargo ["build"]

buildRelease :: (ShellCommands m, Std m) => m ()
buildRelease = do
    runApp $ pure Install
    cargo ["build", "--release"]

-- leading "./" and lack of trailing "/" is necessary to match output of find
builtPaths :: [FilePath]
builtPaths =
    [ "./target"
    , "./node_modules"
    , "./index.node"
    , "./.stack-work"
    ]

parser :: ParserInfo Command
parser = info
    (subcommands <**> helper)
    (fullDesc <> progDesc "Tools for developing and deploying your application")
    where
    subcommands :: Parser Command
    subcommands = subparser
        (  command "build"         (info (pure Build)        (progDesc "build without optimizations" ))
        <> command "build-release" (info (pure BuildRelease) (progDesc "build with optimiations for release" ))
        <> command "test"          (info (pure Test)         (progDesc "run all app tests" ))
        <> command "fmt"           (info (pure Fmt)          (progDesc "format source files in place" ))
        <> command "install"       (info (pure Install)      (progDesc "install build dependencies" ))
        <> command "clean"         (info (pure Clean)        (progDesc "delete all build files" ))
        <> command "test-scripts"  (info (pure TestScripts)  (progDesc "test all the scripts in this file" )))

printWithBoldColor :: MonadIO m => Color -> Text -> m ()
printWithBoldColor color msg = liftIO $ do
    stdoutSupportsANSI <- hSupportsANSI SYS.stdout
    if stdoutSupportsANSI
    then do
        setSGR  [ SetConsoleIntensity BoldIntensity
                , SetColor Foreground Vivid color
                ]
        T.putStrLn msg
        setSGR [Reset]
    else T.putStrLn msg

test :: MonadIO m => Text -> m a -> (a -> Bool) -> Text -> m Bool
test name actual fExpected msg = do
    result <- fExpected <$> actual
    let errOutput = do
            T.putStrLn "‼️ TEST FAILED ‼️"
            throwIO (TestingException msg)
    liftIO $ if result
    then T.putStrLn ("✅ " <> name <> "\n")
    else printWithBoldColor Red "‼️ TEST FAILED ‼️" >> throwIO (TestingException msg)
    pure result

-- todo write meaningful tests
tests :: [IO Bool]
tests =
    [ test "build command parses"
        (handleParseResult $ execParserPure (prefs showHelpOnEmpty) parser [ "build" ] )
        (== Build)
        "expected './install.hs build' to parse as `Run Build`"
    ]