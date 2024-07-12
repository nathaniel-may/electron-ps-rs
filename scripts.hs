#!/usr/bin/env stack
-- stack --resolver lts-21.24 --no-install-ghc --system-ghc runghc --package turtle --package async

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, mapConcurrently, withAsync)
import Control.Exception (Exception(..), throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Console.ANSI
import System.Exit (ExitCode(..))
import System.FilePath (replaceExtension)
import qualified System.IO as SYS
import Turtle

data App = Run Command | TestScripts deriving (Read, Show, Eq)
data Command = Build | BuildRelease | Test | Fmt | Install | Clean deriving (Read, Show, Eq)

newtype ScriptException = ScriptException ByteString deriving (Show)
instance Exception ScriptException

newtype TestingException = TestingException Text deriving (Show)
instance Exception TestingException

main :: IO ()
main = do
    app <- customExecParser (prefs showHelpOnEmpty) parser
    run app

run :: MonadIO m => App -> m ()
run = liftIO . \case
    TestScripts -> do
        passed <- mapConcurrently id tests
        when (and passed) (boldColorPrint Green "All tests passed.")
    Run cmd -> case cmd of
        Build -> sh build
        BuildRelease -> sh buildRelease
        Test -> procs "cargo" ["test"] empty
        Fmt -> procs "cargo" ["fmt"] empty
        Install -> view $ procs "npm" [ "install" ] empty
        Clean -> traverse_ (\dir -> procs "rm" [ "-rf", T.pack dir ] empty ) builtPaths

build :: Shell ()
build = do
    run (Run Install)
    procs "cargo" ["build"] empty

-- optimizations for serving like minifying source, generating the initial html, and converting images to webp
buildRelease :: Shell ()
buildRelease = do
    run (Run Install)
    procs "cargo" ["build", "--release"] empty

-- leading "./" and lack of trailing "/" is necessary to match output of find
builtPaths :: [FilePath]
builtPaths =
    [ "./target"
    , "./node_modules"
    , "./index.node"
    , "./.stack-work"
    ]

parser :: ParserInfo App
parser = info
    (subcommands <**> helper)
    (fullDesc <> progDesc "Tools for developing and deploying your application")
    where
    subcommands :: Parser App
    subcommands = subparser
        (  command "build"         (info (pure $ Run Build)        ( progDesc "build without optimizations" ))
        <> command "build-release" (info (pure $ Run BuildRelease) ( progDesc "build with optimiations for release" ))
        <> command "test"          (info (pure $ Run Test)         ( progDesc "run all app tests" ))
        <> command "fmt"           (info (pure $ Run Fmt)          ( progDesc "format source files in place" ))
        <> command "install"       (info (pure $ Run Install)      ( progDesc "install build dependencies" ))
        <> command "clean"         (info (pure $ Run Clean)        ( progDesc "delete all build files" ))
        <> command "test-scripts"  (info (pure TestScripts)        ( progDesc "test all the scripts in this file" )) )

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

for_  :: Functor f => f a -> (a -> b) -> f ()
for_ x f = void $ fmap f x

boldColorPrint :: MonadIO m => Color -> Text -> m ()
boldColorPrint color msg = liftIO $ do
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
    else boldColorPrint Red "‼️ TEST FAILED ‼️" >> throwIO (TestingException msg)
    pure result

-- todo write meaningful tests
tests :: [IO Bool]
tests =
    [ test "build command parses"
        (handleParseResult $ execParserPure (prefs showHelpOnEmpty) parser [ "build" ] )
        (== Run Build)
        "expected './install.hs build' to parse as `Run Build`"
    ]