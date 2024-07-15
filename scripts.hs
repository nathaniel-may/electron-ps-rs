#!/usr/bin/env stack
-- stack --no-nix --resolver lts-21.24 --no-install-ghc --system-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception (..))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import Turtle

data Command = Run | RunRelease | RunNoBuild | Build | BuildRelease | Test | Fmt | Install | Clean deriving (Read, Show, Eq)

newtype ScriptException = ScriptException ByteString deriving (Show)

instance Exception ScriptException

newtype TestingException = TestingException Text deriving (Show)

instance Exception TestingException

main :: IO ()
main = (sh . run) =<< customExecParser (prefs showHelpOnEmpty) parser

run :: Command -> Shell ()
run cmd = case cmd of
  Run -> run Build >> run RunNoBuild
  RunRelease -> run BuildRelease >> run RunNoBuild
  RunNoBuild -> procs_ "npx" ["electron", "./src/main.js"]
  Build -> sh build
  BuildRelease -> sh buildRelease
  Test -> procs_ "cargo" ["test"]
  Fmt -> do
    procs_ "cargo" ["fmt"]
    procs_ "purs-tidy" ["format-in-place", "src/**/*.purs"]
  Install -> view $ procs_ "npm" ["install"]
  Clean -> traverse_ (\dir -> procs_ "rm" ["-rf", T.pack dir]) builtPaths

buildArgs :: [Text] -> Shell ()
buildArgs args = do
  output "cargo.log" $ inproc_ "cargo" (["build", "--message-format=json"] <> args)
  procs "npx" ["neon", "dist"] (inproc_ "cat" ["cargo.log"])
  rm "cargo.log"
  -- spago can only find a local esbuild installation if it is run via npm
  procs_ "npm" ["run", "spago-bundle-app"]
  procs_ "npx" ["tailwindcss", "-c", "tailwind.config.js", "-i", "./src/style.css", "-o", "./dist/style.css"]

build :: Shell ()
build = do
  run Install
  buildArgs []

buildRelease :: Shell ()
buildRelease = do
  run Install
  run Test
  buildArgs ["--release"]

-- leading "./" and lack of trailing "/" is necessary to match output of find
builtPaths :: [FilePath]
builtPaths =
  [ "./target",
    "./node_modules",
    "./index.node",
    "./.stack-work",
    "./cargo.log",
    "./dist"
  ]

parser :: ParserInfo Command
parser =
  info
    (subcommands <**> helper)
    (fullDesc <> progDesc "Tools for developing and deploying your application")
  where
    subcommands :: Parser Command
    subcommands =
      subparser
        ( command "run" (info (pure Run) (progDesc "run the electron app with a dev build"))
            <> command "run-release" (info (pure RunRelease) (progDesc "run the electron app with a production build"))
            <> command "run-no-build" (info (pure RunRelease) (progDesc "run the electron app with a production build"))
            <> command "build" (info (pure Build) (progDesc "build without optimizations"))
            <> command "build-release" (info (pure BuildRelease) (progDesc "build with optimiations for release"))
            <> command "test" (info (pure Test) (progDesc "run all app tests"))
            <> command "fmt" (info (pure Fmt) (progDesc "format source files in place"))
            <> command "install" (info (pure Install) (progDesc "install build dependencies"))
            <> command "clean" (info (pure Clean) (progDesc "delete all build files"))
        )

procs_ :: (MonadIO io) => Text -> [Text] -> io ()
procs_ cmd args = procs cmd args empty

proc_ :: (MonadIO io) => Text -> [Text] -> io ExitCode
proc_ cmd args = proc cmd args empty

inproc_ :: Text -> [Text] -> Shell Line
inproc_ cmd args = inproc cmd args empty
