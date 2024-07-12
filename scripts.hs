#!/usr/bin/env stack
-- stack --no-nix --resolver lts-21.24 --no-install-ghc --system-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception(..))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Turtle


data Command = Build | BuildRelease | Test | Fmt | Install | Clean deriving (Read, Show, Eq)

newtype ScriptException = ScriptException ByteString deriving (Show)
instance Exception ScriptException

newtype TestingException = TestingException Text deriving (Show)
instance Exception TestingException

main :: IO ()
main = (sh . run) =<< customExecParser (prefs showHelpOnEmpty) parser

run :: Command -> Shell ()
run cmd = case cmd of
    Build -> sh build
    BuildRelease -> sh buildRelease
    Test -> procs "cargo" ["test"] empty
    Fmt -> procs "cargo" ["fmt"] empty
    Install -> view $ procs "npm" [ "install" ] empty
    Clean -> traverse_ (\dir -> procs "rm" [ "-rf", T.pack dir ] empty ) builtPaths

build :: Shell ()
build = do
    run Install
    procs "cargo" ["build"] empty

buildRelease :: Shell ()
buildRelease = do
    run Install
    run Test
    procs "cargo" ["build", "--release"] empty

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
        (  command "build"         (info (pure Build)        ( progDesc "build without optimizations" ))
        <> command "build-release" (info (pure BuildRelease) ( progDesc "build with optimiations for release" ))
        <> command "test"          (info (pure Test)         ( progDesc "run all app tests" ))
        <> command "fmt"           (info (pure Fmt)          ( progDesc "format source files in place" ))
        <> command "install"       (info (pure Install)      ( progDesc "install build dependencies" ))
        <> command "clean"         (info (pure Clean)        ( progDesc "delete all build files" )))
