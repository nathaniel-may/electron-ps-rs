# Script Modules

The code used to build, deploy, and test a project are just as long-lived and mission critical as the application code itself. Long lines of bash in `package.json` and haphazard Makefiles do not instill the confidence necessary for shipping production-grade artifacts. This directory contains well-tested Haskell modules for working with project files.

## Why Haskell?

This code is nearly entirely system calls and side effects. Using mtl-style Haskell offers a convenient way to write unit tests while abstracting over those effects.

## Usage

In the parent directory, call `./scripts.hs`.
