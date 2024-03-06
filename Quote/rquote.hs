#!/usr/bin/env stack
-- stack --resolver lts-21.3 script
{-# LANGUAGE NoImplicitPrelude #-}

import RIO

import System.Environment (getArgs)

import qualified RIO.Text as T
import qualified RIO.List as L


data UsageException = UsageException deriving Show

instance Exception UsageException

main :: IO ()
main = runSimpleApp $ do
    args <- liftIO getArgs
    filename <- case L.headMaybe args of
        Nothing -> throwIO UsageException
        Just x -> pure x
    contents <- readFileUtf8 filename
    logInfo $ display (quote contents)

quote :: Text -> Text
quote = T.unlines . fmap tshow . T.lines
