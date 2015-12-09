module Main where

import System.Environment (getArgs)
import Filesystem (listDirectory)
import qualified Filesystem.Path.CurrentOS as FP

import Odf'xsd (elementMetaData, makeInfoItemMap, parseFile)
import Socket ()

import qualified Data.Text.Strict as T

recursiveLs :: FilePath -> IO [FilePath]
recursiveLs root = do
   children <- listDirectory root
   return $ root : recursiveLs children

main = do
    args <- getArgs
    case args of
        [metaRootStr, devicePath, arduinoserv, sensorbox] -> do
            let metaRoot = FP.fromText . T.pack metaRootStr

            metaFilePaths <- recursiveLs metaRoot
            -- let metaPaths = map (stripPrefix $ parent metaRoot) metaFilePaths

            metaDatas <- forM metaFilePaths $ \ fpath -> do
                (MetaDataType metadata) <- parseFile fpath elmentMetaData
                return (toText $ stripPrefix $ parent metaRoot, makeInfoItemMap metadata)

            let arduinoservTargets = Map.fromList $ filter (isPrefixOf devicePath . fst) metaDatas 

        _ -> putStrLn "Usage: <path_to_metadata_hierarchy> <arduino_path_relative_to_root> <arduinoserv_url> <output_url>"




