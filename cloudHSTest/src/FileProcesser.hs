module FileProcesser (filterFiles) where

import Data.Traversable (traverse)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)

filterFiles :: FilePath -> IO [FilePath]
filterFiles fp = do
    files <- getDirectoryContents fp
    let fileList = filter filePred files
        dirList  = filter dirPred  files
    return $ fileList ++ (map (\path -> filterFiles path) dirList)
    where
     filePred fp = case takeExtension fp of
                    ".hs" -> True
                    _     -> False
     dirPred  fp = case takeExtension fp of
                    "" -> True
                    _  -> False
