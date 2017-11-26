-- Data types for storing JSON objects in Haskell
module Commit(CommitStruct) where


data Commit = Commit{
	
}

data CommitStruct = CommitStruct{
  sha          :: String,
  commit       :: Commit,
  url          :: String,
  html_url     :: String,  
  comments_url :: String,
  author       :: Author,
  committer    :: Author,
  parents      :: Parent
}