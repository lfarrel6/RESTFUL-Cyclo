-- Data types for storing JSON objects in Haskell
module Commit(CommitStruct) where

data CommitStruct = CommitStruct {
  sha          :: String,
  commit       :: Commit,
  url          :: String,
  html_url     :: String,  
  comments_url :: String,
  author       :: Author,
  committer    :: Author,
  parents      :: Parent
}

data Commit = Commit
{ author        :: UserData
, committer     :: UserData
, message       :: String
, tree          :: Tree
, url           :: String
, comment_count :: Int
, verification  :: Verification
}

data Author = Author {
  login               :: String,
  id                  :: Int,
  avatar_url          :: String,
  gravatar_url        :: String,
  url                 :: String,
  html_url            :: String,
  followers_url       :: String,
  following_url       :: String,
  gists_url           :: String,
  starred_url         :: String,
  subscriptions_url   :: String,
  organizations_url   :: String,
  repos_url           :: String,
  events_url          :: String,
  received_events_url :: String,
  type                :: String,
  site_admin          :: Bool
}

data Parent = Parent {
  sha      :: String,
  url      :: String,
  html_url :: String
}

data UserData = UserData {
  name  :: String,
  email :: String,
  date  :: String
}

data Tree = Tree {
  sha :: String,
  url :: String
}

data Verification = Verification {
  verified  :: Bool,
  reason    :: String,
  signature :: String,
  payload   :: String
}