{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( ) where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Commit

type CommitsAPI = "repos" :> Capture "user" String :> Capture "repo" String :> "commits" :> GET '[JSON] [CommitStruct]