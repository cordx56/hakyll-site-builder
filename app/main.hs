--------------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, Value (Array, Object, String), parseJSON)
import Data.Aeson.Key (Key, fromString)
import Data.Aeson.KeyMap (KeyMap, empty, insert)
import Data.Monoid (mappend)
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import Hakyll

--------------------------------------------------------------------------------
configFile :: FilePath
configFile = "config.yaml"

--------------------------------------------------------------------------------
data Config = Config
  { meta :: Maybe (KeyMap String)
  , src :: FilePath
  , out :: FilePath
  , templates :: FilePath
  , rules :: [Rule]
  }
  deriving (Generic, FromJSON)

data Rule = Rule
  { name :: Maybe String
  , patterns :: [String]
  , action :: RuleAction
  }
  deriving (Generic, FromJSON)

data RuleAction = CopyAction {}
instance FromJSON RuleAction where
  parseJSON (String "copy") = pure CopyAction{}
  parseJSON _ = error "Unknown action"

--------------------------------------------------------------------------------
data BuildState = BuildState
  { context :: Context String
  , builtIds :: KeyMap Pattern
  , buildingRules :: Rules ()
  }

insertBuiltIds :: BuildState -> Key -> Pattern -> BuildState
insertBuiltIds (BuildState{context = ctx, builtIds = bts, buildingRules = br}) k pat =
  BuildState{context = ctx, builtIds = insert k pat bts, buildingRules = br}
updateBuildingRules :: BuildState -> Rules () -> BuildState
updateBuildingRules (BuildState{context = ctx, builtIds = blts}) rl =
  BuildState{context = ctx, builtIds = blts, buildingRules = rl}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
buildAction :: BuildState -> Pattern -> RuleAction -> Rules ()
buildAction bs pat CopyAction = do
  match pat $ do
    route idRoute
    compile copyFileCompiler

buildRule :: BuildState -> Rule -> BuildState
buildRule bs (Rule{name = maynam, patterns = patts, action = act}) = do
  let pat = foldr ((.||.) . fromGlob) (fromList []) patts
      buildingRule = buildAction bs pat act
      newBs = updateBuildingRules bs buildingRule
      newBuildState = case maynam of
        Just nm -> insertBuiltIds bs (fromString nm) pat
        Nothing -> bs
  newBuildState
buildRules :: BuildState -> [Rule] -> BuildState
buildRules state (r : rs) = do
  buildRule state r

{-
buildRules = hakyll $ do
  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile
      $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/**" $ do
    route $ setExtension "html"
    compile
      $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "**" $ do
    route idRoute
    compile copyFileCompiler
-}
build :: Config -> IO ()
build (Config{src = sDir, out = oDir, templates = templs, rules = rls}) = hakyllWith hConf $ do
  buildingRules $ buildRules state rls
 where
  hConf =
    defaultConfiguration
      { providerDirectory = sDir
      , destinationDirectory = oDir
      , storeDirectory = ".cache"
      , tmpDirectory = ".tmp"
      , previewHost = "0.0.0.0"
      , previewPort = 8080
      }
  state = BuildState{context = defaultContext, builtIds = empty}

--------------------------------------------------------------------------------
main :: IO ()
main = do
  confE <- decodeFileEither configFile
  case confE of
    Left e -> do
      error ""
    Right conf -> do
      build conf

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext
