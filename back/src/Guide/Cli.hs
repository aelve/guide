{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Guide's command line interface.
--
-- Run @guide --help@ to see available commands.
module Guide.Cli
       ( Command (..)
       , parseCommandLine
       ) where


import Imports

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser)

import Paths_guide (version)

import qualified Data.Text as T
import qualified Options.Applicative as Opt


-- | All available commands
data Command
    = RunServer             -- ^ Run server
    | DryRun                -- ^ Load database and exit
    | LoadPublic FilePath   -- ^ Load PublicDB, create base on it and exit
    | ApiDocs               -- ^ Show docs for the backend API

-- | Parse the command line of the application.
--
-- If no command is supplied, we say the command was 'RunServer'.
parseCommandLine :: IO Command
parseCommandLine = Opt.execParser
    $ Opt.info (Opt.helper <*> versionOption <*> (pure RunServer <|> commandsParser))
    $ Opt.fullDesc

-- | All possible commands.
commandsParser :: Parser Command
commandsParser = Opt.subparser
    $  Opt.command "run"
         (infoP (pure RunServer) "Start server")
    <> Opt.command "dry-run"
         (infoP (pure DryRun) "Load database and exit")
    <> Opt.command "load-public"
         (infoP loadPublicParser "Load PublicDB, create base on it and exit")
    <> Opt.command "api-docs"
         (infoP (pure ApiDocs) "Show swagger.json for the backend API")
  where
    infoP parser desc = Opt.info (Opt.helper <*> parser) $ Opt.progDesc desc

-- | Parse the arguments of 'LoadPublic'.
loadPublicParser :: Parser Command
loadPublicParser = LoadPublic <$> Opt.strOption
    (  Opt.long "path"
    <> Opt.short 'p'
    <> Opt.help "Public DB file name"
    <> Opt.metavar "FILEPATH"
    )

-- | Parse version option.
versionOption :: Parser (a -> a)
versionOption = Opt.infoOption guideVersion
    $ Opt.long "version"
   <> Opt.short 'v'
   <> Opt.help "Show Guide version"

-- | A message with current Guide version and Git info.
guideVersion :: String
guideVersion = T.unpack $ T.intercalate "\n" $
    [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = "Aelve Guide " <> "v" <> T.pack (showVersion version)
    sHash = " ➤ " <> "Git revision: " <> $(gitHash)
    sDate = " ➤ " <> "Commit date:  " <> $(gitCommitDate)
    sDirty = "There are non-committed files."
