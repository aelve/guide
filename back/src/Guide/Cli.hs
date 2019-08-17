{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module adds command line interface to run server.
--
-- See help with @guide --help@ to see available commands.
module Guide.Cli
       ( Command (..)
       , parseCli
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
    = RunServer             -- ^ run server
    | DryRun                -- ^ load database and exit
    | LoadPublic FilePath   -- ^ load PublicDB, create base on it and exit

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Backend uses command line interface:
{-
To see help run command:
$ guide --help
Usage: guide [-v|--version] [COMMAND]

Available options:
  -h,--help                Show this help text
  -v,--version             Show Guide version

Available commands:
  run                      Run server
  dry-run                  Load database and exit
  load-public              Load PublicDB, create base on it and exit

NOTE:
Command 'guide' is the same as 'guide run'

----------------------------------------------------------------------------

$ guide load-public --help
Usage: guide load-public (-p|--path FILEPATH)
  Load PublicDB, create base on it and exit

Available options:
  -h,--help                Show this help text
  -p,--path FILEPATH       Public DB file name

-}

-- | Main parser of the application.
parseCli :: IO Command
parseCli = Opt.execParser
    $ Opt.info (Opt.helper <*> versionP <*> (pure RunServer <|> commandsP))
    $ Opt.fullDesc

-- | All possible commands.
commandsP :: Parser Command
commandsP = Opt.subparser
    $  Opt.command "run" (infoP (pure RunServer) "Start server")
    <> Opt.command "dry-run" (infoP (pure DryRun) "Load database and exit")
    <> Opt.command "load-public"
        (infoP (loadPublic) "Load PublicDB, create base on it and exit")
  where
    infoP parser desc = Opt.info (Opt.helper <*> parser) $ Opt.progDesc desc

-- | Parse filepath of PublicDB.
loadPublic :: Parser Command
loadPublic = LoadPublic <$> Opt.strOption
    (  Opt.long "path"
    <> Opt.short 'p'
    <> Opt.help "Public DB file name"
    <> Opt.metavar "FILEPATH"
    )

-- | Parse version option.
versionP :: Parser (a -> a)
versionP = Opt.infoOption guideVersion
    $ Opt.long "version"
   <> Opt.short 'v'
   <> Opt.help "Show Guide version"

-- | Show current guide version in git system
guideVersion :: String
guideVersion = T.unpack $ T.intercalate "\n" $
    [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = "Aelve Guide " <> "v" <> T.pack (showVersion version)
    sHash = " ➤ " <> ("Git revision: " <> $(gitHash))
    sDate = " ➤ " <> ("Commit date:  " <> $(gitCommitDate))
    sDirty = "There are non-committed files."
