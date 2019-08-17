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
    = RunServer             -- ^ run server
    | DryRun                -- ^ load database and exit
    | LoadPublic FilePath   -- ^ load PublicDB, create base on it and exit

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

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
    $  Opt.command "run" (infoP (pure RunServer) "Start server")
    <> Opt.command "dry-run" (infoP (pure DryRun) "Load database and exit")
    <> Opt.command "load-public"
         (infoP loadPublicParser "Load PublicDB, create base on it and exit")
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
