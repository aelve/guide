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
import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info,
                            infoHeader, infoOption, long, metavar, progDesc, short, strOption,
                            subparser, (<|>))
import Options.Applicative.Help.Chunk (stringChunk)

import Paths_guide (version)

import qualified Data.Text as T


-- | All available commands
data Command
    = Guide               -- ^ run server
    | DryRun              -- ^ load database and exit
    | LoadPublic String   -- ^ load PublicDB, create base on it and exit

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the application.
parseCli :: IO Command
parseCli = execParser
    $ modifyHeader
    $ info (helper <*> versionP <*> (pure Guide <|> commandsP))
    $ fullDesc -- <> progDesc "Guide is wiki"

-- | All possible commands.
commandsP :: Parser Command
commandsP = subparser
    $  command "run" (info (helper <*> pure Guide) $ progDesc "Run server")
    <> command "dry-run" (info (helper <*> pure DryRun) $ progDesc "Load database and exit")
    <> command "load-public" (info (helper <*> loadPublic) $ progDesc "Load PublicDB, create base on it and exit")

-- | Parse filepath of PublicDB.
loadPublic :: Parser Command
loadPublic = LoadPublic <$> strOption
    (long "path" <> short 'p' <> help "Public DB file name" <> metavar "FILEPATH")

-- | Parse version option.
versionP :: Parser (a -> a)
versionP = infoOption guideVersion
    $ long "version"
   <> short 'v'
   <> help "Show Guide version"

-- | Show current guide version in git system
guideVersion :: String
guideVersion = T.unpack $ T.intercalate "\n" $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = "Aelve Guide " <> "v" <> T.pack (showVersion version)
    sHash = " ➤ " <> ("Git revision: " <> $(gitHash))
    sDate = " ➤ " <> ("Commit date:  " <> $(gitCommitDate))
    sDirty = "There are non-committed files."

-- | Put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p {infoHeader = stringChunk "Guide is wiki"}

