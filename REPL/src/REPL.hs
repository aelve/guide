{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module REPL ( processREPLCycle, 
              processDelayCycle,
              makeAlwaysCommand,
              REPLCommand(..), 
              isTrimCommand,
              isPrefixCommand) where 

import qualified Data.Char as DC
import qualified Control.Exception as X

import Data.List(isPrefixOf, find, nub, dropWhileEnd)
import Data.Maybe(fromMaybe)
import Control.Monad(forever, void)
import System.IO (stdout, hFlush)
import System.Exit(exitSuccess)
import Network.HTTP.Client(HttpException)
import Control.Concurrent (threadDelay)


import Common

data REPLCommand cnf = RC {
  -- the tag of the command to display
  cTag :: String, 
  -- returns true if the command matches
  cMatch :: String -> Bool,  
  -- executes the command
  cExec :: cnf -> String -> IO (),
  -- description of the command
  cDescription :: cnf -> String
}

makeAlwaysCommand :: REPLCommand a -> REPLCommand a
makeAlwaysCommand rc = RC {
  cTag = cTag rc,
  cMatch = const True,
  cExec = cExec rc,
  cDescription = cDescription rc
}

trim = dropWhileEnd DC.isSpace . dropWhile DC.isSpace
isTrimCommand commandValue commandStr = trim commandStr == commandValue

isPrefixCommand :: String -> String -> Bool
isPrefixCommand = isPrefixOf

--processREPLCycle = undefined
getDescriptionText :: cnf -> REPLCommand cnf -> String
getDescriptionText config command = cDescription command config

-- Performs the REPL cycle, until exit of quit command is entered
processREPLCycle :: cnf -> [REPLCommand cnf] -> IO()
processREPLCycle config commands = forever $ do
  -- get the next string
  putStr "Input command: "
  hFlush stdout
  commandStr <- getLine
  hFlush stdout
  -- if the command is not found, than help is shown
  let command = findCommand commandStr
  -- TODO. well, need to refactor exception handling
  cExec command config commandStr `X.catch` eh `X.catch` eh2 `X.catch` eh3 `X.catch` eh4
  where 
    commandsWithExtra = exitCommand : quitCommand : helpCommand : commands
    helpCommand = makeHelpCommand config commandsWithExtra
    findCommand str = fromMaybe helpCommand $ find (`cMatch` str) commandsWithExtra

    -- terrible code, definetely needs refactoring. Dunno how to do it better
    eh (e :: X.IOException) = putStrLn $ "IO Error: " ++ show e
    eh2 (e :: UpdateArchiveException) = putStrLn $ "Parsing error: " ++ show e 
    eh3 (e :: X.ErrorCall) = putStrLn $ "Error call: " ++ show e
    eh4 (e :: HttpException) = putStrLn $ "Http exception: " ++ show e

processDelayCycle :: cnf -> REPLCommand cnf -> Int -> IO()
processDelayCycle config command minutes = forever $ do
  putStrLn "Executing command"
  hFlush stdout
  cExec (makeAlwaysCommand command) config "" `X.catch` eh `X.catch` eh2 `X.catch` eh3 `X.catch` eh4

  putStrLn $ "Waiting " ++ show minutes ++ " minutes"
  hFlush stdout
  threadDelay (minutes * 60000)

  where 
    eh (e :: X.IOException) = putStrLn $ "IO Error: " ++ show e
    eh2 (e :: UpdateArchiveException) = putStrLn $ "Parsing error: " ++ show e 
    eh3 (e :: X.ErrorCall) = putStrLn $ "Error call: " ++ show e
    eh4 (e :: HttpException) = putStrLn $ "Http exception: " ++ show e

-- builds the help command
makeHelpCommand :: cnf -> [REPLCommand cnf] -> REPLCommand cnf 
makeHelpCommand config commands = RC {
    cTag = "help", 
    cMatch = (== "help"), 
    cExec = \_ _ -> putStr commandText,
    cDescription = const "help - displays help"
  }
  where    
    -- makes the line for the command by adding tab in front and br in back
    makeCommandText command = "\t" ++ getDescriptionText config command ++ "\n"

    -- gets all the tags from the list of commands. Nub is slow, but there are not many commands
    buildTags = nub . map cTag 
    -- all of the tags
    tags = buildTags commands

    -- true, if the command is satisfied with this tag
    isTag tag command = cTag command == tag

    -- gets all the commands according to tags
    commandBlocks = map (\tag -> (tag, filter (isTag tag) commands)) tags

    -- makes the text for the block of commands
    makeBlockText (tag, commands) = tag ++ ": \n" ++ concatMap makeCommandText commands 

    commandText = "Available commands: \n" ++ concatMap makeBlockText commandBlocks

exitCommand = RC {
  cTag = "exit",
  cMatch = isTrimCommand "exit",
  cExec = \_ _ -> exitREPL,
  cDescription = const "exit - exits the REPL" 
}

quitCommand = RC {
  cTag = "exit",
  cMatch = isTrimCommand "quit",
  cExec = \_ _ -> exitREPL,
  cDescription = const "quit - exits the REPL" 
}

exitREPL :: IO()
exitREPL = putStrLn "Finished working with REPL" >> exitSuccess
