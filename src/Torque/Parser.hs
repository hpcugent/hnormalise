{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Torque.Parser where

--------------------------------------------------------------------------------
import Control.Applicative ( (<|>) )
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator ( lookAhead, manyTill )
import Data.Char ( isDigit )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Map as M
--------------------------------------------------------------------------------

import Torque.Internal
--------------------------------------------------------------------------------

mkTorqueKVTextParser :: Text -> Parser Text
mkTorqueKVTextParser key = string key *> char '=' *> takeTill (== ' ')

mkTorqueKVNumParser :: Integral a => Text -> Parser a
mkTorqueKVNumParser key = string key *> char '=' *> decimal

parseTorqueWalltime :: Parser TorqueWalltime
parseTorqueWalltime =
        parseTorqueDays
    <|> parseTorqueHours
    <|> parseTorqueMinutes
    <|> parseTorqueSeconds

parseTorqueDays = do
    d <- decimal
    char ':'
    w <- parseTorqueHours
    return w { days = d }

parseTorqueHours = do
    h <- decimal
    char ':'
    w <- parseTorqueMinutes
    return w { hours = h }

parseTorqueMinutes = do
    m <- decimal
    char ':'
    w <- parseTorqueSeconds
    return w { minutes = m }

parseTorqueSeconds = do
    s <- decimal
    return TorqueWalltime { days = 0, hours = 0, minutes = 0, seconds = s}

whitespace = many' (char ' ')

parseTorqueMemory :: Parser Integer
parseTorqueMemory = do
    v <- decimal
    unit <- string "b"
        <|> string "kb"
        <|> string "mb"
        <|> string "gb"
    return $ case unit of
        "b" -> v
        "kb" -> v * 1024
        "mb" -> v * 1024 * 1024
        "gb" -> v * 1024 * 1024 * 1024

parseTorqueJobName :: Parser TorqueJobName
parseTorqueJobName = do
    n <- decimal
    m <- char '.' *> takeTill (== '.')
    c <- char '.' *> takeTill (== '.')
    manyTill anyChar (lookAhead ";") *> char ';'
    return $ TorqueJobName { number = n, master = m, cluster = c}

parseTorqueResourceNodeList :: Parser (Either TorqueJobShortNode [TorqueJobFQNode])
parseTorqueResourceNodeList = do
    c <- peekChar'
    if isDigit c then do
        number <- decimal
        ppn <- char ':' *> string "ppn=" *> decimal
        return $ Left $ TorqueJobShortNode { number = number, ppn = ppn }
    else Right <$> (flip sepBy (char '+') $ do
        fqdn <- Data.Attoparsec.Text.takeWhile (/= ':')
        ppn <- char ':' *> string "ppn=" *> decimal
        return TorqueJobFQNode { name = fqdn, ppn = ppn})

parseTorqueResourceRequest :: Parser TorqueResourceRequest
parseTorqueResourceRequest = do
    nodes <- string "Resource_List.nodes=" *> parseTorqueResourceNodeList
    vmem <- whitespace *> string "Resource_List.vmem=" *> parseTorqueMemory
    nodect <- whitespace *> mkTorqueKVNumParser "Resource_List.nodect"
    neednodes <- whitespace *> string "Resource_List.neednodes=" *> parseTorqueResourceNodeList
    nice <- maybeOption $ whitespace *> mkTorqueKVNumParser "Resource_List.nice"
    walltime <- whitespace *> string "Resource_List.walltime=" *> parseTorqueWalltime
    return TorqueResourceRequest
        { nodes = nodes
        , vmem = vmem
        , nodeCount = nodect
        , neednodes = neednodes
        , nice = nice
        , walltime = walltime
        }

parseTorqueResourceUsage :: Parser TorqueResourceUsage
parseTorqueResourceUsage = do
    cput <- whitespace *> mkTorqueKVNumParser "resources_used.cput"
    energy <- whitespace *> mkTorqueKVNumParser "resources_used.energy_used"
    mem <- whitespace *> string "resources_used.mem=" *> parseTorqueMemory
    vmem <- whitespace *> string "resources_used.vmem=" *> parseTorqueMemory
    walltime <- whitespace *> string "resources_used.walltime=" *> parseTorqueWalltime
    return $ TorqueResourceUsage
        { cputime = cput
        , energy = energy
        , mem = mem
        , vmem = vmem
        , walltime = walltime
        }

parseTorqueHostList :: Parser [TorqueExecHost]
parseTorqueHostList = flip sepBy (char '+') $ do
        fqdn <- Data.Attoparsec.Text.takeWhile (/= '/')
        char '/'
        lower <- decimal
        char '-'
        upper <- decimal
        return $ TorqueExecHost { name = fqdn, lowerCore = lower, upperCore = upper}

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

parseTorqueExit :: Parser TorqueJobExit
parseTorqueExit = do
    _ <- manyTill anyChar (lookAhead ";E;") *> string ";E;"   -- drop the prefix
    name <- parseTorqueJobName
    user <- mkTorqueKVTextParser "user"
    group <- whitespace *> mkTorqueKVTextParser "group"
    jobname <- whitespace *> mkTorqueKVTextParser "jobname"
    queue <- whitespace *> mkTorqueKVTextParser "queue"
    start_count <- maybeOption $ whitespace *> mkTorqueKVNumParser "start_count"
    ctime <- whitespace *> mkTorqueKVNumParser "ctime"
    qtime <- whitespace *> mkTorqueKVNumParser "qtime"
    etime <- whitespace *> mkTorqueKVNumParser "etime"
    start <- whitespace *> mkTorqueKVNumParser "start"
    owner <- whitespace *> mkTorqueKVTextParser "owner"
    exec_host <- whitespace *> parseTorqueHostList
    request <- whitespace *> parseTorqueResourceRequest
    session <- whitespace *> mkTorqueKVNumParser "session"
    total_execution_slots <- whitespace *> mkTorqueKVNumParser "total_execution_slots"
    unique_node_count <- whitespace *> mkTorqueKVNumParser "unique_node_count"
    end <- whitespace *> mkTorqueKVNumParser "end"
    exit_status <- whitespace *> mkTorqueKVNumParser "Exit_status"
    usage <- whitespace *> parseTorqueResourceUsage

    return $ TorqueJobExit
        { name = name
        , user = user
        , group = group
        , jobname = jobname
        , queue = queue
        , startCount = start_count
        , owner = owner
        , session = session
        , times = TorqueJobTime
            { ctime = ctime
            , qtime = qtime
            , etime = etime
            , startTime = start
            , endTime = end
            }
        , resourceRequest = request
        , resourceUsage = usage
        , totalExecutionSlots = total_execution_slots
        , uniqueNodeCount = unique_node_count
        , exitStatus = exit_status
        }
