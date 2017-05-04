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

import Common.Parser
import Torque.Internal
--------------------------------------------------------------------------------

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
        ppn <- char ':' *> kvNumParser "ppn"
        return TorqueJobFQNode { name = fqdn, ppn = ppn})

parseTorqueResourceRequest :: Parser TorqueResourceRequest
parseTorqueResourceRequest = do
    nodes <- string "Resource_List.nodes=" *> parseTorqueResourceNodeList
    vmem <- whitespace *> string "Resource_List.vmem=" *> parseTorqueMemory
    nodect <- whitespace *> kvNumParser "Resource_List.nodect"
    neednodes <- whitespace *> string "Resource_List.neednodes=" *> parseTorqueResourceNodeList
    nice <- maybeOption $ whitespace *> kvNumParser "Resource_List.nice"
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
    cput <- whitespace *> kvNumParser "resources_used.cput"
    energy <- whitespace *> kvNumParser "resources_used.energy_used"
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

parseTorqueExit :: Parser TorqueJobExit
parseTorqueExit = do
    _ <- manyTill anyChar (lookAhead ";E;") *> string ";E;"   -- drop the prefix
    name <- parseTorqueJobName
    user <- kvTextParser "user"
    group <- whitespace *> kvTextParser "group"
    jobname <- whitespace *> kvTextParser "jobname"
    queue <- whitespace *> kvTextParser "queue"
    start_count <- maybeOption $ whitespace *> kvNumParser "start_count"
    ctime <- whitespace *> kvNumParser "ctime"
    qtime <- whitespace *> kvNumParser "qtime"
    etime <- whitespace *> kvNumParser "etime"
    start <- whitespace *> kvNumParser "start"
    owner <- whitespace *> kvTextParser "owner"
    exec_host <- whitespace *> parseTorqueHostList
    request <- whitespace *> parseTorqueResourceRequest
    session <- whitespace *> kvNumParser "session"
    total_execution_slots <- whitespace *> kvNumParser "total_execution_slots"
    unique_node_count <- whitespace *> kvNumParser "unique_node_count"
    end <- whitespace *> kvNumParser "end"
    exit_status <- whitespace *> kvNumParser "Exit_status"
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
