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
    many' $ char ' '
    vmem <- string "Resource_List.vmem=" *> parseTorqueMemory
    many' $ char ' '
    nodect <- mkTorqueKVNumParser "Resource_List.nodect"
    many' $ char ' '
    neednodes <- string "Resource_List.neednodes=" *> parseTorqueResourceNodeList
    many' $ char ' '
    nice <- maybeOption $ mkTorqueKVNumParser "Resource_List.nice"
    many' $ char ' '
    walltime <- string "Resource_List.walltime=" *> parseTorqueWalltime
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
    cput <- mkTorqueKVNumParser "resources_used.cput"
    many' $ char ' '
    energy <- mkTorqueKVNumParser "resources_used.energy_used"
    many' $ char ' '
    mem <- string "resources_used.mem=" *> parseTorqueMemory
    many' $ char ' '
    vmem <- string "resources_used.vmem=" *> parseTorqueMemory
    many' $ char ' '
    walltime <- string "resources_used.walltime=" *> parseTorqueWalltime
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
    many' $ char ' '
    group <- mkTorqueKVTextParser "group"
    many' $ char ' '
    jobname <- mkTorqueKVTextParser "jobname"
    many' $ char ' '
    queue <- mkTorqueKVTextParser "queue"
    many' $ char ' '
    start_count <- maybeOption $ mkTorqueKVNumParser "start_count"
    ctime <- mkTorqueKVNumParser "ctime"
    many' $ char ' '
    qtime <- mkTorqueKVNumParser "qtime"
    many' $ char ' '
    etime <- mkTorqueKVNumParser "etime"
    many' $ char ' '
    start <- mkTorqueKVNumParser "start"
    many' $ char ' '
    owner <- mkTorqueKVTextParser "owner"
    many' $ char ' '
    exec_host <- parseTorqueHostList
    many' $ char ' '
    request <- parseTorqueResourceRequest
    many' $ char ' '
    session <- mkTorqueKVNumParser "session"
    many' $ char ' '
    total_execution_slots <- mkTorqueKVNumParser "total_execution_slots"
    many' $ char ' '
    unique_node_count <- mkTorqueKVNumParser "unique_node_count"
    many' $ char ' '
    end <- mkTorqueKVNumParser "end"
    many' $ char ' '
    exit_status <- mkTorqueKVNumParser "Exit_status"
    many' $ char ' '
    usage <- parseTorqueResourceUsage

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
