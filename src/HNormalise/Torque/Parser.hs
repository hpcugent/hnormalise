{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Torque.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative         ((<|>))
import           Data.Attoparsec.Combinator  (lookAhead, manyTill)
import           Data.Attoparsec.Text
import           Data.Char                   (isDigit)
import qualified Data.Map                    as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.ParserCombinators.Perm ((<$$>), (<||>), permute)

--------------------------------------------------------------------------------
import           HNormalise.Common.Parser
import           HNormalise.Torque.Internal

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


--------------------------------------------------------------------------------
parseTorqueMemory :: Parser Integer
parseTorqueMemory = do
    v <- decimal
    unit <- string "b"
        <|> string "kb"
        <|> string "mb"
        <|> string "gb"
    return $ case unit of
        "b"  -> v
        "kb" -> v * 1024
        "mb" -> v * 1024 * 1024
        "gb" -> v * 1024 * 1024 * 1024

--------------------------------------------------------------------------------
parseTorqueJobName :: Parser TorqueJobName
parseTorqueJobName = do
    n <- decimal
    a <- parseArrayId
    m <- char '.' *> takeTill (== '.')
    c <- char '.' *> takeTill (== '.')
    manyTill anyChar (lookAhead ";") *> char ';'
    return $ TorqueJobName { number = n, array_id = a, master = m, cluster = c}
  where
    parseArrayId :: Parser (Maybe Integer)
    parseArrayId = try $ maybeOption $ do
        char '['
        i <- decimal
        char ']'
        return i


--------------------------------------------------------------------------------
parseTorqueResourceNodeList :: Parser (Either TorqueJobShortNode [TorqueJobFQNode])
parseTorqueResourceNodeList = do
    c <- peekChar'
    if isDigit c then do
        number <- decimal
        ppn <- maybeOption $ char ':' *> string "ppn=" *> decimal
        return $ Left $ TorqueJobShortNode { number = number, ppn = ppn }
    else Right <$> (flip sepBy (char '+') $ do
        fqdn <- Data.Attoparsec.Text.takeWhile (/= ':')
        ppn <- char ':' *> kvNumParser "ppn"
        return TorqueJobFQNode { name = fqdn, ppn = ppn})

--------------------------------------------------------------------------------
parseTorqueResourceRequest :: Parser TorqueResourceRequest
parseTorqueResourceRequest = do
    permute $ TorqueResourceRequest
        <$$> whitespace *> string "Resource_List.nodes=" *> parseTorqueResourceNodeList
        <||> whitespace *> string "Resource_List.vmem=" *> parseTorqueMemory
        <||> whitespace *> kvNumParser "Resource_List.nodect"
        <||> whitespace *> string "Resource_List.neednodes=" *> parseTorqueResourceNodeList
        <||> maybeOption (whitespace *> kvNumParser "Resource_List.nice")
        <||> whitespace *> string "Resource_List.walltime=" *> parseTorqueWalltime

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
parseTorqueHostList :: Parser [TorqueExecHost]
parseTorqueHostList = flip sepBy (char '+') $ do
    fqdn <- Data.Attoparsec.Text.takeWhile (/= '/')
    char '/'
    (lower, upper) <- try parseCoreRange <|> parseSingleCore
    return $ TorqueExecHost { name = fqdn, lowerCore = lower, upperCore = upper}
  where parseCoreRange :: Parser (Int, Int)
        parseCoreRange = do
            lower <- decimal
            char '-'
            upper <- decimal
            return (lower, upper)

        parseSingleCore = do
            lower <- decimal
            return (lower, lower)

--------------------------------------------------------------------------------
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
    request <- parseTorqueResourceRequest
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
