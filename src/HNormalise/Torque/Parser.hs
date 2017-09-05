{- hnormalise - a log normalisation library
 -
 - Copyright Andy Georges (c) 2017
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - * Redistributions of source code must retain the above copyright
 - notice, this list of conditions and the following disclaimer.
 -
 - * Redistributions in binary form must reproduce the above
 - copyright notice, this list of conditions and the following
 - disclaimer in the documentation and/or other materials provided
 - with the distribution.
 -
 - * Neither the name of Author name here nor the names of other
 - contributors may be used to endorse or promote products derived
 - from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 - "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 - LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 - A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 - OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Torque.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative         ((<|>))
import           Data.Attoparsec.Combinator  (lookAhead, manyTill)
import           Data.Attoparsec.Text
import           Data.Char                   (isDigit, isSpace)
import qualified Data.Map                    as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.ParserCombinators.Perm ((<$$>), (<||>), (<$?>), (<|?>), permute)

--------------------------------------------------------------------------------
import           HNormalise.Common.Parser
import           HNormalise.Torque.Internal

--------------------------------------------------------------------------------
-- | 'parseTorqueWalltime' parses [[[DD:]HH:]MM:]SS strings representing walltime
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
-- | 'parseTorqueMemory' parses an decimal followed by a memory unit and return the memory in bytes
parseTorqueMemory :: Parser Integer
parseTorqueMemory = do
    v <- decimal
    unit <- asciiCI "b"
        <|> asciiCI "kb"
        <|> asciiCI "mb"
        <|> asciiCI "gb"
    return $ case T.toLower unit of
        "b"  -> v
        "kb" -> v * 1024
        "mb" -> v * 1024 * 1024
        "gb" -> v * 1024 * 1024 * 1024

--------------------------------------------------------------------------------
-- | 'parseTorqueJobName' splits the job name in its components, i.e., ID, [ array ID,] master and cluster
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
-- | 'parseTorqueResourceNodeList' parses a list of FQDN nodes and their ppn or a nodecount and its ppn
parseTorqueResourceNodeList :: Parser TorqueJobNode
parseTorqueResourceNodeList = do
    c <- peekChar'
    if Data.Char.isDigit c then do
        number <- decimal
        ppn <- maybeOption $ char ':' *> string "ppn=" *> decimal
        return $ TSN $ TorqueJobShortNode { number = number, ppn = ppn }
    else TFN <$> (flip sepBy (char '+') $ do
        fqdn <- Data.Attoparsec.Text.takeWhile (/= ':')
        ppn <- char ':' *> kvNumParser "ppn"
        return TorqueJobFQNode { name = fqdn, ppn = ppn})

--------------------------------------------------------------------------------
{- Examples found in the 2016 logs
Resource_List.advres Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.vmem Resource_List.walltime
Resource_List.mem Resource_List.ncpus Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.vmem Resource_List.walltime
Resource_List.mem Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.qos Resource_List.vmem Resource_List.walltime
Resource_List.mem Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.vmem Resource_List.walltime
Resource_List.mem Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.walltime
Resource_List.naccesspolicy Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.vmem Resource_List.walltime
Resource_List.ncpus Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.vmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.nodeset Resource_List.vmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.pmem Resource_List.vmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.pmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.pvmem Resource_List.vmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.qos Resource_List.vmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.vmem Resource_List.walltime
Resource_List.neednodes Resource_List.nice Resource_List.nodect Resource_List.nodes Resource_List.walltime
-}
-- | 'parseTorqueResourceRequest' parses all key value pairs denoting resources requested.
-- Most of these are not obligatory. Since the Torque documentation is vague on mentioning which entries occur, the last
-- 1.5 years of data we have were used to make an educated guess as to which keys might appear and in what order
parseTorqueResourceRequest :: Parser TorqueResourceRequest
parseTorqueResourceRequest = do
    permute $ TorqueResourceRequest
        <$?> (Nothing, Just `fmap` (skipSpace *> string "Resource_List.mem=" *> parseTorqueMemory))
        <|?> (Nothing, Just `fmap` (skipSpace *> kvTextParser "Resource_List.advres"))
        <|?> (Nothing, Just `fmap` (skipSpace *> kvTextParser "Resource_List.naccesspolicy"))
        <|?> (Nothing, Just `fmap` (skipSpace *> kvNumParser "Resource_List.ncpus"))
        <||> skipSpace *> string "Resource_List.neednodes=" *> parseTorqueResourceNodeList
        <|?> (Nothing, Just `fmap` (skipSpace *> kvNumParser "Resource_List.nice"))
        <||> skipSpace *> kvNumParser "Resource_List.nodect"
        <||> skipSpace *> string "Resource_List.nodes=" *> parseTorqueResourceNodeList
        <|?> (Nothing, Just `fmap` (skipSpace *> kvTextParser "Resource_List.select"))
        <|?> (Nothing, Just `fmap` (skipSpace *> kvTextParser "Resource_List.qos"))
        <|?> (Nothing, Just `fmap` (skipSpace *> string "Resource_List.pmem=" *> parseTorqueMemory))
        <|?> (Nothing, Just `fmap` (skipSpace *> string "Resource_List.vmem=" *> parseTorqueMemory))
        <|?> (Nothing, Just `fmap` (skipSpace *> string "Resource_List.pvmem=" *> parseTorqueMemory))
        <||> skipSpace *> string "Resource_List.walltime=" *> parseTorqueWalltime

--------------------------------------------------------------------------------
-- | 'parseTorqueResourceUsage' parses all the key value pairs denoting used resources.
parseTorqueResourceUsage :: Parser TorqueResourceUsage
parseTorqueResourceUsage = do
    cput <- skipSpace *> kvNumParser "resources_used.cput"
    energy <- skipSpace *> kvNumParser "resources_used.energy_used"
    mem <- skipSpace *> string "resources_used.mem=" *> parseTorqueMemory
    vmem <- skipSpace *> string "resources_used.vmem=" *> parseTorqueMemory
    walltime <- skipSpace *> string "resources_used.walltime=" *> parseTorqueWalltime
    return $ TorqueResourceUsage
        { cputime = cput
        , energy = energy
        , mem = mem
        , vmem = vmem
        , walltime = walltime
        }

--------------------------------------------------------------------------------
-- | 'parseTorqueHostList' parses a '+' separated list of hostname/coreranges
-- A core range can be of the form 1,3,5-7,9
parseTorqueHostList :: Parser [TorqueExecHost]
parseTorqueHostList = do
    string "exec_host="
    flip sepBy (char '+') $ do
        fqdn <- Data.Attoparsec.Text.takeWhile (/= '/')
        char '/'
        cores <- parseCores
        return $ TorqueExecHost { name = fqdn, cores = cores}
  where parseCores :: Parser [Int]
        parseCores = do
            cores <- flip sepBy1' (char ',') $ try parseRange <|> parseSingle
            return $ concat cores
        parseRange = do
            lower <- decimal
            char '-'
            upper <- decimal
            return [lower .. upper]
        parseSingle = do
            c <- decimal
            return [c]

--------------------------------------------------------------------------------
-- | `parseTorqueRequestor` parses a requestor string, i.e., the user plus machine issueing e.g., a delete request
parseTorqueRequestor :: Parser TorqueRequestor
parseTorqueRequestor = do
    string "requestor="
    user <- takeTill (== '@')
    char '@'
    whence <- takeTill isSpace   -- FIXME: this works in the given contexts, but might not be general enough.

    return TorqueRequestor
        { user = user
        , whence = whence
        }

--------------------------------------------------------------------------------
-- | 'parseTorqueExit' parses a complete log line denoting a job exit. Tested with Torque 6.1.x.
parseTorqueExit :: Parser (Text, TorqueParseResult)
parseTorqueExit = do
    takeTill (== ';') *> string ";E;"   -- drop the prefix
    name <- parseTorqueJobName
    user <- kvTextParser "user"
    group <- skipSpace *> kvTextParser "group"
    jobname <- skipSpace *> kvTextParser "jobname"
    queue <- skipSpace *> kvTextParser "queue"
    ctime <- skipSpace *> kvNumParser "ctime"
    qtime <- skipSpace *> kvNumParser "qtime"
    etime <- skipSpace *> kvNumParser "etime"
    start_count <- maybeOption $ skipSpace *> kvNumParser "start_count"
    start <- skipSpace *> kvNumParser "start"
    owner <- skipSpace *> kvTextParser "owner"
    exec_host <- skipSpace *> parseTorqueHostList
    request <- parseTorqueResourceRequest
    session <- skipSpace *> kvNumParser "session"
    total_execution_slots <- skipSpace *> kvNumParser "total_execution_slots"
    unique_node_count <- skipSpace *> kvNumParser "unique_node_count"
    end <- skipSpace *> kvNumParser "end"
    exit_status <- skipSpace *> kvNumParser "Exit_status"
    usage <- skipSpace *> parseTorqueResourceUsage

    return $ ("torque", TorqueExit $ TorqueJobExit
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
            , endTime = Just end
            }
        , execHost = exec_host
        , resourceRequest = request
        , resourceUsage = usage
        , totalExecutionSlots = total_execution_slots
        , uniqueNodeCount = unique_node_count
        , exitStatus = exit_status
        , torqueEntryType = TorqueExitEntry
        })

--------------------------------------------------------------------------------
-- | `parseTorqueDelete` parses a complete log line denoting a deleted job. Tested with Torue 6.1.x
parseTorqueDelete :: Parser (Text, TorqueParseResult)
parseTorqueDelete = do
    takeTill (== ';') *> string ";D;"   -- drop the prefix
    name <- parseTorqueJobName
    requestor <- parseTorqueRequestor

    return ("torque", TorqueDelete $ TorqueJobDelete
        { name = name
        , requestor = requestor
        , torqueEntryType = TorqueDeleteEntry
        })

--------------------------------------------------------------------------------
-- | `parseTorqueQueue` parses a complete log line denoting a queued job. Tested with Torue 6.1.x
parseTorqueQueue :: Parser (Text, TorqueParseResult)
parseTorqueQueue = do
    takeTill (== ';') *> string ";Q;"   -- drop the prefix
    name <- parseTorqueJobName
    queue <- kvTextParser "queue"

    return ("torque", TorqueQueue $ TorqueJobQueue
        { name = name
        , queue = queue
        , torqueEntryType = TorqueQueueEntry
        })

--------------------------------------------------------------------------------
-- | `parseTorqueStart` parses a complete log line denoting a started job. Tested with Torque 6.1.x
parseTorqueStart :: Parser (Text, TorqueParseResult)
parseTorqueStart = do
    takeTill (== ';') *> string ";S;"   -- drop the prefix
    name <- parseTorqueJobName
    user <- kvTextParser "user"
    group <- skipSpace *> kvTextParser "group"
    jobname <- skipSpace *> kvTextParser "jobname"
    queue <- skipSpace *> kvTextParser "queue"
    ctime <- skipSpace *> kvNumParser "ctime"
    qtime <- skipSpace *> kvNumParser "qtime"
    etime <- skipSpace *> kvNumParser "etime"
    start <- skipSpace *> kvNumParser "start"
    owner <- skipSpace *> kvTextParser "owner"
    exec_host <- skipSpace *> parseTorqueHostList
    request <- parseTorqueResourceRequest

    return $ ("torque", TorqueStart $ TorqueJobStart
        { name = name
        , user = user
        , group = group
        , jobname = jobname
        , queue = queue
        , owner = owner
        , times = TorqueJobTime
            { ctime = ctime
            , qtime = qtime
            , etime = etime
            , startTime = start
            , endTime = Nothing
            }
        , execHost = exec_host
        , resourceRequest = request
        , torqueEntryType = TorqueStartEntry
        })
