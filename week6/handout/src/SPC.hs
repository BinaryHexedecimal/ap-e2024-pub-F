{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    --pingSPC,
    Job(..),
    JobId,
    jobAdd,
    jobStatus,
    jobCancel,
    jobWait,
    JobStatus(..),
    JobDoneReason(..)

  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay, newChan,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)
--import Control.Monad
import Test.Tasty.Runners (Status(Done))

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.
-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)


-- Messages sent to SPC.
data SPCMsg = MsgJobAdd Job (ReplyChan JobId)
              | MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
              | MsgJobCancel JobId 
              | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
              | MsgJobDone JobId
              | MsgJobCrashed JobId
              | MsgTick


data SPCState = SPCState {
                        --spcPingCounter :: Int
                          spcChan :: Chan SPCMsg
                        , spcJobsPending ::[(JobId, Job)]
                        , spcJobCounter :: JobId
                        , spcJobsDone :: [(JobId, JobDoneReason)]
                        , spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))]
                        , spcJobRunning :: Maybe (JobId, Seconds, ThreadId)
                        }

data Job = Job {jobAction :: IO (),
                  jobMaxSeconds :: Int}

newtype JobId = JobId Int
  deriving(Show, Eq, Ord)


data JobDoneReason =
  Done_  --normal termination
  | DoneTimeout --killed because it ran for too long
  | DoneCancelled ---cancelled explicitly
  | DoneCrashed --due to exception
    deriving(Show, Eq, Ord)

data JobStatus =
  JobDone JobDoneReason
  | JobRunning
  | JobPending
  | JobUnknown
  deriving (Eq, Ord, Show)


--SPC monad (SPCM) is a state monad, also supports IO
newtype SPCM a = 
  SPCM (SPCState -> IO(a, SPCState))

instance Functor SPCM  where   
  fmap :: (a -> b) -> SPCM a -> SPCM b
  fmap = liftM

instance Applicative SPCM where
  pure :: a -> SPCM a
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) :: SPCM (a -> b) -> SPCM a -> SPCM b
  (<*>) = ap

instance Monad SPCM where
  (>>=) :: SPCM a -> (a -> SPCM b) -> SPCM b
  SPCM m >>= f = SPCM $ \state -> do
    (x, state' ) <- m state
    let SPCM f' = f x 
    f' state' 


startSPC :: IO SPC
startSPC = do
  let initial_state c = 
            SPCState {
              spcJobCounter = JobId 0,
              spcJobsPending = [], 
              spcJobsDone = [], 
              spcWaiting = [],
              spcChan = c,
              spcJobRunning = Nothing
            }
  server <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer server
  pure $ SPC server
  where 
    timer server _ = forever $ do
        threadDelay 1000000
        sendTo server MsgTick


handleMsg :: Chan SPCMsg -> SPCM()
handleMsg c = do
  schedule
  msg <- io $ receive c
  case msg of

    MsgJobStatus jobId rsvp -> do
      state <- get
      io $ reply rsvp $
        case (lookup jobId $ spcJobsPending state, 
              spcJobRunning state,
              lookup jobId $ spcJobsDone state) of
          (Just _, _,  _) -> Just JobPending 
          (_, Just (running_job,_, _), _) | jobId == running_job
                               -> Just JobRunning
          (_, _, Just r) ->  Just $ JobDone r
          _ -> Nothing 

    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid

    MsgJobCancel cancel_jobId -> do
      state <- get
      case spcJobRunning state of
        Just (jobId, _, tid)| jobId == cancel_jobId -> do
          io $ killThread tid
          jobDone jobId DoneCancelled
        _ -> pure()
              
    MsgJobWait jobId rsvp -> do
      state <- get
      case lookup jobId $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing ->
          put $ state {
            spcWaiting = (jobId, rsvp) : spcWaiting state
          }  

    MsgJobDone done_jobId -> do
      state <- get
      case spcJobRunning state of
        Just (jobId, _,_) 
          | jobId == done_jobId -> 
                jobDone jobId Done_
        _ -> pure()  

    MsgJobCrashed crashed_jobId -> do
      state <- get
      case spcJobRunning state of
        Just (jobId,_, tid) | jobId == crashed_jobId ->
          jobDone jobId DoneCrashed
        _ -> pure()  

    MsgTick -> pure()



-- pingSPC :: SPC -> IO Int
-- pingSPC (SPC c ) = requestReply c MsgPing

get :: SPCM SPCState
get = SPCM $ \state -> pure(state, state)

put :: SPCState -> SPCM()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f ) = fst <$> f state


jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = 
  requestReply c $ MsgJobAdd job

--query the job status
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobId =
  requestReply c $ MsgJobStatus jobId

--cancel job
jobCancel :: SPC -> JobId -> IO()
jobCancel (SPC c) jobId =
  sendTo c $ MsgJobCancel jobId

jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobId =
  requestReply c $ MsgJobWait jobId 

jobDone :: JobId -> JobDoneReason -> SPCM()  
jobDone jobId reason = do
  state <- get
  case lookup jobId $ spcJobsDone state of
    Just _ -> pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobId) . fst) (spcWaiting state)
      forM_  waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobId, reason) : spcJobsDone state,
            spcJobsPending = removeAssoc jobId $ spcJobsPending state
          }


schedule :: SPCM()
schedule = do
  state <- get
  case (spcJobRunning state, spcJobsPending state) of
    (Nothing, (jobId, job) : jobs) -> do
      t <- io $ forkIO $ do
        let doJob = do
              jobAction job
              send (spcChan state) $ MsgJobDone jobId
            onException :: SomeException -> IO()
            onException _ =
              send (spcChan state) $ MsgJobCrashed jobId  
        doJob `catch` onException
      now <- io getSeconds  
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $ 
        state{
          spcJobRunning = Just (jobId, deadline, t),
          spcJobsPending = jobs
        }
    _ -> pure ()


checkTimeouts :: SPCM()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  case spcJobRunning state of 
    Just (jobid, deadline, tid) | now >= deadline -> do
      io $ killThread tid
      put $ state {spcJobRunning = Nothing}
      jobDone jobid DoneTimeout
    _ -> pure()
