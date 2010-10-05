module MicroThread
    ( MicroThreadT
    , MonadMicroThread (..)
    , Request (..)
    , waitCompletion
    , waitForever
    , withSpark
    , timeout
    , runMicroThreadT
    ) where

import Control.Monad
import Control.Concurrent hiding (yield, killThread)
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.ST.Trans
import Control.Monad.Prompt
import Text.Printf
import Data.List
import Data.Ord
import Data.Int
import System.IO

type ThreadID = Int

newtype MicroThreadT s m a = MicroThreadT {
      unMicroThreadT ::
          ContT () (
                    StateT (SystemState s m) (
                                             STT s (
                                                    PromptT Request m
                                                   )
                                           )
                   ) a
    }
    deriving (Functor, Monad, MonadState (SystemState s m), MonadCont)

data Request a where
    GetCurrentTime :: Request Float
    ThreadDelay :: Float -> Request ()
    Trace :: String -> Request ()
    Warn :: String -> Request ()

data SystemState s m = SystemState
    {
      jumpout :: Yield s m -> MicroThreadT s m ()
    , current :: Thread s m
    , threads :: [Thread s m]
    , spark_threads :: [Thread s m]
    , max_thread_id :: ThreadID
    , abortSystem :: Bool
    }

data Thread s m = Thread
    {
      contThread :: () -> MicroThreadT s m ()
    , contThreadPred :: MicroThreadT s m Bool
    , invariant :: Maybe (Invariant s m)
    , scheduled :: Float
    , threadID :: ThreadID
    , parentID :: Maybe ThreadID
    , finalisers :: [Finaliser s m]
    }

type Finaliser s m = MicroThreadT s m ()

type Invariant s m = MicroThreadT s m Bool

data Yield s m = Delay Float
             | Spark [Thread s m]
             | SparkAndDie [Thread s m]
             | Nop
             | Die
             | Kill ThreadID
             | Abort

liftST :: STT s (PromptT Request m) a -> MicroThreadT s m a
liftST = MicroThreadT . lift . lift

instance MonadPrompt Request (MicroThreadT s m) where
    prompt = MicroThreadT . lift . lift . lift . prompt

instance MonadTrans (MicroThreadT s) where
    lift = MicroThreadT . lift . lift . lift . lift

class (Monad m) => MonadMicroThread m where
    delay :: Float -> m ()
    wait :: m Bool -> m ()
    time :: m Float
    hold :: m Bool -> m a -> m (Maybe a)
    finally :: m () -> m a -> m a

    spark :: m () -> m ThreadID
    terminate :: ThreadID -> m ()
    abort :: m ()

    getCurrentThread :: m ThreadID
    isThreadAlive :: ThreadID -> m Bool

instance MonadMicroThread (MicroThreadT s m) where
    delay dt =
        yield (Delay dt)

    spark thread =
        do current <- getCurrentThread
           id <- pickThreadID
           trace $ "sparking " ++ show id
           yield (Spark [newThread id (Just current) thread])
           return id

    terminate threadID =
        do trace $ "terminating " ++ show threadID
           yield $ Kill threadID

    wait cond =
        withT (\t -> t { contThreadPred = cond }) $
              yield Nop

    hold condition action =
        do rref <- liftST $ newSTRef Nothing
           spark_id <-
               spark $ do
                 id <- getCurrentThread
                 trace $ printf "thread %d holding new invariant" id
                 modifyCurrentT $ \t -> t { invariant = Just condition }
                 finally ( do trace $ printf "thread %d releasing invariant" id
                              id' <- getCurrentThread
                              when (id /= id') $
                                   error $ printf "FATAL!! unexpected current thread, expected %d got %d" id id'
                              modifyCurrentT $ \t -> t { invariant = Nothing }
                         ) $
                       do r <- action
                          liftST $ writeSTRef rref (Just r)
           waitCompletion spark_id
           liftST $ readSTRef rref

    time = prompt GetCurrentTime

    finally guard f =
        do trace $ "FINALLY BEGIN"
           r <- withT t' $ f
           guard'
           return r
        where t' t = t {
                       finalisers = guard' : (finalisers t)
                     }
              guard' = trace ("FINALLY END") >> guard

    -- destroys all threads
    abort =
        do s <- get
           trace $ "ABORT CALLED, current threads: " ++ show (map threadID (threads s))
           yield Abort

    getCurrentThread =
        get >>= \s -> return . threadID . current $ s

    isThreadAlive id =
        do s <- get
           let ids = map threadID $ threads s
           return $ id `elem` ids

timeout :: (MonadMicroThread m) => Float -> m a -> m (Maybe a)
timeout max_t f =
    do t0 <- time
       hold (no_timeout t0) f
    where
      no_timeout t0 = time >>= \t -> return $ t - t0 < max_t

waitCompletion :: (MonadMicroThread m) => ThreadID -> m ()
waitCompletion id = wait (isThreadAlive id >>= return . not)

waitForever :: (MonadMicroThread m) => m ()
waitForever = wait ( return False )
    
withSpark :: (MonadMicroThread m) => m () -> (ThreadID -> m a) -> m a
withSpark thread f =
    do new_id <- spark thread
       finally (maybe_terminate new_id) $
               f new_id
       where
         maybe_terminate id =
             do alive <- isThreadAlive id
                when alive $
                     terminate id

instance Show (Yield s m) where
    show (Delay f) = printf "delay %2.4f" f
    show (Spark _) = "spark"
    show (SparkAndDie _) = "spark-and-die"
    show Die = "die"
    show Nop = "nop"
    show (Kill id) = "kill " ++ show id
    show Abort = "abort"

yield :: Yield s m -> MicroThreadT s m ()
yield y =
    callCC $ \cont ->
        do s <- get
           let c  = current s
               c' = c { contThread = cont }
           put $ s { current = c' }
           jumpout s y

pickThreadID :: MicroThreadT s m ThreadID
pickThreadID =
    do s <- get
       let id = max_thread_id s + 1
       modify $ \s -> s { max_thread_id = id }
       return id

modifyCurrentT :: (Thread s m -> Thread s m) -> MicroThreadT s m ()
modifyCurrentT f =
    modify $ \s ->
        let c  = f (current s) in
        s { current = c }

modifyThread :: ThreadID -> (Thread s m -> Thread s m) -> MicroThreadT s m ()
modifyThread thread_id f =
    modify $ \s -> s { threads = foldl' change [] (threads s) }
    where
      change acc th | threadID th == thread_id    = f th : acc
                    | otherwise                   = th   : acc


withT :: (Thread s m -> Thread s m) -> MicroThreadT s m a -> MicroThreadT s m a
withT mod act =
    do s <- get
       let c  = current s
           c' = mod c
       put $ s { current = c' }
       r <- act
       modify $ \s -> s { current = c }
       return r

newThread :: ThreadID -> (Maybe ThreadID) -> MicroThreadT s m () -> Thread s m
newThread id parent_id thread =
    Thread { contThread = \ _ -> thread
           , contThreadPred = return True
           , invariant = Nothing
           , scheduled = 0
           , threadID = id
           , parentID = parent_id
           , finalisers = [] }

getThread :: ThreadID -> MicroThreadT s m (Maybe (Thread s m))
getThread id =
    do s <- get
       return $ find id (threads s)
    where
      find id ts = case filter ( \t -> threadID t == id ) ts of
                     [ t ] -> Just t
                     [ ] -> Nothing
                     _-> error $ "more than one thread with id " ++ show id

children :: ThreadID -> MicroThreadT s m [ThreadID]
children id =
    do s <- get
       return . map threadID . filter child $ threads s
    where
      child t = parentID t == (Just id)

descendants :: ThreadID -> MicroThreadT s m [ThreadID]
descendants id =
    do cs <- children id
       ds <- mapM descendants cs
       return $ cs ++ (concat ds)

killThread :: ThreadID -> MicroThreadT s m ()
killThread thread_id =
    do ds <- descendants thread_id
       -- kill all descendant threads first
       mapM_ killThread ( reverse . sort $ ds )
       thread <- getThread thread_id
       case thread of
         Just thread ->
             do trace $ "killing " ++ show thread_id
                -- appease finalisers
                modify $ \s -> s { current = thread, threads = filter p (threads s) }
                -- execute any finalisers
                let fs = finalisers thread
                when (not . null $ fs) $
                     trace $ printf "%d has finalisers, executing" thread_id
                sequence_ fs
         Nothing ->
             return ()
  where
    p t = threadID t /= thread_id

trace :: String -> MicroThreadT s m ()
trace msg = prompt (Trace msg)

warn :: String -> MicroThreadT s m ()
warn msg = prompt (Warn msg)

diffTime :: Float -> MicroThreadT s m Float
diffTime t0 =
    do t <- prompt GetCurrentTime
       return (t-t0)

quantum_ms :: Int
quantum_ms = 10

runner :: Float -> MicroThreadT s m ()
runner t0 =
    do t <- diffTime t0
       s <- get
       trace $ printf "----> %05.3f -> tick, %d threads" t (length $ threads s)

       handle t (threads s)
       -- apply sparked threads onto thread heap
       s <- get
       modify (\s -> s { threads = spark_threads s ++ (threads s)
                       , spark_threads = [ ] })
       -- quit if no threads are left
       s <- get
       case threads s of
         [] -> return ()
         xs -> do when (length xs > 20) $
                       warn $ "number of threads: " ++ show (length xs)
                  prompt $ ThreadDelay (fromIntegral quantum_ms / 1000.0)
                  runner t0
    where
      handle t threads = handle' t (sortBy (comparing scheduled) threads)
      handle' t [] = return ()
      handle' t (x:xs)
          | scheduled x > t = return ()
          | otherwise       = iteration x >> handle' t xs

      iteration thread =
          do yield <- callCC $ exec thread -- execute it upto yield
             -- the thread context might've been modified during execution, we have fetch it from current
             s <- get
             let thread' = current s
             trace $ show (threadID thread') ++ " yield: " ++ show yield
             reschedule thread' yield
      
      exec thread engine = do
        trace $ "CONTEXT SWITCH to " ++ show (threadID thread)
        modify ( \s -> s { current = thread
                         , jumpout = engine
                         } )
        s <- get
        case (abortSystem s) of
          True  -> return Die
          False -> exec_real thread engine

      exec_real thread engine = do
        pred <- contThreadPred thread
        v <- check_invariant thread
        case (pred,v) of
          (False,True) -> return Nop  -- current predicate does not hold, don't execute
          (True ,True) -> -- all is well, execute thread
               do trace $ "executing " ++ show (threadID thread)
                  contThread thread ()
                  s <- get
                  trace $ show (threadID $ current s) ++ " finished"
                  -- if we're here that means thread finished it execution gracefully
                  return Die
          (_,False) -> -- invariant violation
                do let current_id = threadID thread
                   trace $ "invariant - violation by thread " ++ show current_id
                   -- clear invariant for this thread
                   modifyThread current_id $ \thread -> thread { invariant = Nothing }
                   -- and DIE
                   return Die

      add_sparks ths =
          modify (\s -> s{ spark_threads = spark_threads s ++ ths })

      reschedule thread yielded = do
        t <- diffTime t0
        s <- get
        let id = threadID thread
            quantum_dt = fromIntegral quantum_ms / 1000.0
        case yielded of
          Nop            -> modifyThread id $ \ _ -> thread { scheduled = t + quantum_dt }
          Delay dt       -> modifyThread id $ \ _ -> thread { scheduled = t + max quantum_dt dt }
          Spark th       -> do add_sparks th
                               modifyThread id $ \ _ -> thread { scheduled = t + quantum_dt }
          SparkAndDie th -> do add_sparks th
                               killThread id
          Kill id'       -> do killThread id'
                               modifyThread id $ \ _ -> thread { scheduled = t + quantum_dt }
          Die            -> killThread id
          Abort          -> modify $ \s -> s { abortSystem = True }

      check_invariant x = do
        case invariant x of
          Nothing  -> return True
          Just inv -> inv
                 

runMicroThreadT :: (Monad m) =>
                   (forall a. Request a -> m a) ->
                   (forall s. MicroThreadT s m ()) ->
                   m ()
runMicroThreadT req bot =
    let cont  = unMicroThreadT (runner 0)
        stat  = runContT cont return
        st    = evalStateT stat s0
        pmp   = runST st
    in
      runPromptT return (\p cont -> req p >>= cont) (\m cont -> m >>= cont) pmp 
    where
      s0 = SystemState { jumpout = undefined
                       , current = undefined
                       , threads = [ newThread 0 Nothing bot ]
                       , spark_threads = [ ]
                       , max_thread_id = 0
                       , abortSystem = False
                       }
