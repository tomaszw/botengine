module MicroThread ( MicroThreadT
           , MonadMicroThread (..)
           , Request (..)
           , waitCompletion
           , withSpark
           , runMicroThreadT ) where

import Control.Monad
import Control.Concurrent hiding (yield, killThread)
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Prompt
import Text.Printf
import Data.List
import Data.Ord
import System.Time
import System.IO

type ThreadID = Int

newtype MicroThreadT m a = MicroThreadT {
      unMicroThreadT :: ContT () (StateT (SystemState m) (PromptT Request m)) a
    }
    deriving (Monad, MonadState (SystemState m), MonadCont)

instance MonadPrompt Request (MicroThreadT m) where
    prompt = MicroThreadT . lift . lift . prompt

instance MonadTrans MicroThreadT where
    lift = MicroThreadT . lift . lift . lift

class (Monad m) => MonadMicroThread m where
    delay :: Float -> m ()
    spark :: m () -> m ThreadID
    terminate :: ThreadID -> m ()
    wait :: m Bool -> m ()
    time :: m Float
    timeout :: Float -> m () -> m ()
    invariant :: m Bool -> m () -> m a -> m a
    finally :: m () -> m a -> m a
    currentThreadID :: m ThreadID
    isThreadAlive :: ThreadID -> m Bool

instance MonadMicroThread (MicroThreadT m) where
    delay dt =
        yield (Delay dt)

    spark thread =
        do id <- pickThreadID
           trace $ "sparking " ++ show id
           yield (Spark [newThread id thread])
           return id

    terminate threadID =
        do trace $ "terminating " ++ show threadID
           yield $ Kill threadID

    wait cond =
        withT (\t -> t { contThreadPred = cond }) $
              yield Nop

    isThreadAlive id =
        do s <- get
           let ids = map threadID $ threads s
           return $ id `elem` ids

    -- if invariant does not hold at any point of execution of given block,
    -- terminate current thread and spark a new one (passed as violated arg)
    invariant hold violated f =
        do s <- get 
           let invs = invariants (current s)
           -- only add it, don't release -> supposed to be done by runtime
           modifyCurrentT (\t -> t { invariants = (hold,violated):invs })
           f

    currentThreadID =
        get >>= \s -> return . threadID . current $ s

    time = prompt GetCurrentTime

    finally guard f =
        do r <- withT t' $ f
           guard
           return r
        where t' t = t {
                       finalisers = guard : (finalisers t)
                     }

    timeout max_t f =
        time >>= run
        where
          run t0 = do
            id <- spark $ do
                     let testTimeout = time >>= \t -> return $ t - t0 < max_t
                     invariant testTimeout (return ()) f
            waitCompletion id

waitCompletion :: (MonadMicroThread m) => ThreadID -> m ()
waitCompletion id = wait (isThreadAlive id >>= return . not)

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

data Request a where
    GetCurrentTime :: Request Float
    ThreadDelay :: Float -> Request ()
    Trace :: String -> Request ()

data Thread m = Thread
    {
      contThread :: () -> MicroThreadT m ()
    , contThreadPred :: MicroThreadT m Bool
    , invariants :: [ (MicroThreadT m Bool, MicroThreadT m ()) ] -- hold condition , what to do when violated
    , scheduled :: Float
    , threadID :: ThreadID
    , finalisers :: [Finaliser m]
    }

type Finaliser m = MicroThreadT m ()

data Yield m = Delay Float
             | Spark [Thread m]
             | SparkAndDie [Thread m]
             | Nop
             | Die
             | Kill ThreadID

instance Show (Yield m) where
    show (Delay f) = printf "delay %2.4f" f
    show (Spark _) = "spark"
    show (SparkAndDie _) = "spark-and-die"
    show Die = "die"
    show Nop = "nop"
    show (Kill id) = "kill " ++ show id

data SystemState m = SystemState
    {
      jumpout :: Yield m -> MicroThreadT m ()
    , current :: Thread m
    , threads :: [Thread m]
    , spark_threads :: [Thread m]
    , max_thread_id :: ThreadID
    }

yield :: Yield m -> MicroThreadT m ()
yield y =
    callCC $ \cont ->
        do s <- get
           let c  = current s
               c' = c { contThread = cont }
           put $ s { current = c' }
           jumpout s y

pickThreadID :: MicroThreadT m ThreadID
pickThreadID =
    do s <- get
       let id = max_thread_id s + 1
       modify $ \s -> s { max_thread_id = id }
       return id

modifyCurrentT :: (Thread m -> Thread m) -> MicroThreadT m ()
modifyCurrentT f =
    modify $ \s ->
        let c  = f (current s) in
        s { current = c }

withT :: (Thread m -> Thread m) -> MicroThreadT m a -> MicroThreadT m a
withT mod act =
    do s <- get
       let c  = current s
           c' = mod c
       put $ s { current = c' }
       r <- act
       modify $ \s -> s { current = c }
       return r

newThread :: ThreadID -> MicroThreadT m () -> Thread m
newThread id thread =
    Thread { contThread = \ _ -> thread
           , contThreadPred = return True
           , invariants = []
           , scheduled = 0
           , threadID = id
           , finalisers = [] }

getThread :: ThreadID -> MicroThreadT m (Thread m)
getThread id =
    do s <- get
       return $ find id (threads s)
    where
      find id ts = case filter ( \t -> threadID t == id ) ts of
                     [ t ] -> t
                     [ ] -> error $ "no thread of id " ++ show id
                     _-> error $ "more than one thread with id " ++ show id


killThread :: ThreadID -> MicroThreadT m ()
killThread thread_id =
    do trace $ "killing " ++ show thread_id
       thread <- getThread thread_id
       modify  $ \s -> s { threads = filter p (threads s) }
       -- execute any finalisers
       let fs = finalisers thread
       sequence_ fs
  where
    p t = threadID t /= thread_id

trace :: String -> MicroThreadT m ()
trace msg = prompt (Trace msg)

diffTime :: Float -> MicroThreadT m Float
diffTime t0 =
    do t <- prompt GetCurrentTime
       return (t-t0)

quantum_ms :: Int
quantum_ms = 10

runner :: Float -> MicroThreadT m ()
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
         xs -> do prompt $ ThreadDelay (fromIntegral quantum_ms / 1000.0)
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
        modify ( \s -> s { current = thread
                         , jumpout = engine
                         } )
        pred <- contThreadPred thread
        v <- check_invariants thread
        case (pred,v) of
          (False,[]) -> return Nop  -- current predicate does not hold, don't execute
          (True ,[]) -> -- all is well, execute thread
               do trace $ "executing " ++ show (threadID thread)
                  contThread thread ()
                  s <- get
                  trace $ show (threadID $ current s) ++ " finished"
                  -- if we're here that means thread finished it execution gracefully
                  return Die
          (_,viols) -> -- if invariants don't hold, kill the thread and invoke specified actions in NEW threads
                do let current_id = threadID thread
                   trace $ "invariant violation by " ++ show (threadID thread)
                   -- clear invariants for this thread
                   replace current_id $ thread { invariants = [] }
                   -- spark invariant actions
                   id <- pickThreadID
                   let t = newThread id (head viols)
                   return $ SparkAndDie [t]

      replace thread_id th' =
          modify $ \s -> s { threads = foldl' f [] (threads s) }
          where
            f acc th | threadID th == thread_id    = th' : acc
                     | otherwise                   = th  : acc

      add_sparks ths =
          modify (\s -> s{ spark_threads = spark_threads s ++ ths })

      reschedule thread yielded = do
        t <- diffTime t0
        s <- get
        let id = threadID thread
            quantum_dt = fromIntegral quantum_ms / 1000.0
        case yielded of
          Nop            -> replace id $ thread { scheduled = t + quantum_dt }
          Delay dt       -> replace id $ thread { scheduled = t + max quantum_dt dt }
          Spark th       -> do add_sparks th
                               replace id $ thread { scheduled = t + quantum_dt }
          SparkAndDie th -> do add_sparks th
                               killThread id
          Kill id'       -> do killThread id'
                               replace id $ thread { scheduled = t + quantum_dt }
          Die            -> killThread id

      check_invariants :: Thread m -> MicroThreadT m [MicroThreadT m ()]
      check_invariants x = do
        r <- foldM check [] (invariants x)
        return r
        where
          check acc (hold,violated) =
              do holds <- hold
                 if holds
                    then return acc
                    else return $ violated : acc

runMicroThreadT :: (Monad m) => (forall a. Request a -> m a) -> MicroThreadT m () -> m ()
runMicroThreadT req bot =
    let cont = unMicroThreadT (runner 0)
        stat = runContT cont return
        pmp = evalStateT stat s0
    in
      runPromptT return (\p cont -> req p >>= cont) (\m cont -> m >>= cont) pmp 
    where
      s0 = SystemState { jumpout = undefined
                       , current = undefined
                       , threads = [ newThread 0 bot ]
                       , spark_threads = [ ]
                       , max_thread_id = 0
                       }
