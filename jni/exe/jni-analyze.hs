-- This program analyzes eventlogs produced with JNI in a program @prog@
-- when invoked as
--
-- > HASKELL_JNI_TRACE=localrefs prog +RTS -l-au -RTS
--
-- Invoke @jni-analyze@ as
--
-- > jni-analize prog.eventlog
--
-- The output of the analysis is a file @prog.stacksamples@ with stack samples
-- listing the maximum count of local references found in a given stack.
-- The stack frames with the highest reference counts are listed first.
--
-- The output file is useful to find leaks in the code without further
-- processing. But the output format is suitable to produce a flame-graph if
-- desired.
--
-- > flamegraph.pl prog.stacksamples > prog.svg

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when, forM, forM_)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32)
import qualified Data.IntMap as IM
import Data.List (intersperse, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Data.Version (showVersion)
import Foreign.JNI.Trace
import Foreign.Ptr (IntPtr)
import GHC.Stack (HasCallStack)
import GHC.RTS.Events
  (readEventLogFromFile, Event(..), EventInfo(..), Data(..), EventLog(..))
import GHC.RTS.Events.Analysis (Machine(..), validates)
import Options.Applicative
import qualified Paths_jni as JNI (version)
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.FilePath (addExtension, dropExtension, (</>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
    cfg <- getConfig
    r <- readEventLogFromFile (cfgEventlogFile cfg)
    case r of
      Left err -> error err
      Right e -> do
        let outfile = sampleStackFileName (cfgEventlogFile cfg)
        withFile outfile WriteMode $ \hSamples -> do
          putStrLn "Sorting events ..."
          sortUserEvents (events $ dat e) $ \evs -> do
            putStrLn "Analyzing events ..."
            analyzeRefs cfg hSamples evs
            putStrLn $ "The output was written to " ++ show outfile ++ "."
  where
    sampleStackFileName f = addExtension (dropExtension f) ".stacksamples"

data Config = Config
    { cfgEventlogFile :: FilePath
    , cfgMaxLocalRefs :: Int
    , cfgMaxStackDepth :: Int
    , cfgCheckFrameCapacities :: Bool
    , cfgReportsAsErrors :: Bool
    }

getConfig :: IO Config
getConfig = do
    exe <- getProgName
    execParser (opts exe)
  where
    opts exe = info (parseConfig <**> helper) $ mconcat [
        fullDesc
      , Options.Applicative.header $ unlines
          [ exe ++ " " ++ showVersion JNI.version
          , "Utility for analyzing traces produced with JNI programs."
          ]
      , progDesc $ unlines
          [ "The output of the analysis is a file FILE.stacksamples with"
          , "stack samples listing the maximum count of local references found"
          , "in each stack frame. The frames are listed in descending order"
          , "so the ones with the most local references appear first."
          , "A flame-graph can be produced by feeding the stack samples to a"
          , "graph generation tool."
          ]
      ]

parseConfig :: Parser Config
parseConfig = Config <$>
    strArgument (mconcat
      [ help "Path to an eventlog file."
      , metavar "FILE.eventlog"
      ])
    <*> option auto (mconcat
      [ long "max-local-refs"
      , short 'l'
      , help "Report frames which have more than this amount of local references."
      , metavar "INT"
      , showDefault
      , value 30
      ])
    <*> option auto (mconcat
      [ long "max-stack-depth"
      , short 'd'
      , help "Report frames beyond this depth."
      , metavar "INT"
      , showDefault
      , value 64
      ])
    <*> flag True False (mconcat
      [ long "no-frame-capacities"
      , short 'c'
      , help "Don't check if frame capacities are exceeded."
      , showDefault
      ])

    <*> switch (mconcat
      [ long "reports-as-errors"
      , short 'e'
      , help "Stop the analysis when something to report is found."
      , showDefault
      ])

-- | Writes stack samples to the given handle.
analyzeRefs :: Config -> Handle -> [Event] -> IO ()
analyzeRefs cfg hSamples xs = do
   let mevents = [ y | x <- xs, Just y <- [mapToMachineEvent x] ]
       mbuilder = buildStackSamples $ validates (refsMachine cfg) mevents
   when (null mevents) $ do
     hPutStrLn stderr "There are no JNI events in the eventlog."
     exitFailure
   case mbuilder of
     Just builder -> Builder.hPutBuilder hSamples builder
     Nothing -> do
       hPutStrLn stderr "There were errors during the analysis."
       exitFailure

-- | Produces stack samples from the simulated JNI states.
buildStackSamples :: [Either (JNIState, MachineEvent) JNIState] -> Maybe Builder
buildStackSamples = go Map.empty
  where
    -- Accumulates stack samples on the first parameter.
    --
    -- If there are two stack samples, we want to keep the one with the
    -- highest reference count. Thus, if there is a leak, it is readily
    -- visible.
    go :: Map LBS.ByteString Int
       -> [Either (JNIState, MachineEvent) JNIState]
       -> Maybe Builder
    -- All the samples have been collected. Produce the output.
    go samples [] = Just $ foldr
       (\(bs, mx) b -> mconcat
         [ Builder.lazyByteString bs
         , Builder.char8 ' '
         , Builder.intDec mx
         , Builder.char8 '\n'
         , b
         ]
       )
       mempty $
       sortOn (negate . snd) $ Map.toList samples
    go _ (Left (s, _) : _) =
      maybe (error "bug in the analyzer: invalid state") (const Nothing)
            $ jnisMsg s
    go !samples (Right s : ss) = reportMessages s $ case jnisStackSample s of
      -- Skip the state if it produced no sample.
      Nothing -> go samples ss
      Just (sts, mx) ->
        let -- produce a call stack in bytestring form
            cs = showStacks sts
            samples' = Map.insertWith max cs mx samples
         in go samples' ss

    reportMessages :: JNIState -> a -> a
    reportMessages s a = case jnisMsg s of
      Nothing -> a
      Just (msg, _) -> unsafePerformIO $ hPutStrLn stderr msg >> return a

showStacks :: [[StackFrameInfo]] -> LBS.ByteString
showStacks =
    Builder.toLazyByteString
    . mconcat
    . intersperse (Builder.char8 ';')
    . builderSampleStack
    . dropCommonPrefixes . reverse . map reverse
  where
    -- Creates a builder for each call stack in a list of call stacks.
    -- The list of call stacks identifies a frame. The first call stack
    -- gives the location of the code which produced the top-most frame,
    -- and the next call stacks give the locations where the descendant
    -- frames were created.
    --
    -- On the output, one builder is produced for each call stack.
    builderSampleStack :: [[StackFrameInfo]] -> [Builder]
    builderSampleStack [] = []
    -- The topmost frame sometimes may appear as an empty call stack.
    -- We just skip it to avoid producing empty cells in the flame graph.
    builderSampleStack ([] : sts) = builderSampleStack sts
    builderSampleStack (st : sts) =
       mconcat
         (intersperse (Builder.string8 "::") $ map builderStackFrameInfo st)
       : builderSampleStack sts

    builderStackFrameInfo :: StackFrameInfo -> Builder
    builderStackFrameInfo (SFI sfiFunction sfiFile sfiLine sfiCol) = mconcat
      [ Builder.stringUtf8 sfiFile
      , Builder.char8 ':'
      , Builder.intDec sfiLine
      , Builder.char8 ':'
      , Builder.intDec sfiCol
      , Builder.char8 ':'
      , Builder.stringUtf8 sfiFunction
      ]

-- | @zs ++ dropCommonPrefix xs ys == ys@ where @zs@ is the largest common
-- prefix of @xs@ and @ys@.
dropCommonPrefix :: [StackFrameInfo] -> [StackFrameInfo] -> [StackFrameInfo]
dropCommonPrefix xs0 xs1 =
    let go (SFI _ sfiFile0 sfiLine0 sfiCol0 : xss0)
           (SFI _ sfiFile1 sfiLine1 sfiCol1 : xss1)
          | sfiFile0 == sfiFile1
            && sfiLine0 == sfiLine1
            && sfiCol0 == sfiCol1   = go xss0 xss1
        go _ xss1 = xss1
     in go xs0 xs1

-- Because the call stacks might contain redundant prefixes, this
-- function attempts to remove the redundancy by comparing each
-- call stack with the precedent one and strips the prefix if shared.
dropCommonPrefixes :: [[StackFrameInfo]] -> [[StackFrameInfo]]
dropCommonPrefixes xs = zipWith dropCommonPrefix ([] : xs) xs

-- | Events which are interesting to the 'refsMachine'.
--
-- The event also tells on which OS thread the event happened.
data MachineEvent = MachineEvent KernelThreadId JNIEvent

-- | Selects the interesting events for the 'refsMachine'.
mapToMachineEvent :: HasCallStack => Event -> Maybe MachineEvent
mapToMachineEvent = maybe Nothing parseMachineEvent . jniEventMaybe

-- | Tells if an event is a JNI event.
jniEventMaybe :: Event -> Maybe String
jniEventMaybe e = case evSpec e of
    UserMessage ('[' : 'j' : 'n' : 'i' : ']' : ' ' : msg)
      -> Just msg
    _ -> Nothing

-- | Parses a machine event from a string of the form @"threadId <JNIEvent>"@.
parseMachineEvent :: HasCallStack => String -> Maybe MachineEvent
parseMachineEvent msg = case reads msg of
    (kTid, rest) : _ -> case reads rest of
      (ev, _) : _ -> Just $ MachineEvent (KernelThreadId kTid) ev
      _ -> error $ "Could not read JNIEvent: " ++ show msg
    _ -> error $ "Could not read JNIEvent: " ++ show msg


data JNIState = JNIState
    { -- | For each thread there is a stack frame with local refs,
      -- the call stack where the frame was created, and the maximum
      -- amount of refs created in a given frame.
      jnisLocalRefs :: !(Map KernelThreadId [StackFrameState])
      -- | A message from the analysis (msg, isFatal)
    , jnisMsg :: Maybe (String, Bool)
    -- | A stack sample produced by the last step
    , jnisStackSample :: Maybe ([[StackFrameInfo]], Int)
    }

-- | State of a stack frame
data StackFrameState = StackFrameState
    { -- | Set of local references of the frame.
      -- It may contain the call stacks at the time the local
      -- references were created.
      sfsLocalRefs :: !(Map IntPtr (Maybe [StackFrameInfo]))
      -- | Source location where of the frame was produced
    , sfsFrameInfo :: [StackFrameInfo]
      -- | Peak amount of local refs in the frame
    , sfsPeakLocalRefs :: !Int
      -- | Initial frame capacity
    , sfsCapacity :: Maybe Int32
    }

-- | A machine to simulate creation of local refs
refsMachine :: Config -> Machine JNIState MachineEvent
refsMachine Config {..} = Machine
    { initial = JNIState Map.empty Nothing Nothing
    , alpha = const True
    , final = const False
    , delta = step
    }
  where
    step :: JNIState -> MachineEvent -> Maybe JNIState
    step s0@(JNIState {..}) (MachineEvent kTid ev) =
      let -- We reset earlier stack samples here.
          s1 = s0 { jnisStackSample = Nothing }
       in case jnisMsg of
      -- The current state has an error message. Stop processing events.
      Just (_, True) -> Nothing
      _ -> let s = s1 { jnisMsg = Nothing }
            -- Upon receiving a JNI event, get or initialize the analyzer state
            -- for the corresponding thread.
            in case Map.lookup kTid jnisLocalRefs of
            { Nothing -> return []
            ; Just xs -> return xs
            } >>= \xs -> case ev of
          -- Insert a local ref in the state.
          NLR ptr st -> insertRef s kTid ptr st xs
          -- Delete the local ref from the state.
          DLR ptr -> do
            -- The reference may not be in the topmost frame, so we need delete
            -- from the frame where it belongs.
            let deletePtr [] = []
                deletePtr (sfs@StackFrameState {..} : xss) =
                  if Map.member ptr sfsLocalRefs then
                    sfs { sfsLocalRefs = Map.delete ptr sfsLocalRefs } : xss
                  else sfs : deletePtr xss
            Just s { jnisLocalRefs =
                       Map.insert kTid (deletePtr xs) jnisLocalRefs }
          -- Create a new frame.
          PushLF mcap st -> do
            let s' = s { jnisLocalRefs =
                           Map.insert kTid (StackFrameState (Map.empty)
                                              st 0 mcap : xs)
                                      jnisLocalRefs
                       }
            -- Only report the first frame
            if length xs >= cfgMaxStackDepth then
              mkFatalError s $ concat
                [ "Too many stack frames (--max-stack-depth "
                , show cfgMaxStackDepth, ") at\n", showStacksString [st]
                ]
            else
              Just s'
          -- Delete a frame.
          PopLF ptr st -> case xs of
            [] -> mkReport s $ "popLocalFrame has no matching pushLocalFrame:\n"
                               ++ showStacksString [st]
            sfs : xss -> do
              -- We record a stack sample of the frame before deleting it.
              let s' = s { jnisStackSample =
                        Just ( [ sfsFrameInfo sfsi | sfsi <- xs ]
                             , sfsPeakLocalRefs sfs
                             )
                       }
              insertRef s' kTid ptr (Just st) xss

    mkFatalError s msg = Just s { jnisMsg = Just (msg, True) }
    mkReport s msg = Just s { jnisMsg = Just (msg, cfgReportsAsErrors) }

    -- Inserts a reference in the given state for the given OS thread.
    insertRef :: JNIState
              -> KernelThreadId
              -> IntPtr
              -> Maybe [StackFrameInfo]
              -> [StackFrameState]
              -> Maybe JNIState
    insertRef s kTid 0 _ xss =
      Just s { jnisLocalRefs = Map.insert kTid xss (jnisLocalRefs s) }
    insertRef s kTid ptr st [] = Just s { jnisLocalRefs =
      Map.insert kTid [StackFrameState (Map.singleton ptr st) [] 1 Nothing]
                 (jnisLocalRefs s) }
    insertRef s kTid ptr st xs@(sfs : xss) = do
      let szx = Map.size (sfsLocalRefs sfs)
          s' = s { jnisLocalRefs = Map.insert kTid
                   (sfs { sfsLocalRefs = Map.insert ptr st (sfsLocalRefs sfs)
                        , sfsPeakLocalRefs =
                            max (sfsPeakLocalRefs sfs) (1 + szx)
                        }
                   : xss
                   )
                   (jnisLocalRefs s)
                 }
          localRefs =
            zipWith (\i sts -> show (i :: Int) ++ ": " ++ sts)
                    [1..]
                    [ showStacksString [refst]
                    | Just refst <- Map.elems (sfsLocalRefs sfs)
                    ]
          reportRefs =
            if null localRefs then
              [ "Rerun the traced program with " ++
                "HASKELL_JNI_TRACE=localrefs.callstacks"
              , "to see where local references were created."
              ]
            else "local references: " : localRefs
      if szx < cfgMaxLocalRefs then
        if cfgCheckFrameCapacities then
           case sfsCapacity sfs of
             Just cap
                 -- Report the error the first time the bound is reached.
               | szx == fromIntegral cap ->
                 mkReport s' $ unlines $
                   ("Initial capacity exceeded (--no-frame-capacities)"
                   ++ " at the stack frame created at")
                   : showStacksString (map sfsFrameInfo xs) : reportRefs
               | szx > fromIntegral cap ->
                 -- Don't grow the state to have a better chance to finish the
                 -- analysis
                 Just s
             _ ->
               Just s'
        else
          Just s'
      else if szx == cfgMaxLocalRefs then
        mkReport s' $ unlines $ concat
          [ "Too many references (--max-local-refs ", show cfgMaxLocalRefs
          , ") at the stack frame created at"
          ] : showStacksString (map sfsFrameInfo xs) : reportRefs
      else
        -- Don't grow the state to have a better chance to finish the
        -- analysis.
        Just s

    showStacksString = LText.unpack . LText.decodeUtf8 . showStacks

-- | @sortUserEvents events f@ sorts the user events in @events@
-- and passes them to @f@. This function runs in bounded memory.
--
-- The events are sent to different temporary files, each file has
-- the events corresponding to a given capability. Then @f@ is given
-- the result of merging the files.
--
-- @f@ must not use the sorted events after it completes as the
-- temporary files are removed and the list of events is produced
-- lazily.
sortUserEvents :: [Event] -> ([Event] -> IO a) -> IO a
sortUserEvents events f =
    withSystemTempDirectory "jni-analyze" $ \dir -> do
      m <- splitEvents dir IM.empty events
      forM_ m hClose
      mergeEvents dir (IM.keys m) >>= f
  where
    capFile :: FilePath -> Int -> FilePath
    capFile dir capId = dir </> (show capId ++ ".eventlog")

    splitEvents :: FilePath -> IM.IntMap Handle -> [Event]
                -> IO (IM.IntMap Handle)
    splitEvents _ m [] = return m
    splitEvents dir m evs@(e : _) =
      let evCapId = maybe 0 (+1) . evCap
          capId = evCapId e
       in case IM.lookup capId m of
         -- Create the temporary capability file.
         Nothing -> do
           h <- openFile (capFile dir capId) WriteMode
           hSetBuffering h (BlockBuffering Nothing)
           splitEvents dir (IM.insert capId h m) evs
         -- Send the events to the capability file.
         Just h ->
           doWhile (\ev -> capId == evCapId ev)
                   (\ev -> case evSpec ev of
                      UserMessage msg ->
                        hPutStrLn h (show (evTime ev) ++ " " ++ msg)
                      _ -> return ()
                   )
                   evs
            >>=
              splitEvents dir m

    doWhile :: (a -> Bool) -> (a -> IO ()) -> [a] -> IO [a]
    doWhile _ _ [] = return []
    doWhile p fn xs@(x : xs')
         | p x = fn x >> doWhile p fn xs'
         | otherwise = return xs

    mergeEvents :: FilePath -> [Int] -> IO [Event]
    mergeEvents dir ids = do
      evss <- forM ids $ \capId -> do
        t <- readFile (capFile dir capId)
        return [ Event tt (UserMessage $ drop 1 rest) cap
               | line <- lines t
               , let (tt, rest) = case reads line of
                       r : _ -> r
                       _ -> error $ "cannot read timestamp: " ++ show line
               , let cap = if capId > 0 then Just (capId - 1) else Nothing
               ]
      let mergeAll [] = []
          mergeAll [xs] = xs
          mergeAll xs = mergeAll (mergePairs xs)
          mergePairs (x : y : xss)  = merge x y : mergePairs xss
          mergePairs xs = xs
          merge (x : xs) (y : ys) =
            if evTime x <= evTime y then x : y : merge xs ys
              else y : x : merge xs ys
          merge [] ys = ys
          merge xs [] = xs
      return (mergeAll evss)
