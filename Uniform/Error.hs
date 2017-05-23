-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Error
--
-- | a miniaml set of error processing
-- uses monads-tf  -- family used (not fp)
-- and other monads often used (state)
-- collects from eithererror package what is working (with monads-tf)
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Error (module Uniform.Error
    , module Uniform.Strings
    , module Safe
    , module Control.Monad.Error  -- is monads-tf
    , module Control.Exception   -- to avoid control.error
--     , ErrOrVal
--          , ErrIO, ErrorT (..)
--          , MonadError (..)
--          , module Safe
--          , liftIO
-- --         , callErrIO
--          , callIO
--          , maybe2error
-- --        , callx   -- for fileio
-- --         , signal  -- only for ErrIO
-- --         , makeSignal
--          , throwError, catchError, bracketErrIO
--          , SomeException
--          -- but catch works only for errors thrown here, not for general errors/exceptions
-- --        , ErrorT (..)  -- , runErrorT included
--         , runErr
--         , throwErrorT
--         , errorT, errorWords
--         , fromJustNoteT,  headNoteT
--
--         , undef
-- --        , throwErrorWords
-- --        , lift
-- --        , Identity (..)
-- --        , MonadState (..), StateT (..)
-- --        , MonadError (..)
-- --        , execState, execStateT, evalState, evalStateT
-- --        , MonadIO (..) -- , liftIO  included
--         , unless, when
-- --        , ErrOps3 (..)
-- ----        , ErrOps4 (..)
-- --        , runErrFail
--         , Musts (..)
-- --         N.throwM, N.SomeException, N.MonadCatch
-- --        , SomethingBad (..)
-- --        , callErrIO2, callIO2, throwErrorWords2
-- --        , callErrIO3
-- --        , catchAll
--     , htf_thisModulesTests
        )  where

import           "monads-tf" Control.Monad.Error
    -- hiding (catchError)
--    (Error (..), ErrorT (..), MonadError(..), MonadIO (..)
--        , unless, when )

--import qualified Control.Monad.Error (catchError)
-- import Control.Monad.Error
import           Safe
import           Test.Framework
import           Uniform.Strings
--import Control.Exception (SomeException (..))
--import Control.Monad.Catch

import Control.Exception
--    (catch, SomeException, mask
--        , onException, displayException)
--import Control.Exception.Monadic
instance CharChains2 IOError Text where
    show' = s2t . show

type ErrOrVal = Either Text

type ErrIO  = ErrorT Text IO
-- an instance of Control.Monad.Error for ErrIO is automatic

--catchError :: (ErrIO a) -> ErrIO a -> ErrIO a
---- | redefine catchError - the definition in monads-tf seems broken
--catchError = catch


-- | runErr to avoid the depreceated message for runErrorT, which is identical
runErr :: ErrIO a -> IO (ErrOrVal a)
runErr = runErrorT
--
undef :: Text -> a
undef = error . t2s
-- ^ for type specification, not to be evaluated

fromRight :: ErrOrVal a -> a
fromRight (Right a) = a
fromRight (Left msg) = errorT ["fromright", msg]

bracketErrIO
        ::
            ErrIO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> ErrIO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> ErrIO c)  -- ^ computation to run in-between
        -> ErrIO  c          -- returns the value from the in-between computation
--bracketErrIO before after thing = bracket before after thing
-- no way to catch IO errors reliably in ErrIO -- missing Monad Mask or similar
bracketErrIO before after thing =  (fmap fromRight) .  callIO $
    bracket
        (do
            ra <- runErr $ before
            return ra) --  (ra :: ErrOrVal a) )
        (\a -> runErr $ after  . fromRight  $ a )
        (\a -> runErr $ thing . fromRight  $ a)

--        ra <- before
--        rc <- thing ra
--        return rc
--    `catchError` \e -> do
--                putIOwordsT ["bracketErrIO caught - release resource"]
--                rb <- after ra
--                throwError e
--                return rc
--
--  mask $ \restore -> do
--    a <- before
--    r <- restore ( thing a) `onExceptionErrIO` after a
--    _ <- after a
--    return r
--
---- | Like 'finally', but only performs the final action if there was an
---- exception raised by the computation.
--onExceptionErrIO :: ErrIO a -> ErrIO b -> ErrIO a
--onExceptionErrIO io what =
--            io
--        `catchError` \e -> do
--                            _ <- what
--                            throwError e
--       -- the exception is ot ErrorType  $ s2t .  displayException   $ (e :: SomeException)


--catchT :: (MonadError m, ErrorType m ~ Text) => m a -> (Text -> m a) -> m a
--catchT p f = do
--                  p
--                `catch` \(e::SomeException) -> f (showT e)

instance Error Text where
-- noMsg = Left ""
-- strMsg s = Left s

--callIO ::  (MonadError m, MonadIO m, ErrorType m ~ Text) => IO a -> m a
-- this is using now catch to grab all errors
callIO op = do
        r2 <- liftIO $ do
                    r <- op
                    return $ Right r
                `catch` (\e -> return . Left $  (e::SomeException))
        case r2 of
            Left e -> throwError (showT e)
            Right v -> return v
--

throwErrorT :: [Text] -> ErrIO a
-- throw an error with a list of texts as a text
throwErrorT = throwError . unwordsT

maybe2error :: Maybe a -> ErrIO a
maybe2error Nothing  = fail "was Nothing"
maybe2error (Just a) = return a

errorT :: [Text] ->  a
-- ^ a list of texts is output with failure
errorT  = error . t2s . unwordsT
errorWords = errorT

fromJustNoteT :: [Text] -> Maybe a -> a
-- produce error with msg when Nothing, msg is list of texts
fromJustNoteT msgs a = fromJustNote (t2s . unlinesT $ msgs) a

headNoteT :: [Text] -> [a] -> a
-- get head with a list of texts
headNoteT msg s = headNote (t2s $ unwords' msg) s

class (MonadError m) => Musts  m where
    mustFail:: Text -> f -> m Bool
    mustFailIO :: Text -> m () -> m Bool
    mustFailM :: Text -> m a -> m Bool
    mustSucceed :: Text -> Bool -> m Bool
    -- throws error if not True
    mustSucceedIO:: Text -> m () -> m Bool
    mustSucceedM :: Text -> m a -> m Bool
--    mustFailIOval :: Text -> m a -> m Bool
    mustReturnTrueB
        , mustReturnFalseB :: Text -> m Bool -> m Bool
    mustReturnValueMB :: Eq v => Text -> v -> m v -> m Bool
    mustReturnValueErr :: (Eq v, Show v) => Text -> v -> v -> m Bool



instance (MonadError m, MonadIO m, Show (ErrorType m)
            , m ~ ErrorT Text IO)
                => Musts m where
    mustFail st op = do
            let !a = op
            throwErrorT   ["should fail!", st]
            return False
        `catchError` \s -> do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - expected",   s]
                    return True


    mustFailIO st op = do
            op
            return False
        `catchError` \s ->  do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail expected  ()",   s]
                    return True

    mustFailM st op = do
            op
            return False
        `catchError` \s ->  do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail expected  ok",   s]
                    return True
    mustSucceed st op = do
            let !a = op
            if a then return a else throwErrorT [st, "did return False"]
        `catchError` \s -> do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - not expected",   s]
                    return False

    mustSucceedIO st op = do
            a <-  op
            return True
        `catchError` \s -> do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - not expected",   s]
                    return True

    mustSucceedM st op = do
            op
            return True
        `catchError` \s ->  do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - not expected",   s]
                    return False

    mustReturnTrueB st op = do
        t <- op
        unless t $ putIOwords [st, "error - ", st]
        return t

    mustReturnFalseB st op = do
        t <- op
        when t $ putIOwords [st, "error - ", st]
        return (not t)

    mustReturnValueMB st v op = do
        t <- op
        unless (t==v) $ putIOwords [st, "error - ", st]
        return (t==v)

    mustReturnValueErr st v op = do
        let !t = op
        unless (t==v) $ do
                    putIOwords [st, "error - ", showT t, "expected", showT v]
                    throwErrorT [st, "error - ", showT t, "expected", showT v]
        return (t==v)

test_catch =
            error "some intentional error"
       `catch` \(e::SomeException) -> assertBool True

--test_catch_error =
--            error "some intentional error"
--       `catchT` \(e :: S) -> assertBool True


test_callIO = do
        r <- runErr $ callIO $ readFile "xxxabc"
        case r of
            Left _ -> assertBool True
            Right _ -> assertBool False

--test_catch2 =
--            readFile2 "xxxabc"
--       `catch` \(e::SomeException) -> assertBool True
--
--test_catch_error2 =
--            readFile2 "xxxabcd"
--       `catchError ` \(e) -> assertBool True

--
--data SomethingBad   = SomethingBad [Text]
--    deriving D.Typeable
--unSMB (SomethingBad ss) = ss
--
--instance Show SomethingBad where
--    show (SomethingBad s) = "something bad happened:" ++ unwords s
--instance N.Exception SomethingBad
--
--throwErrorWords2 :: (N.MonadCatch m) => [Text] -> m a
--throwErrorWords2 s = N.throwM (SomethingBad s)
--
---- {-# DEPRECATED  throwErrorWords   "replace with class  throwErrorWords2 " #-}
---- the constraint MonadCatch is not possible for em
--
--throwErrorWords :: (MonadError  m, ErrorType m ~ Text)  => [Text] -> m a
--throwErrorWords s = throwError .unwords $ s   -- use only this!!
--
--
----------------------------
--class (-- MonadError m
--      Show (ErrMonadTypeF m)
--    ) =>
--        ErrOps3 m   where
--    type ErrMonadF m ::  *   -> *
--    type ErrMonadTypeF m
--    m2em :: m a -> ErrMonadF m  a  -- is callIO
--        -- was callX
--    em2m :: (ErrMonadF m ) a -> m (ErrOrVal a) -- was getErr, is runErr
--
--    -- | to call an operation in the ErrIO monad  returns ErrOrVal
--    rep1 :: MonadIO m => ErrIO a -> m (ErrOrVal a)
--    -- | call and report error in stdout
--    rep2 :: (Zeros a, MonadIO m) => Text -> ErrIO a -> m a
--    -- | call and report error on stdout  (without zeros constraint)
--    rep3 :: ( MonadIO m) => Text -> a -> ErrIO a -> m a
--
--    rep1   =  liftIO . runErr  -- runExceptT . runErrIO
--    rep2 s op = do
--        r <- rep1 op
--        case r of
--                    Left msg -> do
--                                     putIOwords ["rep2 - appop3 eres1", s, "message", msg]
--                                     return zero
--                    Right i -> return i
--    rep3 s defval op = do
--        r <- rep1 op
--        case r of
--                    Left msg -> do
--                                     putIOwords ["appop3 eres1", s, "message", msg]
--                                     return defval
--                    Right i -> return i
--
--    -- | to run an ErrIO op and fail (throw someerror )
--    rep2e ::  Text -> ErrIO a -> m a
--
--instance ErrOps3 IO where
--    type  ErrMonadF   IO  = ErrorT Text IO
--    type ErrMonadTypeF IO = Text
--    m2em = callIO2
--    em2m = callErrIO2
--
--    --rep1 ::  ErrIO a -> m (ErrOrVal a)
--    rep1 op = liftIO $ callErrIO2 op
--
--    rep2e st op = callErrIO3 st op
--
------
------ | call and report error  (without zeros constraint) and complete with op2
----rep4 :: ( MonadIO m) => Text ->   ErrIO a -> (a -> m ()) -> ( Text -> m ()) -> m ()
----rep4 s   op op2 report = do
----    r <- rep1 op
----    case r of
----                Left msg -> report (s ++ msg)
----                Right i -> op2 i
--
---------------- the single calls -- better naming?? TODO
---- | call an ErrIO operation and return Either type (catch all errors!)
---- essentially try ?
--
--callErrIO2 :: ErrIO a  -> IO (Either Text a)
--callErrIO2 op = do
--        res <- runErrorT  op
--        return res
--  `N.catchAll` \s -> return . Left . show $ s
--
--catchAll = N.catchAll
--
---- | call an ErrIO operation and return value or throw error
--callErrIO3 :: Text -> ErrIO a  -> IO a
--callErrIO3 st op = do
--        res <- runErrorT  op
--        case res of
--            Left msg -> do
--                    putIOwords ["callErrIO - error in call", msg]
--                    throwErrorWords2 ["error in ErrIO op", msg]
--            Right s -> return s
--  `N.catchAll` \s -> throwErrorWords2 [st, " --",   show s ]
----- | call an io operation and catch the error
---- used in fileio, what is difference to liftIO
---- returns the error message into left (liftIO passes error)
----callIO = callx
--
----callIO2 = callx
--callIO2 :: IO a -> ErrIO a
--callIO2 op = do
--    r <- liftIO op
--    return r
-- `N.catchAll` (\s -> do
--                    putIOwords ["callIO2 catchAll", show s]
--                    fail  . show $ s
--                    )
--
---- | call an ErrIO operation and stop (passing an undefined value as return
---- stops suddendly!
--callErrIO :: ErrIO a  -> IO a
--callErrIO op = do
--    res <- runErrorT  op
--    case res of
--        Left msg -> do
--                putIOwords ["callErrIO - error in call", msg]
--                return (undefined)
--        Right s -> return s
--
--{-# DEPRECATED callErrIO, callIO, callx  "replace with class callErrIO2 callIO2 or em2m m2em" #-}
--
--
--
--callx :: (N.MonadCatch m, MonadIO m) =>  m a -> ErrorT Text m a
--callx op = do
--        res <- lift op
--        return res
--   `N.catchAll` \(e ) ->   N.throwM (SomethingBad ([show e])) -- throwError e
--
--
--
--
--{-# DEPRECATED signal, makeSignal, runErrFail
--         "replace ..." #-}
---- to signal an error
--signal  :: (Show e) => e -> [Text] -> ErrIO a
--signal s t = throwError  $ makeSignal s t
--
---- | to produce a single string from an  list of strings for a signal
--makeSignal :: Show a => a -> [Text] ->  Text
--makeSignal et txts = unwords $ (show et) : txts
--
---- | to finalize a error from ErrIO into the IO monad
---- does not catch other exceptions, only the left value
--runErrFail :: Text -> ErrIO a -> IO a
--runErrFail msg op = do
--    er <- runErrorT op
--    either (fail $ "runErrFail " ++ msg) (return) er
--


{-

class (Monad ( ErrMonadF m), Monad m) => ErrOps3 m   where
    type ErrMonadF  m ::  * -> *
    callIO, m2em :: m a -> (ErrMonadF m ) a
        -- was callX
    m2em = callIO
    runErr, em2m :: (ErrMonadF m ) a -> m (ErrOrVal a) -- was getErr
    em2m = runErr
--    signal  :: (Show e) => e -> [Text] -> (ErrMonadF m ) a
 --    errio2meov :: ErrIO a -> m (ErrOrVal a)
--    lift_err2meov :: (ErrMonadF m) a -> m (ErrOrVal a)
--    call2IOeov :: m a -> IO (ErrOrVal a)
--    io2errm,  liftIOEF :: IO a -> (ErrMonadF m) a  -- liftio in the errmonad
--    liftIOF :: IO a -> m a  -- the ordinary lift io
    -- | get the result from a computation into the IO monad
--    err2m :: (ErrMonadF m) a -> m a
--    errio2m :: ErrIO a -> m a
--    errio2errm :: ErrIO a -> (ErrMonadF m) a  -- problematic, leads to ambiguous type

class ErrOps4 em where
    type MonadF em :: * -> *
    fm2m :: em a -> (MonadF em) (ErrOrVal a)
    m2fm :: (MonadF em) a -> em a
    errio2fm :: ErrIO a -> em a
    io2fm :: IO a -> em a
-}
