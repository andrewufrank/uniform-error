----------------------------------------------------------------------
--
-- Module      :  Uniform.Error
--
----------------------------------------------------------------------

    {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}

module Uniform.Error
  ( module Uniform.Error,
    module Uniform.Strings,
    module Safe,
    module Control.Monad.Error, -- is monads-tf
    module Control.Exception, -- to avoid control.error
  )
where

import Control.Exception (Exception, SomeException, bracket, catch)
import "monads-tf" Control.Monad.Error (Error, ErrorT, ErrorType, MonadError, MonadIO, catchError, liftIO, runErrorT, throwError, unless, when)
import Safe (headNote)
import Uniform.Strings hiding (S, (<.>), (</>))

instance CharChains2 IOError Text where
  show' = s2t . show

type ErrOrVal = Either Text

type ErrIO = ErrorT Text IO

instance Exception [Text]


toErrOrVal :: Either String a -> ErrOrVal a
toErrOrVal (Left s) = Left (s2t s)
toErrOrVal (Right r) = Right r

-- | runErr to avoid the depreceated message for runErrorT, which is identical
runErr :: ErrIO a -> IO (ErrOrVal a)
runErr = runErrorT

runErrorVoid :: ErrIO () -> IO ()
-- ^ run an operation in ErrIO which is not returning anything
-- simpler to use than runErr
runErrorVoid a = do
  res <- runErr a
  case res of
    Left msg -> error (t2s msg)
    Right _ -> return ()

undef :: Text -> a
undef = error . t2s
-- ^ for type specification, not to be evaluated

fromRightEOV :: ErrOrVal a -> a
fromRightEOV (Right a) = a
fromRightEOV (Left msg) = errorT ["fromrightEOV", msg]

bracketErrIO ::
  -- | computation to run first (\"acquire resource\")
  ErrIO a ->
  -- | computation to run last (\"release resource\")
  (a -> ErrIO b) ->
  -- | computation to run in-between
  (a -> ErrIO c) ->
  ErrIO c -- returns the value from the in-between computation
  --bracketErrIO before after thing = bracket before after thing
  -- no way to catch IO errors reliably in ErrIO -- missing Monad Mask or similar
bracketErrIO before after thing =
  fmap fromRightEOV . callIO $
    bracket
      ( do
          ra <- runErr $ before
          return ra --  (ra :: ErrOrVal a) )
      )
      (\a -> runErr $ after . fromRightEOV $ a)
      (\a -> runErr $ thing . fromRightEOV $ a)

instance Error Text

callIO :: (MonadError m, MonadIO m, ErrorType m ~ Text) => IO a -> m a
-- | this is using catch to grab all errors
callIO op = do
  r2 <-
    liftIO $
      do
        r <- op
        return $ Right r
        `catch` ( \e -> do
                    --                         putStrLn "callIO catch caught error\n"
                    return . Left $ (e :: SomeException)
                )
  case r2 of
    Left e -> do
      --                        putIOwords ["\ncallIO Left branch\n", showT e, "throwError\n"]
      throwError (showT e)
    Right v -> return v


throwErrorT :: [Text] -> ErrIO a
-- throw an error with a list of texts as a text
throwErrorT = throwError . unwordsT

maybe2error :: Maybe a -> ErrIO a
maybe2error Nothing = fail "was Nothing"
maybe2error (Just a) = return a

errorT :: [Text] -> a
-- ^ a list of texts is output with failure
errorT = error . t2s . unwordsT

errorWords :: [Text] -> a
errorWords = errorT

fromJustNoteT :: [Text] -> Maybe a -> a
-- produce error with msg when Nothing, msg is list of texts
fromJustNoteT msgs a = fromJustNote (t2s . unlinesT $ msgs) a

fromRightNoteString :: Text -> Either String b -> b
-- produce an error when assuming that a value is Right
fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
fromRightNoteString _ (Right a) = a

fromRightNote :: Text -> Either Text b -> b
-- produce an error when assuming that a value is Right
fromRightNote msg (Left a) = errorT ["fromRight", showT a, msg]
fromRightNote _ (Right a) = a

headNoteT :: [Text] -> [a] -> a
-- get head with a list of texts
headNoteT msg s = headNote (t2s $ unwords' msg) s

startProg :: Show a => Text -> ErrIO a -> IO ()
startProg programName mainProg =
  do
    putIOwords
      [ "------------------ ",
        programName,
        " ----------------------------\n"
      ]
    r <- runErr $ mainProg
    putIOwords
      [ "\n------------------",
        "main",
        programName,
        "\nreturning",
        either id showT r,
        "\n"
      ]
    return ()
    `catchError` ( \e -> do
                     putIOwords
                       [ "startProg error caught\n",
                         programName,
                         "\n",
                         showT e
                       ]
                     return ()
                 )

