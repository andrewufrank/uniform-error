----------------------------------------------------------------------
--
-- Module      :  Uniform.NoticeLevel
--              a simplistic idea to control the amount of output 
--              helping with debug 
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
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}

module  Uniform.NoticeLevel
  ( module Uniform.NoticeLevel
      )
where

 
import Uniform.Strings hiding (S, (<.>), (</>))
import Data.Default
import GHC.Generics

data NoticeLevel = NoticeLevel0 | NoticeLevel1 | NoticeLevel2 deriving (Eq, Ord, Show, Read, Generic)
instance Zeros NoticeLevel where zero = NoticeLevel0 
instance Default NoticeLevel where 
        def = NoticeLevel2 

inform, informNone, informAll :: NoticeLevel -> Bool
inform =  not . isZero
informNone = const False  -- to use with: when (informNone debug)
informAll = const True 