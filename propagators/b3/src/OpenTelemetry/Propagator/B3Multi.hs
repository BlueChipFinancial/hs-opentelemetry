{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  OpenTelemetry.Propagators.W3CTraceContext
-- Copyright   :  (c) Ian Duncan, 2021
-- License     :  BSD-3
-- Description :  Standardized trace context propagation format intended for HTTP headers
-- Maintainer  :  Ian Duncan
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions)
--
-- Distributed tracing is a methodology implemented by tracing tools to follow, analyze and debug a transaction across multiple software components. Typically, a distributed trace traverses more than one component which requires it to be uniquely identifiable across all participating systems. Trace context propagation passes along this unique identification. Today, trace context propagation is implemented individually by each tracing vendor. In multi-vendor environments, this causes interoperability problems, like:
--
-- - Traces that are collected by different tracing vendors cannot be correlated as there is no shared unique identifier.
-- - Traces that cross boundaries between different tracing vendors can not be propagated as there is no uniformly agreed set of identification that is forwarded.
-- - Vendor specific metadata might be dropped by intermediaries.
-- - Cloud platform vendors, intermediaries and service providers, cannot guarantee to support trace context propagation as there is no standard to follow.
-- - In the past, these problems did not have a significant impact as most applications were monitored by a single tracing vendor and stayed within the boundaries of a single platform provider. Today, an increasing number of applications are highly distributed and leverage multiple middleware services and cloud platforms.
--
-- - This transformation of modern applications calls for a distributed tracing context propagation standard.
--
-- This module therefore provides support for tracing context propagation in accordance with the multi-header B3 tracing context
-- propagation specifications: https://www.w3.org/TR/trace-context/
module OpenTelemetry.Propagator.B3Multi where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Word (Word8)
import Network.HTTP.Types (RequestHeaders, ResponseHeaders)
import qualified OpenTelemetry.Context as Ctxt
import OpenTelemetry.Propagator (Propagator (..))
import OpenTelemetry.Trace.Core
  ( Span,
    SpanContext (..),
    TraceFlags,
    defaultTraceFlags,
    getSpanContext,
    isSampled,
    setSampled,
    unsetSampled,
    wrapSpanContext,
  )
import OpenTelemetry.Trace.Id (Base (..), SpanId, TraceId, baseEncodedToSpanId, baseEncodedToTraceId, spanIdBaseEncodedByteString, traceIdBaseEncodedByteString)
import OpenTelemetry.Trace.TraceState (empty)
import Prelude hiding (takeWhile)

data TraceParent = TraceParent
  { version :: {-# UNPACK #-} !Word8,
    traceId :: {-# UNPACK #-} !TraceId,
    parentId :: {-# UNPACK #-} !SpanId,
    traceFlags :: {-# UNPACK #-} !TraceFlags
  }
  deriving (Show)

data B3Info = B3Info
  { traceIdB :: TraceId,
    spanIdB :: SpanId,
    parentSpanId :: Maybe SpanId,
    sampled :: Maybe ByteString
  }

-- | Attempt to decode a 'SpanContext' from B3 Headers
--
-- @since 0.0.1.0
decodeSpanContext ::
  B3Info ->
  Maybe SpanContext
decodeSpanContext (B3Info traceId' spanId' _ sampled) = do
  -- Sample if Debug, or if we don't have information yet
  let flags = (if sampled == Just "0" then unsetSampled else setSampled) defaultTraceFlags
  pure $
    SpanContext
      { traceFlags = flags,
        isRemote = True,
        traceId = traceId',
        spanId = spanId',
        traceState = empty
      }

decodeB3Headers :: RequestHeaders -> Maybe B3Info
decodeB3Headers hs = do
  traceId' <- decode baseEncodedToTraceId =<< Prelude.lookup "X-B3-TraceId" hs
  spanId' <- decode baseEncodedToSpanId =<< Prelude.lookup "X-B3-SpanId" hs
  parentSpanId' <- decode baseEncodedToSpanId <$> Prelude.lookup "X-B3-ParentSpanId" hs
  let sampled = Prelude.lookup "X-B3-Sampled" hs
  pure $ B3Info traceId' spanId' parentSpanId' sampled
  where
    decode :: (Base -> ByteString -> Either String a) -> ByteString -> Maybe a
    decode decoder = either (const Nothing) pure . decoder Base16

-- | Encoded the given 'Span' into a @traceparent@, @tracestate@ tuple.
--
-- @since 0.0.1.0
encodeSpanContext :: Span -> IO [(CI ByteString, ByteString)]
encodeSpanContext s = do
  SpanContext {..} <- getSpanContext s
  let tags =
        [ ("TraceId", traceIdBaseEncodedByteString Base16 traceId),
          ("SpanId", spanIdBaseEncodedByteString Base16 spanId),
          ("Sampled", if isSampled traceFlags then "1" else "0")
        ]

  pure $ map (first tag) tags
  where
    tag :: ByteString -> CI ByteString
    tag = CI.mk . ("X-B3-" <>)

-- | Propagate trace context information via headers using the b3 specification format
--
-- @since 0.0.1.0
b3MultiPropagator :: Propagator Ctxt.Context RequestHeaders ResponseHeaders
b3MultiPropagator = Propagator {..}
  where
    propagatorNames = ["b3multi"]

    extractor hs c = do
      let mspanContext = decodeSpanContext =<< decodeB3Headers hs
      pure $! case mspanContext of
        Nothing -> c
        Just s -> Ctxt.insertSpan (wrapSpanContext s) c

    injector c hs = case Ctxt.lookupSpan c of
      Nothing -> pure hs
      Just s -> do
        tags <- encodeSpanContext s
        pure (tags <> hs)
