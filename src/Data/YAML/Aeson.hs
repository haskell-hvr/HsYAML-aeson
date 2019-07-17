{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy     #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2018
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- The [YAML 1.2](https://yaml.org/spec/1.2/spec.html) format provides
-- a much richer data-model and feature-set
-- than the [JavaScript Object Notation (JSON)](https://tools.ietf.org/html/rfc7159) format.
-- However, sometimes it's desirable to ignore the extra capabilities
-- and treat YAML as if it was merely a more convenient markup format
-- for humans to write JSON data. To this end this module provides a
-- compatibility layer atop "Data.YAML" which allows decoding YAML
-- documents in the more limited JSON data-model while also providing
-- convenience by reusing @aeson@'s 'FromJSON' instances for decoding
-- the YAML data into native Haskell data types.
--
module Data.YAML.Aeson
    ( -- * Parsing YAML using JSON models
      -- ** High-level parsing/decoding via 'FromJSON' instances
      Data.YAML.Aeson.decode1
    , decode1'
      -- ** Parsing into JSON AST ('J.Value')
    , decodeValue
    , decodeValue'
    , scalarToValue
    ) where

import           Control.Applicative    as Ap
import           Control.Monad.Identity (runIdentity)
import           Data.Aeson             as J
import qualified Data.Aeson.Types       as J
import qualified Data.ByteString.Lazy   as BS.L
import           Data.Text              (Text)
import qualified Data.Vector            as V
import           Data.YAML              as Y
import           Data.YAML.Event        (Pos)

-- | Parse a single YAML document using the 'coreSchemaResolver' and decode to Haskell types using 'FromJSON' instances.
--
-- This operation will fail if the YAML stream does not contain
-- exactly one YAML document. This operation is designed to be the
-- moral equivalent of @aeson@'s 'eitherDecode' function.
--
-- See 'decodeValue' for more information about this functions' YAML
-- decoder configuration.
--
decode1 :: FromJSON v => BS.L.ByteString -> Either String v
decode1 bs = case decodeValue bs of
  Left (_ ,err) -> Left err
  Right vs -> case vs of
    [] -> Left "No documents found in YAML stream"
    (_:_:_) -> Left "Multiple documents encountered in YAML stream"
    [v1] -> do
      case J.fromJSON v1 of
        J.Success v2 -> Right $! v2
        J.Error err  -> Left ("fromJSON: " ++ err)

-- | Variant of 'decode1' allowing for customization. See 'decodeValue'' for documentation of parameters.
decode1' :: FromJSON v => SchemaResolver -> (J.Value -> Either String Text) -> BS.L.ByteString -> Either String v
decode1' schema keyconv bs = case decodeValue' schema keyconv bs of
  Left (_ ,err) -> Left err
  Right vs -> case vs of 
    [] -> Left "No documents found in YAML stream"
    (_:_:_) -> Left "Multiple documents encountered in YAML stream"
    [v1] -> do
      case J.fromJSON v1 of
        J.Success v2 -> Right $! v2
        J.Error err  -> Left ("fromJSON: " ++ err)

-- | Parse YAML documents into JSON 'Value' ASTs
--
-- This is a wrapper function equivalent to
--
-- @'decodeValue'' 'coreSchemaResolver' identityKeyConv@
--
-- with @identityKeyConv@ being defined as
--
-- >> identityKeyConv :: Data.Aeson.Value -> Either String Text
-- >> identityKeyConv (Data.Aeson.String k) = Right k
-- >> identityKeyConv _ = Left "non-String key encountered in YAML mapping"
--
-- which performs no conversion and will fail when encountering YAML
-- Scalars that have not been resolved to a text Scalar (according to
-- the respective YAML schema resolver).
decodeValue :: BS.L.ByteString -> Either (Pos, String) [J.Value]
decodeValue = decodeValue' coreSchemaResolver identityKeyConv
  where
    identityKeyConv :: J.Value -> Either String Text
    identityKeyConv (J.String k) = Right k
    identityKeyConv _ = Left "non-String key encountered in mapping"

-- | Parse YAML documents into JSON 'Value' ASTs
--
-- YAML Anchors will be resolved and inlined accordingly. Resulting YAML cycles are not supported and will be treated as a decoding error.
--
-- __NOTE__: This decoder ignores YAML tags and relies on the YAML
-- 'SchemaResolver' provided to ensure that scalars have been resolved
-- to the proper known core YAML types.

decodeValue' :: SchemaResolver  -- ^ YAML Schema resolver to use
             -> (J.Value -> Either String Text)
                -- ^ JSON object key conversion function. This operates on the YAML node as resolved by the 'SchemaResolver' and subsequently converted into a JSON Value according to the 'scalarToValue' conversion. See 'decodeValue' documentation for an example.

             -> BS.L.ByteString -- ^ YAML document to parse
             -> Either (Pos, String) [J.Value]
decodeValue' SchemaResolver{..} keyconv bs0
    = runIdentity (decodeLoader failsafeLoader bs0)
  where
    failsafeLoader = Loader { yScalar   = \t s v _ -> pure $! schemaResolverScalar t s v >>= mkScl
                            , ySequence = \t vs _  -> pure $! schemaResolverSequence t >>= \_ -> mkArr vs
                            , yMapping  = \t kvs _  -> pure $! schemaResolverMapping  t >>= \_ -> mkObj kvs
                            , yAlias    = \_ c n _ -> pure $! if c then Left "cycle detected" else Right n
                            , yAnchor   = \_ n _   -> Ap.pure $! Right $! n
                            }

    mkObj :: [(J.Value, J.Value)] -> Either String J.Value
    mkObj xs = object <$> mapM mkPair xs

    mkPair :: (J.Value,J.Value) -> Either String J.Pair
    mkPair (k, v) = do
      k' <- keyconv k
      Right (k', v)

    mkArr :: [J.Value] -> Either String J.Value
    mkArr xs = Right $! J.Array $! V.fromList xs

    mkScl :: Y.Scalar -> Either String J.Value
    mkScl s = case scalarToValue s of
                Nothing -> Left "unresolved YAML scalar encountered"
                Just v  -> Right $! v

-- | Convert a YAML 'Scalar' into a JSON 'J.Value'
--
-- This conversion will return 'Nothing' for 'SUnknown',
-- i.e. unresolved YAML nodes.
scalarToValue :: Scalar -> Maybe J.Value
scalarToValue Y.SNull        = Just J.Null
scalarToValue (Y.SBool b)    = Just $! J.Bool b
scalarToValue (Y.SFloat x)   = Just $! J.Number (realToFrac x)
scalarToValue (Y.SInt i)     = Just $! J.Number (fromInteger i)
scalarToValue (SStr t)       = Just $! J.String t
scalarToValue (SUnknown _ _) = Nothing
