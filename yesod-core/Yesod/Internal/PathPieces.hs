{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Internal.PathPieces
    ( SinglePiece (..)
    , MultiPiece (..)
    ) where

import Data.Int (Int64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read

class SinglePiece master s where
    fromSinglePiece :: master -> S.Text -> Maybe s
    toSinglePiece :: master -> s -> S.Text
instance SinglePiece master String where
    fromSinglePiece _ s = if S.null s then Nothing else Just (S.unpack s)
    toSinglePiece _ = S.pack
instance SinglePiece master S.Text where
    fromSinglePiece _ s = if S.null s then Nothing else Just s
    toSinglePiece _ = id
instance SinglePiece master L.Text where
    fromSinglePiece _ s = if S.null s then Nothing else Just (L.fromChunks [s])
    toSinglePiece _ = S.concat . L.toChunks
instance SinglePiece master Integer where
    fromSinglePiece _ s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toSinglePiece _ = S.pack . show
instance SinglePiece master Int where
    fromSinglePiece _ s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toSinglePiece _ = S.pack . show
instance SinglePiece master Int64 where
    fromSinglePiece _ s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toSinglePiece _ = S.pack . show

class MultiPiece master s where
    fromMultiPiece :: master -> [S.Text] -> Maybe s
    toMultiPiece :: master -> s -> [S.Text]
instance MultiPiece master [String] where
    fromMultiPiece _ = Just . map S.unpack
    toMultiPiece _ = map S.pack
instance MultiPiece master [S.Text] where
    fromMultiPiece _ = Just
    toMultiPiece _ = id
instance MultiPiece master [L.Text] where
    fromMultiPiece _ = Just . map (L.fromChunks . return)
    toMultiPiece _ = map $ S.concat . L.toChunks
