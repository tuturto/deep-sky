{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}

module Errors
    ( ErrorCode(..), raiseIfErrors )

    where

import Import
import qualified Prelude as P
import Data.Aeson.TH ( deriveJSON, defaultOptions )


-- | Error codes for all errors returned by API
data ErrorCode
    -- common error codes
    = ResourceNotFound
    | InsufficientRights
    | FailedToParseDataInDatabase
    -- errors specific to news
    | SpecialEventHasAlreadyBeenResolved
    | UnsupportedArticleType
    | SpecialNewsExtractionFailed
    | TriedToMakeChoiceForRegularArticle
    deriving (Show, Read, Eq)


-- | Internal representation for error code - explanation pair
data ECode = ECode ErrorCode Text
    deriving (Show, Read, Eq)


instance ToJSON ECode where
    toJSON (ECode code expl) =
        object [ "code" .= code
               , "error" .= expl
               ]


-- | Map error code to http status code
errorCodeToStatusCode :: ErrorCode -> Int
errorCodeToStatusCode =
    \case
        ResourceNotFound -> 404
        InsufficientRights -> 403
        FailedToParseDataInDatabase -> 500
        SpecialEventHasAlreadyBeenResolved -> 409
        UnsupportedArticleType -> 400
        SpecialNewsExtractionFailed -> 500
        TriedToMakeChoiceForRegularArticle -> 500


-- | Map status code to message text
statusCodeToText :: Int -> ByteString
statusCodeToText 200 = "OK"
statusCodeToText 400 = "Bad Request"
statusCodeToText 401 = "Unauthorized"
statusCodeToText 403 = "Forbidden"
statusCodeToText 404 = "Not Found"
statusCodeToText 409 = "Conflict"
statusCodeToText 500 = "Internal Server Error"
statusCodeToText _ = "Unknown"


-- | return error message to caller if any errors happened
-- http status code used is selected based on the first error code
raiseIfErrors :: [ErrorCode] -> HandlerFor App ()
raiseIfErrors errors = do
    when (not $ null errors) $ do
        let eCodes = (\x -> ECode x $ errorCodeToText x) <$> errors
        let code = P.head $ errorCodeToStatusCode <$> errors
        sendStatusJSON (Status code (statusCodeToText code)) $ toJSON eCodes


-- | Mapping between error codes and their plain text explanations
errorCodeToText :: ErrorCode -> Text
errorCodeToText =
    \case
        ResourceNotFound ->
            "Resource was not found"

        InsufficientRights ->
            "Insufficient rights to perform operation"

        SpecialEventHasAlreadyBeenResolved ->
            "Special event has already been resolved"

        FailedToParseDataInDatabase ->
            "Failed to parse contents of database"

        UnsupportedArticleType ->
            "This type of article can't be handeled"

        SpecialNewsExtractionFailed ->
            "Failed to extract special news from news article"

        TriedToMakeChoiceForRegularArticle ->
            "Tried to make a choice for regular article"


$(deriveJSON defaultOptions ''ErrorCode)