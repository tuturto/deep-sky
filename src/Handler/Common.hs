{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import MenuHelpers ( starDate, getScore )
import Common ( apiRequireFaction )
import Dto.Common ( StarDateResponse(..) )

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.


getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


-- | API for loading current star date
getApiStarDateR :: Handler Value
getApiStarDateR = do
    currentDate <- runDB starDate
    return $ toJSON $ StarDateResponse currentDate


-- | API for loading currently available resources
getApiResourcesR :: Handler Value
getApiResourcesR = do
    (_, _, _, fId) <- apiRequireFaction
    faction <- runDB $ get fId
    let score = getScore faction
    return $ toJSON score
