{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.News where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import QC.Generators.Common ( ArbStarDate(..) )
import QC.Generators.Database
import QC.Generators.People
import Dto.News ( DesignCreatedNewsDto(..), UserWrittenNewsDto(..), UserNewsIconDto(..)
                , NewsArticleDto(..) )


newtype ArbUserNewsIconDto = ArbUserNewsIconDto
    { unArbUserNewsIconDto :: UserNewsIconDto }


instance Arbitrary ArbUserNewsIconDto where
    arbitrary = oneof [ return $ ArbUserNewsIconDto GenericUserNewsDto
                      , return $ ArbUserNewsIconDto JubilationUserNewsDto
                      , return $ ArbUserNewsIconDto CatUserNewsDto
                      ]


singleDesignCreatedNewsDto :: Gen DesignCreatedNewsDto
singleDesignCreatedNewsDto = do
    aDesignId <- randomDesignKey
    aName <- arbitrary
    aDate <- arbitrary
    return $ DesignCreatedNewsDto { designCreatedNewsDtoDesignId = aDesignId
                                  , designCreatedNewsDtoName = aName
                                  , designCreatedNewsDtoDate = unArbStarDate aDate
                                  }


singleUserWrittenNewsDto :: Gen UserWrittenNewsDto
singleUserWrittenNewsDto = do
    aMessage <- arbitrary
    aDate <- arbitrary
    aUserName <- arbitrary
    aIcon <- arbitrary
    return $ UserWrittenNewsDto { userWrittenNewsDtoContent = aMessage
                                , userWrittenNewsDtoDate = unArbStarDate aDate
                                , userWrittenNewsDtoUser = unArbPersonName aUserName
                                , userWrittenNewsDtoIcon = unArbUserNewsIconDto aIcon
                                }


singleNewsArticleDto :: Gen NewsArticleDto
singleNewsArticleDto = do
    oneof [ do
                content <- singleUserWrittenNewsDto
                return $ UserWrittenDto content
          , do
                content <- singleDesignCreatedNewsDto
                return $ DesignCreatedDto content
          ]

-- data NewsArticleDto =
-- StarFoundDto StarFoundNewsDto
-- | PlanetFoundDto PlanetFoundNewsDto
-- | ConstructionFinishedDto ConstructionFinishedNewsDto
-- deriving (Show, Read, Eq)
