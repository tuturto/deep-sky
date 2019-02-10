{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Research.Tree ( techTree, techMap )
    where

import qualified Data.Map.Strict as Map
import Research.Data ( Research(..), ResearchCategory(..), EngineeringSubField(..)
                     , SocialScienceSubField(..), TechTree(..), TotalResearchScore(..)
                     , ResearchTier(..), Technology(..), ResearchScore(..)
                     )

techTree :: TechTree
techTree = TechTree
    [ Research { researchName = "High-gain sensors"
               , researchType = HighSensitivitySensors
               , researchCategory = Engineering FieldManipulation
               , researchAntecedents = []
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 1
               }
    , Research { researchName = "Side channel sensors"
               , researchType = SideChannelSensors
               , researchCategory = Engineering FieldManipulation
               , researchAntecedents = [ HighSensitivitySensors ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 1
               }
    , Research { researchName = "Satellites"
               , researchType = SatelliteTechnology
               , researchCategory = Engineering Materials
               , researchAntecedents = [ ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 1
               }
    -- various hulls
    , Research { researchName = "Destroyer hulls"
               , researchType = DestroyerHulls
               , researchCategory = Engineering Materials
               , researchAntecedents = [ ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 1
               }
    , Research { researchName = "Frigate hulls"
               , researchType = FrigateHulls
               , researchCategory = Engineering Materials
               , researchAntecedents = [ DestroyerHulls ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 750
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 1
               }
    , Research { researchName = "Cruiser hulls"
               , researchType = CruiserHulls
               , researchCategory = Engineering Materials
               , researchAntecedents = [ FrigateHulls ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 1000
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 2
               }
    , Research { researchName = "Battleship hulls"
               , researchType = BattleShipHulls
               , researchCategory = Engineering Materials
               , researchAntecedents = [ CruiserHulls ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 1500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 2
               }
    , Research { researchName = "Station hulls"
               , researchType = StationHulls
               , researchCategory = Engineering Materials
               , researchAntecedents = []
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 2500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 2
               }
    , Research { researchName = "Mobilebase hulls"
               , researchType = MobileBaseHulls
               , researchCategory = Engineering Materials
               , researchAntecedents = [ StationHulls ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 2500
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 3
               }
    -- general military theory
    , Research { researchName = "Explorer corps"
               , researchType = ExplorerCorps
               , researchCategory = SocialScience MilitaryTheory
               , researchAntecedents = [ ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 0
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 500
                                    }
               , researchTier = ResearchTier 1
               }
    , Research { researchName = "Carrier operations"
               , researchType = CarrierOps
               , researchCategory = SocialScience MilitaryTheory
               , researchAntecedents = [ BattleShipHulls ]
               , researchCost = TotalResearchScore
                                    { totalResearchScoreEngineering = ResearchScore 1000
                                    , totalResearchScoreNatural = ResearchScore 0
                                    , totalResearchScoreSocial = ResearchScore 0
                                    }
               , researchTier = ResearchTier 2
               }
    ]


techMap :: Map.Map Technology Research
techMap = Map.fromList $ (\x -> (researchType x, x)) <$> unTechTree techTree
