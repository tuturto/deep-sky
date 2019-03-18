{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Research.Tree ( techTree, techMap )
    where

import Research.Data ( Research(..), ResearchCategory(..), EngineeringSubField(..)
                     , SocialScienceSubField(..), TechTree(..), TotalResearchScore(..)
                     , ResearchTier(..), Technology(..), ResearchScore(..)
                     )

techTree :: TechTree
techTree =
    TechTree $ fmap techMap [minBound ..]


techMap :: Technology -> Research
techMap HighSensitivitySensors =
    Research { researchName = "High-gain sensors"
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

techMap SideChannelSensors =
    Research { researchName = "Side channel sensors"
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

techMap SatelliteTechnology =
    Research { researchName = "Satellites"
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
-- working line
techMap BawleyHulls =
    Research { researchName = "Bawley hulls"
             , researchType = BawleyHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

techMap YawlHulls =
    Research { researchName = "Yawl hulls"
             , researchType = YawlHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ BawleyHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 750
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

techMap BilanderHulls =
    Research { researchName = "Bilander hulls"
             , researchType = BilanderHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ YawlHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 1000
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 2
             }

techMap CogHulls =
    Research { researchName = "Cog hulls"
             , researchType = CogHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ BilanderHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 1500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 2
             }

techMap FreighterHulls =
    Research { researchName = "Freighter hulls"
             , researchType = FreighterHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ CogHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 2500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 3
             }

techMap CraneShipHulls =
    Research { researchName = "Crane ship hulls"
             , researchType = CraneShipHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ CogHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 2500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 3
             }

techMap CruiseLinerHulls =
    Research { researchName = "Cruise liner hulls"
             , researchType = CruiseLinerHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ CogHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 2500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 3
             }

techMap SatelliteLayerHulls =
    Research { researchName = "Satellite layer hulls"
             , researchType = SatelliteLayerHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ CogHulls
                                     , SatelliteTechnology ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 2500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 3
             }

-- fast line
techMap FlyboatHulls =
    Research { researchName = "Flyboat hulls"
             , researchType = FlyboatHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

techMap BrigantineHulls =
    Research { researchName = "Brigantine hulls"
             , researchType = BrigantineHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ FlyboatHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 750
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

techMap SchoonerHulls =
    Research { researchName = "Schooner hulls"
             , researchType = SchoonerHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ BrigantineHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 1000
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 2
             }

techMap BlackwallFrigateHulls =
    Research { researchName = "Blackwall frigate hulls"
             , researchType = BlackwallFrigateHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ SchoonerHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 1500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 2
             }

techMap ClipperHulls =
    Research { researchName = "Clipper hulls"
             , researchType = ClipperHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ BlackwallFrigateHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 2500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 3
             }

-- war line
techMap CaravelHulls =
    Research { researchName = "Caravel hulls"
             , researchType = CaravelHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

techMap CorvetteHulls =
    Research { researchName = "Corvette hulls"
             , researchType = CorvetteHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ CaravelHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 750
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

techMap FrigateHulls =
    Research { researchName = "Frigate hulls"
             , researchType = FrigateHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ CorvetteHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 1000
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 2
             }

techMap GalleonHulls =
    Research { researchName = "Galleon hulls"
             , researchType = GalleonHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ FrigateHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 1500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 2
             }

techMap ManOfWarHulls =
    Research { researchName = "Man-of-War hulls"
             , researchType = ManOfWarHulls
             , researchCategory = Engineering Materials
             , researchAntecedents = [ GalleonHulls ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 2500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 3
             }

-- misc hulls
techMap StationHulls =
    Research { researchName = "Station hulls"
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

techMap MobileBaseHulls =
    Research { researchName = "Mobilebase hulls"
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

techMap ExplorerCorps =
    Research { researchName = "Explorer corps"
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

-- armour plating
techMap HighTensileMaterials =
    Research { researchName = "High tensile materials"
             , researchType = HighTensileMaterials
             , researchCategory = Engineering Materials
             , researchAntecedents = [ ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }

-- motive systems
techMap HoverCrafts =
    Research { researchName = "Hovercrafts"
             , researchType = HoverCrafts
             , researchCategory = Engineering Propulsion
             , researchAntecedents = [ ]
             , researchCost = TotalResearchScore
                                { totalResearchScoreEngineering = ResearchScore 500
                                , totalResearchScoreNatural = ResearchScore 0
                                , totalResearchScoreSocial = ResearchScore 0
                                }
             , researchTier = ResearchTier 1
             }
