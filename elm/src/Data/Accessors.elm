module Data.Accessors exposing
    ( availableBuildingsA
    , buildingSearchTextA
    , buildingsA
    , buildingsStatusA
    , constructionStatusA
    , constructionsA
    , errorsA
    , indexA
    , landedShipsStatusA
    , orbitingShipsStatusA
    , planetDetailsStatusA
    , planetRA
    , planetsA
    , planetsStatusA
    , populationStatusA
    , populationsA
    , starLanesStatusA
    , starListStatusA
    , starSystemsA
    , starSystemsRA
    , starsA
    , systemDetailsStatusA
    )

import Accessors exposing (Relation, makeOneToOne)


buildingSearchTextA : Relation field sub wrap -> Relation { rec | buildingSearchText : field } sub wrap
buildingSearchTextA =
    makeOneToOne
        .buildingSearchText
        (\change rec -> { rec | buildingSearchText = change rec.buildingSearchText })


availableBuildingsA : Relation field sub wrap -> Relation { rec | availableBuildings : field } sub wrap
availableBuildingsA =
    makeOneToOne
        .availableBuildings
        (\change rec -> { rec | availableBuildings = change rec.availableBuildings })


indexA : Relation field sub wrap -> Relation { rec | index : field } sub wrap
indexA =
    makeOneToOne
        .index
        (\change rec -> { rec | index = change rec.index })


constructionsA : Relation field sub wrap -> Relation { rec | constructions : field } sub wrap
constructionsA =
    makeOneToOne
        .constructions
        (\change rec -> { rec | constructions = change rec.constructions })


constructionStatusA : Relation field sub wrap -> Relation { rec | constructionStatus : field } sub wrap
constructionStatusA =
    makeOneToOne
        .constructionStatus
        (\change rec -> { rec | constructionStatus = change rec.constructionStatus })


buildingsA : Relation field sub wrap -> Relation { rec | buildings : field } sub wrap
buildingsA =
    makeOneToOne
        .buildings
        (\change rec -> { rec | buildings = change rec.buildings })


orbitingShipsStatusA : Relation field sub wrap -> Relation { rec | orbitingShipsStatus : field } sub wrap
orbitingShipsStatusA =
    makeOneToOne
        .orbitingShipsStatus
        (\change rec -> { rec | orbitingShipsStatus = change rec.orbitingShipsStatus })


landedShipsStatusA : Relation field sub wrap -> Relation { rec | landedShipsStatus : field } sub wrap
landedShipsStatusA =
    makeOneToOne
        .landedShipsStatus
        (\change rec -> { rec | landedShipsStatus = change rec.landedShipsStatus })


buildingsStatusA : Relation field sub wrap -> Relation { rec | buildingsStatus : field } sub wrap
buildingsStatusA =
    makeOneToOne
        .buildingsStatus
        (\change rec -> { rec | buildingsStatus = change rec.buildingsStatus })


errorsA : Relation field sub wrap -> Relation { rec | errors : field } sub wrap
errorsA =
    makeOneToOne
        .errors
        (\change rec -> { rec | errors = change rec.errors })


populationsA : Relation field sub wrap -> Relation { rec | populations : field } sub wrap
populationsA =
    makeOneToOne
        .populations
        (\change rec -> { rec | populations = change rec.populations })


populationStatusA : Relation field sub wrap -> Relation { rec | populationStatus : field } sub wrap
populationStatusA =
    makeOneToOne
        .populationStatus
        (\change rec -> { rec | populationStatus = change rec.populationStatus })


planetDetailsStatusA : Relation field sub wrap -> Relation { rec | planetDetailsStatus : field } sub wrap
planetDetailsStatusA =
    makeOneToOne
        .planetDetailsStatus
        (\change rec -> { rec | planetDetailsStatus = change rec.planetDetailsStatus })


planetRA : Relation field sub wrap -> Relation { rec | planetR : field } sub wrap
planetRA =
    makeOneToOne
        .planetR
        (\change rec -> { rec | planetR = change rec.planetR })


starSystemsA : Relation field sub wrap -> Relation { rec | starSystems : field } sub wrap
starSystemsA =
    makeOneToOne
        .starSystems
        (\change rec -> { rec | starSystems = change rec.starSystems })


planetsA : Relation field sub wrap -> Relation { rec | planets : field } sub wrap
planetsA =
    makeOneToOne
        .planets
        (\change rec -> { rec | planets = change rec.planets })


starSystemsRA : Relation field sub wrap -> Relation { rec | starSystemsR : field } sub wrap
starSystemsRA =
    makeOneToOne
        .starSystemsR
        (\change rec -> { rec | starSystemsR = change rec.starSystemsR })


systemDetailsStatusA : Relation field sub wrap -> Relation { rec | systemDetailsStatus : field } sub wrap
systemDetailsStatusA =
    makeOneToOne
        .systemDetailsStatus
        (\change rec -> { rec | systemDetailsStatus = change rec.systemDetailsStatus })


starListStatusA : Relation field sub wrap -> Relation { rec | starListStatus : field } sub wrap
starListStatusA =
    makeOneToOne
        .starListStatus
        (\change rec -> { rec | starListStatus = change rec.starListStatus })


starLanesStatusA : Relation field sub wrap -> Relation { rec | starLanesStatus : field } sub wrap
starLanesStatusA =
    makeOneToOne
        .starLanesStatus
        (\change rec -> { rec | starLanesStatus = change rec.starLanesStatus })


planetsStatusA : Relation field sub wrap -> Relation { rec | planetsStatus : field } sub wrap
planetsStatusA =
    makeOneToOne
        .planetsStatus
        (\change rec -> { rec | planetsStatus = change rec.planetsStatus })


starsA : Relation field sub wrap -> Relation { rec | stars : field } sub wrap
starsA =
    makeOneToOne
        .stars
        (\change rec -> { rec | stars = change rec.stars })
