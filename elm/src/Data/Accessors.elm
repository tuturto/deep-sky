module Data.Accessors exposing
    ( activeUserIconA
    , availableBuildingsA
    , availableResearchA
    , buildingSearchTextA
    , buildingsA
    , buildingsStatusA
    , choiceA
    , constructionStatusA
    , constructionsA
    , currentPageA
    , currentResearchA
    , currentResearchStatusA
    , errorsA
    , focusedTopCategoryA
    , iconsA
    , idA
    , indexA
    , landedShipsStatusA
    , messagesRA
    , newsA
    , newsPanelStatusA
    , orbitingShipsStatusA
    , pageSizeA
    , planetDetailsStatusA
    , planetIdA
    , planetRA
    , planetStatusA
    , planetStatusesStatusA
    , planetsA
    , planetsStatusA
    , populationStatusA
    , populationsA
    , productionStatusA
    , researchFieldStatusA
    , researchProductionA
    , researchRA
    , starLanesStatusA
    , starListStatusA
    , starSystemsA
    , starSystemsRA
    , starsA
    , systemDetailsStatusA
    , userEntryA
    , userEntryStatusA
    )

import Accessors exposing (Relation, makeOneToOne)


productionStatusA : Relation field sub wrap -> Relation { rec | productionStatus : field } sub wrap
productionStatusA =
    makeOneToOne
        .productionStatus
        (\change rec -> { rec | productionStatus = change rec.productionStatus })


researchProductionA : Relation field sub wrap -> Relation { rec | researchProduction : field } sub wrap
researchProductionA =
    makeOneToOne
        .researchProduction
        (\change rec -> { rec | researchProduction = change rec.researchProduction })


focusedTopCategoryA : Relation field sub wrap -> Relation { rec | focusedTopCategory : field } sub wrap
focusedTopCategoryA =
    makeOneToOne
        .focusedTopCategory
        (\change rec -> { rec | focusedTopCategory = change rec.focusedTopCategory })


researchFieldStatusA : Relation field sub wrap -> Relation { rec | researchFieldStatus : field } sub wrap
researchFieldStatusA =
    makeOneToOne
        .researchFieldStatus
        (\change rec -> { rec | researchFieldStatus = change rec.researchFieldStatus })


researchRA : Relation field sub wrap -> Relation { rec | researchR : field } sub wrap
researchRA =
    makeOneToOne
        .researchR
        (\change rec -> { rec | researchR = change rec.researchR })


currentResearchStatusA : Relation field sub wrap -> Relation { rec | currentResearchStatus : field } sub wrap
currentResearchStatusA =
    makeOneToOne
        .currentResearchStatus
        (\change rec -> { rec | currentResearchStatus = change rec.currentResearchStatus })


currentResearchA : Relation field sub wrap -> Relation { rec | currentResearch : field } sub wrap
currentResearchA =
    makeOneToOne
        .currentResearch
        (\change rec -> { rec | currentResearch = change rec.currentResearch })


availableResearchA : Relation field sub wrap -> Relation { rec | availableResearch : field } sub wrap
availableResearchA =
    makeOneToOne
        .availableResearch
        (\change rec -> { rec | availableResearch = change rec.availableResearch })


planetStatusesStatusA : Relation field sub wrap -> Relation { rec | planetStatusesStatus : field } sub wrap
planetStatusesStatusA =
    makeOneToOne
        .planetStatusesStatus
        (\change rec -> { rec | planetStatusesStatus = change rec.planetStatusesStatus })


planetIdA : Relation field sub wrap -> Relation { rec | planetId : field } sub wrap
planetIdA =
    makeOneToOne
        .planetId
        (\change rec -> { rec | planetId = change rec.planetId })


idA : Relation field sub wrap -> Relation { rec | id : field } sub wrap
idA =
    makeOneToOne
        .id
        (\change rec -> { rec | id = change rec.id })


planetStatusA : Relation field sub wrap -> Relation { rec | planetStatus : field } sub wrap
planetStatusA =
    makeOneToOne
        .planetStatus
        (\change rec -> { rec | planetStatus = change rec.planetStatus })


choiceA : Relation field sub wrap -> Relation { rec | choice : field } sub wrap
choiceA =
    makeOneToOne
        .choice
        (\change rec -> { rec | choice = change rec.choice })


activeUserIconA : Relation field sub wrap -> Relation { rec | activeUserIcon : field } sub wrap
activeUserIconA =
    makeOneToOne
        .activeUserIcon
        (\change rec -> { rec | activeUserIcon = change rec.activeUserIcon })


iconsA : Relation field sub wrap -> Relation { rec | icons : field } sub wrap
iconsA =
    makeOneToOne
        .icons
        (\change rec -> { rec | icons = change rec.icons })


pageSizeA : Relation field sub wrap -> Relation { rec | pageSize : field } sub wrap
pageSizeA =
    makeOneToOne
        .pageSize
        (\change rec -> { rec | pageSize = change rec.pageSize })


userEntryA : Relation field sub wrap -> Relation { rec | userEntry : field } sub wrap
userEntryA =
    makeOneToOne
        .userEntry
        (\change rec -> { rec | userEntry = change rec.userEntry })


userEntryStatusA : Relation field sub wrap -> Relation { rec | userEntryStatus : field } sub wrap
userEntryStatusA =
    makeOneToOne
        .userEntryStatus
        (\change rec -> { rec | userEntryStatus = change rec.userEntryStatus })


newsPanelStatusA : Relation field sub wrap -> Relation { rec | newsPanelStatus : field } sub wrap
newsPanelStatusA =
    makeOneToOne
        .newsPanelStatus
        (\change rec -> { rec | newsPanelStatus = change rec.newsPanelStatus })


currentPageA : Relation field sub wrap -> Relation { rec | currentPage : field } sub wrap
currentPageA =
    makeOneToOne
        .currentPage
        (\change rec -> { rec | currentPage = change rec.currentPage })


messagesRA : Relation field sub wrap -> Relation { rec | messagesR : field } sub wrap
messagesRA =
    makeOneToOne
        .messagesR
        (\change rec -> { rec | messagesR = change rec.messagesR })


newsA : Relation field sub wrap -> Relation { rec | news : field } sub wrap
newsA =
    makeOneToOne
        .news
        (\change rec -> { rec | news = change rec.news })


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
