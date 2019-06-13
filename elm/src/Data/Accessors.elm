module Data.Accessors exposing
    ( activeUserIconA
    , ageA
    , amountA
    , availableBuildingsA
    , availableChassisA
    , availableComponentsA
    , availableResearchA
    , buildingSearchTextA
    , buildingsA
    , buildingsStatusA
    , chassisCurrentPageA
    , chassisListStatusA
    , choiceA
    , commandsStatusA
    , componentListStatusA
    , componentsA
    , componentsCurrentPageA
    , constructionStatusA
    , constructionsA
    , currentDesignA
    , currentPageA
    , currentResearchA
    , currentResearchStatusA
    , demesneA
    , demesneCurrentPageA
    , demesneStatusA
    , designPanelStatusA
    , designerRA
    , designsA
    , designsCurrentPageA
    , designsPanelStatusA
    , diplomacyA
    , emptyPanelStatusA
    , errorsA
    , focusedTopCategoryA
    , genderA
    , iconsA
    , idA
    , indexA
    , intelTypesA
    , intriqueA
    , landedShipsStatusA
    , learningA
    , martialA
    , messagesRA
    , messagesStatusA
    , nameA
    , newsA
    , newsPanelStatusA
    , orbitingShipsStatusA
    , pageSizeA
    , personA
    , personDetailsStatusA
    , personRA
    , planetA
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
    , relationsA
    , relationsCurrentPageA
    , relationsStatusA
    , researchFieldStatusA
    , researchProductionA
    , researchRA
    , selectedChassisA
    , selectedComponentsA
    , sexA
    , shortTitleA
    , starLanesStatusA
    , starListStatusA
    , starSystemA
    , starSystemRA
    , starSystemsA
    , starSystemsRA
    , starsA
    , statsA
    , statsStatusA
    , stewardshipA
    , systemDetailsStatusA
    , userEntryA
    , userEntryStatusA
    , validatationMessagesA
    )

import Accessors exposing (Relation, makeOneToN, makeOneToOne)
import Accessors.Library exposing (onEach)


intelTypesA : Relation field sub wrap -> Relation { rec | intelTypes : field } sub wrap
intelTypesA =
    makeOneToOne
        .intelTypes
        (\change rec -> { rec | intelTypes = change rec.intelTypes })


relationsCurrentPageA : Relation field sub wrap -> Relation { rec | relationsCurrentPage : field } sub wrap
relationsCurrentPageA =
    makeOneToOne
        .relationsCurrentPage
        (\change rec -> { rec | relationsCurrentPage = change rec.relationsCurrentPage })


relationsStatusA : Relation field sub wrap -> Relation { rec | relationsStatus : field } sub wrap
relationsStatusA =
    makeOneToOne
        .relationsStatus
        (\change rec -> { rec | relationsStatus = change rec.relationsStatus })


relationsA : Relation field sub wrap -> Relation { rec | relations : field } sub wrap
relationsA =
    makeOneToOne
        .relations
        (\change rec -> { rec | relations = change rec.relations })


shortTitleA : Relation field sub wrap -> Relation { rec | shortTitle : field } sub wrap
shortTitleA =
    makeOneToOne
        .shortTitle
        (\change rec -> { rec | shortTitle = change rec.shortTitle })


demesneCurrentPageA : Relation field sub wrap -> Relation { rec | demesneCurrentPage : field } sub wrap
demesneCurrentPageA =
    makeOneToOne
        .demesneCurrentPage
        (\change rec -> { rec | demesneCurrentPage = change rec.demesneCurrentPage })


demesneStatusA : Relation field sub wrap -> Relation { rec | demesneStatus : field } sub wrap
demesneStatusA =
    makeOneToOne
        .demesneStatus
        (\change rec -> { rec | demesneStatus = change rec.demesneStatus })


demesneA : Relation field sub wrap -> Relation { rec | demesne : field } sub wrap
demesneA =
    makeOneToOne
        .demesne
        (\change rec -> { rec | demesne = change rec.demesne })


starSystemA : Relation field sub wrap -> Relation { rec | starSystem : field } sub wrap
starSystemA =
    makeOneToOne
        .starSystem
        (\change rec -> { rec | starSystem = change rec.starSystem })


starSystemRA : Relation field sub wrap -> Relation { rec | starSystemR : field } sub wrap
starSystemRA =
    makeOneToOne
        .starSystemR
        (\change rec -> { rec | starSystemR = change rec.starSystemR })


statsStatusA : Relation field sub wrap -> Relation { rec | statsStatus : field } sub wrap
statsStatusA =
    makeOneToOne
        .statsStatus
        (\change rec -> { rec | statsStatus = change rec.statsStatus })


statsA : Relation field sub wrap -> Relation { rec | stats : field } sub wrap
statsA =
    makeOneToOne
        .stats
        (\change rec -> { rec | stats = change rec.stats })


martialA : Relation field sub wrap -> Relation { rec | martial : field } sub wrap
martialA =
    makeOneToOne
        .martial
        (\change rec -> { rec | martial = change rec.martial })


intriqueA : Relation field sub wrap -> Relation { rec | intrique : field } sub wrap
intriqueA =
    makeOneToOne
        .intrique
        (\change rec -> { rec | intrique = change rec.intrique })


learningA : Relation field sub wrap -> Relation { rec | learning : field } sub wrap
learningA =
    makeOneToOne
        .learning
        (\change rec -> { rec | learning = change rec.learning })


stewardshipA : Relation field sub wrap -> Relation { rec | stewardship : field } sub wrap
stewardshipA =
    makeOneToOne
        .stewardship
        (\change rec -> { rec | stewardship = change rec.stewardship })


diplomacyA : Relation field sub wrap -> Relation { rec | diplomacy : field } sub wrap
diplomacyA =
    makeOneToOne
        .diplomacy
        (\change rec -> { rec | diplomacy = change rec.diplomacy })


genderA : Relation field sub wrap -> Relation { rec | gender : field } sub wrap
genderA =
    makeOneToOne
        .gender
        (\change rec -> { rec | gender = change rec.gender })


sexA : Relation field sub wrap -> Relation { rec | sex : field } sub wrap
sexA =
    makeOneToOne
        .sex
        (\change rec -> { rec | sex = change rec.sex })


personDetailsStatusA : Relation field sub wrap -> Relation { rec | personDetailsStatus : field } sub wrap
personDetailsStatusA =
    makeOneToOne
        .personDetailsStatus
        (\change rec -> { rec | personDetailsStatus = change rec.personDetailsStatus })


ageA : Relation field sub wrap -> Relation { rec | age : field } sub wrap
ageA =
    makeOneToOne
        .age
        (\change rec -> { rec | age = change rec.age })


personRA : Relation field sub wrap -> Relation { rec | personR : field } sub wrap
personRA =
    makeOneToOne
        .personR
        (\change rec -> { rec | personR = change rec.personR })


personA : Relation field sub wrap -> Relation { rec | person : field } sub wrap
personA =
    makeOneToOne
        .person
        (\change rec -> { rec | person = change rec.person })


planetA : Relation field sub wrap -> Relation { rec | planet : field } sub wrap
planetA =
    makeOneToOne
        .planet
        (\change rec -> { rec | planet = change rec.planet })


componentsCurrentPageA : Relation field sub wrap -> Relation { rec | componentsCurrentPage : field } sub wrap
componentsCurrentPageA =
    makeOneToOne
        .componentsCurrentPage
        (\change rec -> { rec | componentsCurrentPage = change rec.componentsCurrentPage })


designsCurrentPageA : Relation field sub wrap -> Relation { rec | designsCurrentPage : field } sub wrap
designsCurrentPageA =
    makeOneToOne
        .designsCurrentPage
        (\change rec -> { rec | designsCurrentPage = change rec.designsCurrentPage })


chassisCurrentPageA : Relation field sub wrap -> Relation { rec | chassisCurrentPage : field } sub wrap
chassisCurrentPageA =
    makeOneToOne
        .chassisCurrentPage
        (\change rec -> { rec | chassisCurrentPage = change rec.chassisCurrentPage })


designsPanelStatusA : Relation field sub wrap -> Relation { rec | designsPanelStatus : field } sub wrap
designsPanelStatusA =
    makeOneToOne
        .designsPanelStatus
        (\change rec -> { rec | designsPanelStatus = change rec.designsPanelStatus })


validatationMessagesA : Relation field sub wrap -> Relation { rec | validatationMessages : field } sub wrap
validatationMessagesA =
    makeOneToOne
        .validatationMessages
        (\change rec -> { rec | validatationMessages = change rec.validatationMessages })


nameA : Relation field sub wrap -> Relation { rec | name : field } sub wrap
nameA =
    makeOneToOne
        .name
        (\change rec -> { rec | name = change rec.name })


amountA : Relation field sub wrap -> Relation { rec | amount : field } sub wrap
amountA =
    makeOneToOne
        .amount
        (\change rec -> { rec | amount = change rec.amount })


componentsA : Relation field sub wrap -> Relation { rec | components : field } sub wrap
componentsA =
    makeOneToOne
        .components
        (\change rec -> { rec | components = change rec.components })


currentDesignA : Relation field sub wrap -> Relation { rec | currentDesign : field } sub wrap
currentDesignA =
    makeOneToOne
        .currentDesign
        (\change rec -> { rec | currentDesign = change rec.currentDesign })


designPanelStatusA : Relation field sub wrap -> Relation { rec | designPanelStatus : field } sub wrap
designPanelStatusA =
    makeOneToOne
        .designPanelStatus
        (\change rec -> { rec | designPanelStatus = change rec.designPanelStatus })


selectedComponentsA : Relation field sub wrap -> Relation { rec | selectedComponents : field } sub wrap
selectedComponentsA =
    makeOneToOne
        .selectedComponents
        (\change rec -> { rec | selectedComponents = change rec.selectedComponents })


selectedChassisA : Relation field sub wrap -> Relation { rec | selectedChassis : field } sub wrap
selectedChassisA =
    makeOneToOne
        .selectedChassis
        (\change rec -> { rec | selectedChassis = change rec.selectedChassis })


messagesStatusA : Relation field sub wrap -> Relation { rec | messagesStatus : field } sub wrap
messagesStatusA =
    makeOneToOne
        .messagesStatus
        (\change rec -> { rec | messagesStatus = change rec.messagesStatus })


emptyPanelStatusA : Relation field sub wrap -> Relation { rec | emptyPanelStatus : field } sub wrap
emptyPanelStatusA =
    makeOneToOne
        .emptyPanelStatus
        (\change rec -> { rec | emptyPanelStatus = change rec.emptyPanelStatus })


designerRA : Relation field sub wrap -> Relation { rec | designerR : field } sub wrap
designerRA =
    makeOneToOne
        .designerR
        (\change rec -> { rec | designerR = change rec.designerR })


componentListStatusA : Relation field sub wrap -> Relation { rec | componentListStatus : field } sub wrap
componentListStatusA =
    makeOneToOne
        .componentListStatus
        (\change rec -> { rec | componentListStatus = change rec.componentListStatus })


commandsStatusA : Relation field sub wrap -> Relation { rec | commandsStatus : field } sub wrap
commandsStatusA =
    makeOneToOne
        .commandsStatus
        (\change rec -> { rec | commandsStatus = change rec.commandsStatus })


chassisListStatusA : Relation field sub wrap -> Relation { rec | chassisListStatus : field } sub wrap
chassisListStatusA =
    makeOneToOne
        .chassisListStatus
        (\change rec -> { rec | chassisListStatus = change rec.chassisListStatus })


designsA : Relation field sub wrap -> Relation { rec | designs : field } sub wrap
designsA =
    makeOneToOne
        .designs
        (\change rec -> { rec | designs = change rec.designs })


availableChassisA : Relation field sub wrap -> Relation { rec | availableChassis : field } sub wrap
availableChassisA =
    makeOneToOne
        .availableChassis
        (\change rec -> { rec | availableChassis = change rec.availableChassis })


availableComponentsA : Relation field sub wrap -> Relation { rec | availableComponents : field } sub wrap
availableComponentsA =
    makeOneToOne
        .availableComponents
        (\change rec -> { rec | availableComponents = change rec.availableComponents })


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
