# Changelog
Notable changes to the project are documented here. Latest ones are on top.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `SystemStatus` and related checks for API endpoints
- People section for admin panel
- `Creators.Person` module for generating new people
- interface for accessing unit info added
- units report their status to faction of owner
- Robot Framework and Selenium taken into use for testing
- `pushUrl` function for type safe url manipulation
- `SaveData` to track when data is being saved
- generic tab control on the client side
### Changed
- Admin interface has been rewritten in Elm
- `Age` changed from `Int` to `Natural`
- `Vehicles` module renamed to `Units`
- url of planet changed from `/starsystem/sId/pId` to `planet/pId`
- type of design name changed from `Text` to `DesignName`
- `PersonName` and related types and functions moved from `Data.Person` to `Data.PersonName` on client
- Data loaded from server is now handled with `RemoteData` library
### Removed
### Fixed
- [Old planet details are shown][6]
- [No people are shown in admin view][13]
- [Submitting user written news fails][14]
### Known bugs
- [Disallow dismissing special events][1]

## [0.2.0] - 2019-10-19
### Added
- Players have avatar that they're playing as
- `Person` to represent people in detailed way
- `HumanIntelligence` to represent level of person intel
- `Relation` to represent relations between people
- `Dynasty` to represent dynasties
- `Marriage` to represent engagements and marriages
- `Errors` module for common error codes and returning them to client
- Started using `Data.Either.Validation` for message validation
- People can have pets
- Added immediate events that are resolved as soon as choice has been made
- Added `Markov` module for working with markov chains
- Added `Names` module for generating names with markov chains
- Added `Data.Aeson.Lens` as a dependency
- `PlanetName`, `StarName` and `StarSystemName` taken into use
- People can be in different planets or assigned to a unit
- Designs have crew requirements displayed
### Changed
- `News` can now be targeted to faction or specific person
- planet can have ruler
- planet report JSON result has all keys starting with uppercase
- star system can have ruler
- Database table `time` replaced with `simulation`
- `StarDate` used instead of `Time` everywhere
- Level of component changed from `Int` to `ComponentLevel`
- Amount of components changed from `Int` to `ComponentAmount`
- Component name changed from `String` to `ComponentName`
- Component description changed from `String` to `ComponentDescription`
- Players are not directly members of faction, but via their avatar
- `requireFaction` returns also user avatar
- `apiRequireFaction` returns also user avatar
- User submitted news have player avatar as originator
- Faction resources changed from `Int` to `RawResource a`
- Faction name changed from `Text` to `FactionName`
- `SpectralType`, `LuminosityClass`, `PlanetaryStatus` and `Coordinates` moved to `Space.Data`
- Designs must alloce enough living quarters for their crews
### Removed
- `maybeFaction` function removed
### Fixed
- [Empty construction queue on a planet causes error during simulation][4]
- [Remove widget files][5]
- [Designer shows only 6 components][10]
### Known bugs
- [Disallow dismissing special events][1]
- [Old planet details are shown][6]

## [0.1.0] - 2019-05-11
### Added
- List of known star systems
- Details of kown star system
- Details of known planets
- Initial system for resource production on planets
- System for constructing buildings on planets
- System for performing research
- List of received messages
- Users may submit their own news
- Special events system
- [User manual](https://tuturto.github.io/deep-sky/)
- Vehicle designer
### Known bugs
- [Disallow dismissing special events][1]

[Unreleased]: https://github.com/tuturto/deep-sky/compare/0.2.0...HEAD

[0.1.0]: https://github.com/tuturto/deep-sky/releases/tag/0.1.0
[0.2.0]: https://github.com/tuturto/deep-sky/releases/tag/0.2.0

[1]: https://github.com/tuturto/deep-sky/issues/1
[4]: https://github.com/tuturto/deep-sky/issues/4
[5]: https://github.com/tuturto/deep-sky/issues/5
[6]: https://github.com/tuturto/deep-sky/issues/6
[10]: https://github.com/tuturto/deep-sky/issues/10
[13]: https://github.com/tuturto/deep-sky/issues/13
[14]: https://github.com/tuturto/deep-sky/issues/14