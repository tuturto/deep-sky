insert into time (current_time) values (20191);

insert into person (name, sex, gender, date_of_birth)
    values ('RegularName (FirstName {unFirstName = "Paavo"}) (FamilyName {unFamilyName = "Virtanen"}) Nothing',
            'Male', 'Man', 19751);

insert into star_system (name, coord_x, coord_y,ruler_id) values ('Sol', 0, 0, 2);
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Sun', 1, 'G', 'V');
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Mercury', 1, 1, NULL, 0.38, NULL);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Venus', 2, 1, NULL, 0.904, NULL);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Earth', 3, 1, 1, 1.0, 1);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Mars', 4, 1, NULL, 0.376, NULL);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Jupiter', 6, 1, NULL, 2.528, NULL);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Saturn', 7, 1, NULL, 1.065, NULL);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Uranus', 8, 1, NULL, 0.886, NULL);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Neptune', 9, 1, NULL, 1.14, NULL);

insert into star_system (name, coord_x, coord_y, ruler_id) values ('Aclael', 4.2, -1.2, null);
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Aclael alpha', 2, 'A', 'II');
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Aclael beta', 2, 'M', 'VI');
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Aclael I', 1, 2, NULL, 0.9, NULL);

insert into race (name) values ('Terrans');
insert into race (name) values ('Aclaelites');

insert into planet_population (planet_id, race_id, population) values (3, 1, 10);

insert into star_system (name, coord_x, coord_y) values ('Barynth', 5.6, 2.2);
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Barynth alpha', 3, 'L', 'VI');

insert into star_lane (star_system1, star_system2) values (1, 2);
insert into star_lane (star_system1, star_system2) values (2, 3);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Bawley', 100, 'BawleyHulls', 'SpaceShip',
    2, 4, 4, 2, 2, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (1, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (1, 'StarSailComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (1, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (1, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Yawl', 120, 'YawlHulls', 'SpaceShip',
    2, 6, 6, 2, 2, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (2, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (2, 'StarSailComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (2, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (2, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Bilander', 400, 'BilanderHulls', 'SpaceShip',
    3, 8, 6, 2, 4, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (3, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (3, 'StarSailComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (3, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (3, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Cog', 600, 'CogHulls', 'SpaceShip',
    4, 10, 8, 2, 4, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (4, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (4, 'StarSailComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (4, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (4, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Freighter', 2000, 'FreighterHulls', 'SpaceShip',
    8, 20, 16, 3, 6, 0, 0, 4);
insert into required_component (chassis_id, component_type, level, amount) values (5, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (5, 'StarSailComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (5, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (5, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Crane ship', 800, 'CraneShipHulls', 'SpaceShip',
    2, 8, 16, 3, 1, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (6, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (6, 'StarSailComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (6, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (6, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Cruise liner', 2000, 'CruiseLinerHulls', 'SpaceShip',
    4, 40, 40, 4, 0, 0, 0, 4);
insert into required_component (chassis_id, component_type, level, amount) values (7, 'BridgeComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (7, 'StarSailComponent', 1, 4);
insert into required_component (chassis_id, component_type, level, amount) values (7, 'SensorComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (7, 'SupplyComponent', 1, 2);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Satellite Layer', 400, 'SatelliteLayerHulls', 'SpaceShip',
    2, 20, 10, 4, 1, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (8, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (8, 'StarSailComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (8, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (8, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Flyboat', 75, 'FlyboatHulls', 'SpaceShip',
    1, 5, 2, 2, 1, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (9, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (9, 'StarSailComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (9, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (9, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Brigantine', 100, 'BrigantineHulls', 'SpaceShip',
    2, 7, 4, 2, 1, 0, 0, 2);
insert into required_component (chassis_id, component_type, level, amount) values (10, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (10, 'StarSailComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (10, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (10, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Schooner', 400, 'SchoonerHulls', 'SpaceShip',
    4, 15, 10, 3, 2, 0, 0, 5);
insert into required_component (chassis_id, component_type, level, amount) values (11, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (11, 'StarSailComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (11, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (11, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Blackwall Frigate', 600, 'BlackwallFrigateHulls', 'SpaceShip',
    4, 20, 15, 3, 3, 0, 0, 3);
insert into required_component (chassis_id, component_type, level, amount) values (12, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (12, 'StarSailComponent', 1, 3);
insert into required_component (chassis_id, component_type, level, amount) values (12, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (12, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Clipper', 700, 'ClipperHulls', 'SpaceShip',
    4, 20, 15, 3, 3, 0, 0, 5);
insert into required_component (chassis_id, component_type, level, amount) values (13, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (13, 'StarSailComponent', 1, 4);
insert into required_component (chassis_id, component_type, level, amount) values (13, 'SensorComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (13, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Caravel', 100, 'CaravelHulls', 'SpaceShip',
    1, 5, 5, 3, 4, 0, 0, 3);
insert into required_component (chassis_id, component_type, level, amount) values (14, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (14, 'StarSailComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (14, 'SensorComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (14, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Corvette', 250, 'CorvetteHulls', 'SpaceShip',
    3, 6, 6, 3, 8, 0, 0, 3);
insert into required_component (chassis_id, component_type, level, amount) values (15, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (15, 'StarSailComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (15, 'SensorComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (15, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Frigate', 500, 'FrigateHulls', 'SpaceShip',
    6, 10, 10, 4, 16, 0, 0, 3);
insert into required_component (chassis_id, component_type, level, amount) values (16, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (16, 'StarSailComponent', 1, 3);
insert into required_component (chassis_id, component_type, level, amount) values (16, 'SensorComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (16, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Galleon', 1000, 'GalleonHulls', 'SpaceShip',
    12, 15, 15, 4, 32, 0, 0, 4);
insert into required_component (chassis_id, component_type, level, amount) values (17, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (17, 'StarSailComponent', 1, 3);
insert into required_component (chassis_id, component_type, level, amount) values (17, 'SensorComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (17, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('Man-of-war', 1500, 'ManOfWarHulls', 'SpaceShip',
    20, 20, 20, 4, 64, 0, 0, 5);
insert into required_component (chassis_id, component_type, level, amount) values (18, 'BridgeComponent', 1, 1);
insert into required_component (chassis_id, component_type, level, amount) values (18, 'StarSailComponent', 1, 4);
insert into required_component (chassis_id, component_type, level, amount) values (18, 'SensorComponent', 1, 2);
insert into required_component (chassis_id, component_type, level, amount) values (18, 'SupplyComponent', 1, 1);

insert into chassis (name, tonnage, technology, type, armour_slots, inner_slots, outer_slots, sensor_slots,
    weapon_slots, engine_slots, motive_slots, sail_slots) values ('SUV', 5, null, 'LandVehicle',
    0, 2, 1, 1, 0, 1, 1, 0);
insert into required_component (chassis_id, component_type, level, amount) values (19, 'MotiveComponent', 1, 1);

insert into faction (name, home_system, biologicals, mechanicals, chemicals) values ('Terrans', 1, 10000, 7500, 7500);
insert into faction (name, home_system, biologicals, mechanicals, chemicals) values ('Republic of Aclael', 2, 5000, 5000, 5000);
insert into user (ident, faction_id) values ('tuukka', 1);
insert into user_role (user_id, role) values (1, 'RoleAdministrator');

insert into building (planet_id, type, level, damage) values (3, 'SensorStation', 1, 0);
insert into building (planet_id, type, level, damage) values (3, 'ResearchComplex', 1, 0.25);
insert into building (planet_id, type, level, damage) values (3, 'ResearchComplex', 1, 0);
insert into building (planet_id, type, level, damage) values (3, 'Farm', 1, 0);

insert into star_system_report (star_system_id, name, coord_x, coord_y, faction_id, date) values (1, 'Sol', 0, 0, 1, 18000);
insert into star_system_report (star_system_id, name, coord_x, coord_y, faction_id, date) values (2, 'Barynth', 5.6, 2.2, 2, 18000);
