insert into time (current_time) values (20191);

insert into star_system (name, coord_x, coord_y) values ('Sol', 0, 0);
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Sun', 1, 'G', 'V');
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Mercury', 1, 1, NULL, 0.38);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Venus', 2, 1, NULL, 0.904);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Earth', 3, 1, 1, 1.0);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Mars', 4, 1, NULL, 0.376);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Jupiter', 6, 1, NULL, 2.528);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Saturn', 7, 1, NULL, 1.065);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Uranus', 8, 1, NULL, 0.886);
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Neptune', 9, 1, NULL, 1.14);

insert into star_system (name, coord_x, coord_y) values ('Aclael', 4.2, -1.2);
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Aclael alpha', 2, 'A', 'II');
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Aclael beta', 2, 'M', 'VI');
insert into planet (name, position, star_system_id, owner_id, gravity) values ('Aclael I', 1, 2, NULL, 0.9);

insert into star_system (name, coord_x, coord_y) values ('Barynth', 5.6, 2.2);
insert into star (name, star_system_id, spectral_type, luminosity_class) values ('Barynth alpha', 3, 'L', 'VI');

insert into star_lane (star_system1, star_system2) values (1, 2);
insert into star_lane (star_system1, star_system2) values (2, 3);

insert into faction (name, home_system, biologicals, mechanicals, chemicals) values ('Terrans', 1, 10000, 7500, 7500);
insert into faction (name, home_system, biologicals, mechanicals, chemicals) values ('Republic of Aclael', 2, 5000, 5000, 5000);
insert into user (ident, faction_id) values ('tuukka', 1);
insert into user_role (user_id, role) values (1, 'RoleAdministrator');

insert into building (planet_id, type, level, construction, damage) values (3, 'SensorStation', 1, 1.0, 0);
insert into building (planet_id, type, level, construction, damage) values (3, 'ResearchComplex', 1, 1.0, 0.25);
insert into building (planet_id, type, level, construction, damage) values (3, 'ResearchComplex', 1, 0.5, 0);

insert into star_system_report (star_system_id, name, coord_x, coord_y, faction_id, date) values (1, 'Sol', 0, 0, 1, 20183);
insert into star_report (star_id, star_system_id, name, spectral_type, luminosity_class, faction_id, date) values (1, 1, 'Sun', 'G', NULL, 1, 20183);
insert into star_report (star_id, star_system_id, name, spectral_type, luminosity_class, faction_id, date) values (1, 1, 'Sun', 'G', 'V', 1, 20184);

insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (1, 1, NULL, 'Mercury', 1, NULL, 1, 20182);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (2, 1, NULL, 'Venus', 2, NULL, 1, 20182);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (2, 1, NULL, 'Venus', 2, 0.904, 1, 20183);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (3, 1, 1, 'Earth', 3, 0.99, 1, 20181);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (3, 1, 1, 'Earth', 3, 1, 1, 20182);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (4, 1, NULL, 'Mars', 4, NULL, 1, 20183);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (5, 1, NULL, 'Jupiter', 6, NULL, 1, 20182);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (6, 1, NULL, 'Saturn', 7, NULL, 1, 20182);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (7, 1, NULL, 'Uranus', 8, NULL, 1, 20182);
insert into planet_report (planet_id, star_system_id, owner_id, name, position, gravity, faction_id, date) values (8, 1, NULL, 'Neptune', 9, NULL, 1, 20182);

insert into star_system_report (star_system_id, name, coord_x, coord_y, faction_id, date) values (2, 'Aclael', 4.2, -1.2, 1, 2018.3);
insert into star_report (star_id, star_system_id, name, spectral_type, luminosity_class, faction_id, date) values (2, 2, 'Aclael alpha', 'A', NULL, 1, 20183);
insert into star_report (star_id, star_system_id, name, spectral_type, luminosity_class, faction_id, date) values (3, 2, 'Aclael beta', 'M', NULL, 1, 20184);

insert into star_lane_report (star_system1, star_system2, star_system_name1, star_system_name2, faction_id, date) values (1, 2, 'Sol', 'Aclael', 1, 20184);
insert into star_lane_report (star_system1, star_system2, star_system_name1, star_system_name2, faction_id, date) values (2, 3, 'Aclael', NULL, 1, 20184);

insert into building_report (building_id, planet_id, type, level, construction, damage, faction_id, date) values (1, 3, 'SensorStation', 1, 1.0, 0, 1, 20184);
insert into building_report (building_id, planet_id, type, level, construction, damage, faction_id, date) values (2, 3, 'ResearchComplex', 1, 1.0, 0.25, 1, 20184);
insert into building_report (building_id, planet_id, type, level, construction, damage, faction_id, date) values (3, 3, 'ResearchComplex', 1, 0.5, 0, 1, 20184);
