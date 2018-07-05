insert into star_system (name, coord_x, coord_y) values ('Sol', 0, 0);
insert into star (name, star_system_id, spectral_Type, luminosity_class) values ('Sun', 1, 'G', 'V');
insert into planet (name, position, star_system_id, gravity) values ('Mercury', 1, 1, 0.38);
insert into planet (name, position, star_system_id, gravity) values ('Venus', 2, 1, 0.904);
insert into planet (name, position, star_system_id, gravity) values ('Earth', 3, 1, 1.0);
insert into planet (name, position, star_system_id, gravity) values ('Mars', 4, 1, 0.376);
insert into planet (name, position, star_system_id, gravity) values ('Jupiter', 6, 1, 2.528);
insert into planet (name, position, star_system_id, gravity) values ('Saturn', 7, 1, 1.065);
insert into planet (name, position, star_system_id, gravity) values ('Uranus', 8, 1, 0.886);
insert into planet (name, position, star_system_id, gravity) values ('Neptune', 9, 1, 1.14);

insert into user (ident) values ('tuukka');

insert into star_system_report (star_system_id, name, coord_x, coord_y, user_id, date) values (1, 'Sol', 0, 0, 1, '2018-07-03');
insert into star_report (star_id, star_system_id, name, spectral_type, user_id, date) values (1, 1, 'Sun', 'G', 1, '2018-07-03');

insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (1, 1, 'Mercury', 1, NULL, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (2, 1, 'Venus', 2, NULL, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (2, 1, 'Venus', 2, 0.904, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (3, 1, 'Earth', 3, 0.99, 1, '2018-07-03');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (3, 1, 'Earth', 3, 1, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (4, 1, 'Mars', 4, NULL, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (5, 1, 'Jupiter', 6, NULL, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (6, 1, 'Saturn', 7, NULL, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (7, 1, 'Uranus', 8, NULL, 1, '2018-07-04');
insert into planet_report (planet_id, star_system_id, name, position, gravity, user_id, date) values (8, 1, 'Neptune', 9, NULL, 1, '2018-07-04');
