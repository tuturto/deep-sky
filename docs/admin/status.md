Simulation status
=================

This panel is visible at all times at the top of admin view. It shows current
status of simulation and can be used to change the status or trigger processing
of next turn.

Status
------

There are four statuses:

 - Offline
 - Maintenance
 - Online
 - Processing turn

When simulation is offline or in maintenance mode, only administrators may
interact with it. In online mode regular users have full access to
simulation. Processing turn mode indicates that the system is currently
calculating results of current turn. In this mode regular users can only
browse the data (that might be changing as they do it), but they can't
do any edits or give any commands.

Turn processing
---------------

Turn processing is initiated by pressing the corresponding button. It will
first turn write access off for regular users and then start calculating
results of the current turn. As calculation finishes, simulation returns to
online state automatically.

[Back to admin](admin)
