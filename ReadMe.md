FsSpaceInvaders-Imperative
==========================

A slightly-shitty space invaders.

I'm not really that interested in how good this is as space invaders!
This is a "game development in F#" spike program.

This version uses *mutable* state.

Intentions
----------
This is in F#, and is intended to target Windows and Linux via .Net
Core, using the "SDL2" library, and the "SDL2-CS" interop library.

It is also intended to be recompiled with the F# Fable compiler to
target Javascript, which will require the SDL2 back-end stripping
off and replacing with HTML Canvas (I think -- unless advised
otherwise).  I would also need to find out best practice 
recommendations for games in the browser, eg: timers / input etc...

Status
------
Only Windows .NET is developer-tested.
Linux is untried at the time of writing.
Fable/JS back-end code isn't started (no code).

Framework Notes
---------------
This was developed in .Net Framework, but then I manually switched it
to .Net Core by fiddling some files.  These fiddlings may have left
behind configuration artefacts I still need to remove.  Possibly!

Future work
-----------
The FsSpaceInvaders-Functional presents a more pure-functional
approach to this program, with some enhancements.  

I do not think I will come back to this program, but I am 
recording it in any case.

