# XMLisp

eXtreme Media Lisp: Rich media cross-platform programming for 3D (OpenGL) and 2D applications

This project was imported from http://code.google.com/p/xmlisp

* 3D: Open Agent Engine: scene graph based mini engine, 3D objects, animation, drag and drop, mouse hovering, picking, selection, camera control, texture management. Full access to OpenGL functions including shaders (GLSL).
* 2D: controls: layout managers, buttons, sliders, text
* Media: Sound, Text-to-Speech (OS X only)
* Events: mouse, keyboard, multi touch, gestures (OS X 10.6 and later): pinch, rotate, scroll
* IDE: a simple development environment including symbol completion to edit, run and debug code

XMLisp simulations and animations run as fast compiled code in native threads. Run multiple simulations at the same time, resize windows, move windows, move the camera, browse web pages, .... your simulation continues as smooth as possible.

## Philosophy

Conceptually the 3D part of XMLisp, that is the Open Agent Engine, is somewhere
between scene graph (e.g., Java3D) and direct mode rendering (JOGL). Simple
applications consist of a tree structure of agents such as cubes, planes, and
spheres. No OpenGL code needs to be written for these. But if you need
specialized rendering you create subclasses of agents and extend their draw
method using OpenGL calls from basic polygons to shaders.

## License

LGPL - GNU Lesser GPL
