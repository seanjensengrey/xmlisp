(in-package :ccl)

(require 'cocoa)

(require 'easygui)

(load "ccl:cocoa-ide;defsystem.lisp")

(load "home:working copies;Xmlisp svn;trunk;XMLisp;sources;XMLisp-init")

;; If this is uncomented the framte-rate will be unbound
;(setf lui::*Framerate-Ceilling* nil)

(cl-user::build-agentcubes)

