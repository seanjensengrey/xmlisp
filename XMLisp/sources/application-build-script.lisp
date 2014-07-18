(in-package :ccl)

(require 'cocoa)

#-cocotron
(require 'easygui)

(load "ccl:cocoa-ide;defsystem.lisp")

#-cocotron
(load "home:working copies;Deployment;xmlisp;trunk;XMLisp;sources;XMLisp-init")

#+cocotron
(setf (logical-pathname-translations "virtual")
`((,(make-pathname :host "virtual"
                   :directory '(:absolute :wild-inferiors)
                   :name :wild
                   :type :wild
                   :version :wild)
   #p"x:/**/*.*")))

#+cocotron 
(load "virtual:Deployment;xmlisp;trunk;XMLisp;sources;XMLisp-init")
;; If this is uncomented the framte-rate will be unbound
;(setf lui::*Framerate-Ceilling* nil)

(cl-user::build-agentcubes)
