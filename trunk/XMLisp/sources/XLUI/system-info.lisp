(in-package :xlui)

(defun SYSTEM-INFO ()
  #-cocotron
  (let ((Process-Info (#/alloc ns:ns-process-info)))
    (values 
     (case (#/operatingSystem Process-Info)
       (1 "Windows NT")
       (2 "Windows 95")
       (3 "Solaris")
       (4 "HPUX")
       (5 "Mac OS X")
       (6 "Sun OS")
       (7 "OSF10")
       (t "????"))
     (ccl::lisp-string-from-nsstring (#/operatingSystemVersionString Process-Info))
     (#/activeProcessorCount Process-Info)
     (#/physicalMemory Process-Info))))

#|
(defun SYSTEM-INFO ()
  (let ((Process-Info (#/alloc ns:ns-process-info)))
    (values 
     (#/operatingSystemName Process-Info)
     (#/operatingSystem Process-Info)
     (#/operatingSystemVersionString Process-Info)
     (#/activeProcessorCount Process-Info)
     (#/physicalMemory Process-Info))))
|#

; (system-info)