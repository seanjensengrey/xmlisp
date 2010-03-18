;; Trivial OpenGL example
;; 8/25/09 Alexander Repenning
;; how to make a slow circle

(in-package :xlui)

(defclass CIRCLE-VIEW (opengl-dialog)
  ((radius :accessor radius :initform 1.0 :type float)
   (slices :accessor slices :initform 10 :type integer)))


(defmethod INIT ((Self circle-view))
  ;; (with-glcontext Self  )

#|  on OS X Leopard uncommenting will result in a recursion of #/run 
 
> Error: Stack overflow on temp stack.
> While executing: CCL::CHECK-NS-EXCEPTION, in process Initial(0).
> Type :POP to abort, :R for a list of available restarts.
> Type :? for other options.
1 > :b
 (BFFFF720) : 0 (CHECK-NS-EXCEPTION #<A Foreign Pointer [stack-allocated] (:* #) #xB0C10>) 336
 (BFFFF730) : 1 (FUNCALL #'#<(:OBJC-DISPATCH NEXTSTEP-FUNCTIONS:|run|)> ???) 392
 (BFFFF750) : 2 (FUNCALL #'#<(:OBJC-DISPATCH NEXTSTEP-FUNCTIONS:|run|)> ???) 640
 (BFFFF760) : 3 (EVENT-LOOP NIL) 280
1 > 

|#
   )
 

(defmethod DRAW ((Self circle-view))
  (let ((delta (/ (* (float pi 0.0) 2.0) (slices Self))))
    (glBegin GL_LINE_LOOP)
    (dotimes (i (slices Self))
      (let ((angle (* i delta)))
        (glVertex2f (* (cos angle) (radius Self)) (* (sin angle) (radius Self)))))
    (glEnd)))


<application-window title="circle, really?" y="50">
  <circle-view slices="3"/>
</application-window>


<application-window title="circle, kinda?" y="300">
  <circle-view slices="6"/>
</application-window>


<application-window title="circle" y="550">
  <circle-view slices="40"/>
</application-window>


#| Examples:

(add-subview 
 (make-instance 'window :width 300 :height 300)
 (make-instance 'circle-view :width 300 :height 300))

|#