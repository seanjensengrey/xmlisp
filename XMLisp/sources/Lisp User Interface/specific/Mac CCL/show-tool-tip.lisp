;;; Basic API to show and hide tool tips on the screen
;;; 10/9/2011 Alexander Repenning

(in-package :lui)


(export '(show-tool-tip hide-tool-tip))


(defgeneric SHOW-TOOL-TIP (string x y)
  (:documentation "Show tool tip <string> at position <x>, <y> on screen. Use hide-tool-tip to remove."))

;************************************
;* Implementation                   *
;************************************

;; Use a borderless NSWindow

(defvar *Tool-Tip-Window* nil)


(defvar *Tool-Tip-Background-Color* nil)


(defun TOOL-TIP-BACKGROUND-COLOR ()
  (or *Tool-Tip-Background-Color*
      (setq *Tool-Tip-Background-Color* 
            (#/yellowColor ns:ns-color))))


(defvar *TOOL-TIP-Font-Color* nil "color used for anticipatory text")


(defun TOOL-TIP-FONT-COLOR ()
  (or *TOOL-TIP-Font-Color*
      (setf *TOOL-TIP-Font-Color* (#/blackColor ns:ns-color))))


(defclass TOOL-TIP-VIEW (ns:ns-view)
  ((text-attributes :accessor text-attributes :foreign-type :id)
   (text :accessor text :foreign-type :id))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/drawRect: :void) ((self tool-tip-view) (rect :<NSR>ect))
  (ccl::with-autorelease-pool 
    (#/set (#/clearColor ns:ns-color))
    (#_NSRectFill (#/bounds self))
    (ns:with-ns-point (point 0 1)
      (#/drawAtPoint:withAttributes: (slot-value self 'text)
                                     point
                                     (slot-value self 'text-attributes)))))


(defun TOOL-TIP-WINDOW () "
  Return current overlay window used for symbol completion. 
  Create one if needed."
  (or *Tool-Tip-Window*
      (setq *Tool-Tip-Window*
            (ns:with-ns-rect (frame 100 100 400 40)
              (let* ((w (make-instance 'ns:ns-window
                          :with-content-rect frame
                          :style-mask #$NSBorderlessWindowMask
                          :backing #$NSBackingStoreBuffered
                          :defer #$YES))
                     (view (make-instance 'tool-tip-view
                             :with-frame (#/frame (#/contentView w))))
                     ;; Create attributes to use in window
                     (attribs (make-instance 'ns:ns-mutable-dictionary :with-capacity 3)))
                (#/setObject:forKey: attribs (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font))
                                     #&NSFontAttributeName)
                (#/setObject:forKey: attribs (tool-tip-font-color)
                                     #&NSForegroundColorAttributeName)
                (#/setObject:forKey: attribs (tool-tip-background-color)
                                     #&NSBackgroundColorAttributeName)
                (setf (slot-value view 'text-attributes) (#/retain attribs))
                (setf (slot-value view 'text) (#/retain (gui::%make-nsstring "")))
                (#/setContentView: w view)
                (#/setHasShadow: w #$NO)  ;; YES would leave a trace in Windows 7
                ;; Set the background color to clear so that (along with the setOpaque call below)
                ;; we can see through the parts of the window that we're not drawing into
                (#/setBackgroundColor: w (#/clearColor ns:ns-color))
                ;; No transparency for actual drawing into the window
                (#/setAlphaValue: w (gui::cgfloat 1.0))
                ;; Allow see through the parts of the window we're not drawing into
                (#/setOpaque: w #$YES)
                ;; Make it as unobtrusive as possible
                (#/setIgnoresMouseEvents: w #$YES)
                (#/setExcludedFromWindowsMenu: w #$YES)
                (#/setHidesOnDeactivate: w #$YES)
                w)))))


(defmethod SHOW-TOOL-TIP ((String string) x y)
  "Show text at screen position"
  (ccl::with-autorelease-pool 
      (let* ((w (tool-tip-window))
             (overlay (#/contentView w))
             (nsstring (ccl::%make-nsstring String)))
        (with-slots (text) overlay
          (#/release text)
          (setf text (#/retain nsstring)))
        ;; window should be size of tool tip text
        (let ((Size (#/sizeWithAttributes: (text overlay) (text-attributes overlay))))
          (#/setContentSize: w Size)
          ;; adjust position to avoid 
          (ns:with-ns-point (Position (min x 
                                           (- (pref (#/frame (#/mainScreen ns:ns-screen)) <NSR>ect.size.width) 
                                              (rref Size <NSS>ize.width)
                                              10))  ;; psychological margin to make people not think they are missing part of the tool tip
                                      y)
            (#/setFrameOrigin: w Position)))
        ;; (#/invalidateShadow w)
        (#/display w)
        (#/orderFront: w ccl:+null-ptr+))))


(defun HIDE-TOOL-TIP ()
  "Hide the overlay window"
  (let ((w (tool-tip-window)))
    (when (#/isVisible w)
      (#/orderOut: w ccl:+null-ptr+))))



#| examples:

(show-tool-tip "This is helpful" 100 100)
(hide-tool-tip)



(show-tool-tip "This is a very long tool that which could be too long for the screen and cause a problem with the user getting confused" 2400 100)
(hide-tool-tip)


(setq *Tool-Tip-Window* nil) 

|#
