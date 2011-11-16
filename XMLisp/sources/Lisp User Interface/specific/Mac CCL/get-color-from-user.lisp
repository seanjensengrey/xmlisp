(in-package :lui)

(export '(get-color-from-user))

(defun GET-COLOR-FROM-USER (&key (Shows-Alpha t) (Red 1.0) (Green 0.0) (Blue 0.0) (Alpha 1.0)) "
  Make user pick an RGBA value"
  #+cocotron (declare (ftype function xlui::get-windows-color-picker))
  #-cocotron
  (progn
    (#/setShowsAlpha: (#/sharedColorPanel ns:ns-color-panel) Shows-Alpha)
    (#/setColor: 
     (#/sharedColorPanel ns:ns-color-panel)
     (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color Red Green Blue Alpha))
    (let ((Color (#/colorUsingColorSpaceName: (easygui::user-pick-color) #$NSCalibratedRGBColorSpace)))
      (rlet ((r #>CGFloat)
             (g #>CGFloat)
             (b #>CGFloat)
             (a #>CGFloat))
        (#/getRed:green:blue:alpha: Color r g b a)
        (values 
         (float (lui::pref r #>CGFloat) 0.0)
         (float (lui::pref g #>CGFloat) 0.0)
         (float (lui::pref b #>CGFloat) 0.0)
         (float (lui::pref a #>CGFloat) 0.0)))))
  #+cocotron
  ;; need to implement a real color picker
  (let ((color (xlui::get-windows-color-picker :red red :green green :blue blue :alpha alpha)))
    (when color
      (values (first color) (second color) (third color) (fourth color) ))))


#| Examples:

(get-color-from-user)

(get-color-from-user :shows-alpha nil)

|#