(in-package :lui)


(export '(get-color-from-user))

(defun GET-COLOR-FROM-USER (&key (Shows-Alpha t)) "
  Make user pick an RGBA value"
  #-cocotron
  (progn
    (#/setShowsAlpha: (#/sharedColorPanel ns:ns-color-panel) Shows-Alpha)
    (let ((Color (easygui::user-pick-color)))
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
  (values 1.0 1.0 1.0 1.0))


#| Examples:

(get-color-from-user)
(get-color-from-user :shows-alpha nil)

|#