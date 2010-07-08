(in-package :lui)

(defun GET-COLOR-FROM-USER ()
  #-cocotron 
  (let ((Color (easygui::user-pick-color)))
        (rlet ((r #>CGFloat)
               (g #>CGFloat)
               (b #>CGFloat)
               (a #>CGFloat))
          (#/getRed:green:blue:alpha: Color r g b a)         
          `(,(truncate (* (lui::pref r #>CGFloat) 255)) ,(truncate (* (lui::pref g #>CGFloat) 255)) ,(truncate (* (lui::pref b #>CGFloat) 255)) ,(truncate (* (lui::pref a #>CGFloat) 255)) )))
  )


#| Examples:

(get-color-from-user)

|#