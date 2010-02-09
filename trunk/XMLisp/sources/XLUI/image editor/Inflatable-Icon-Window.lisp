;;-*- Mode: Lisp; Package: AD3D -*-
;*********************************************************************
;*                                                                   *
;*    I N F L A T A B L E    I C O N   E D I T O R   W I N D O W     *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2007, AgentSheets Inc.                    *
   ;* Filename  : Inflatable-Icon-window.lisp                        *
   ;* Updated   : 08/16/07                                           *
   ;* Version   :                                                    *
   ;*    1.0    : 07/20/06                                           *
   ;*    1.1    : 01/04/07 Save/restore                              *
   ;*    1.2    : 05/30/07 Selection based inflation                 *
   ;*    1.2.1  : 08/16/07 if inflatable-icon has icon slot use      *
   ;*                       as image filename for image.             *
   ;* HW/SW     : G4, OS X 10.4.10, MCL 5.2                          *
   ;* Abstract  : Editor for inflatable icons                        *
   ;* Portable  : white                                              *
   ;*                                                                *
   ;******************************************************************

(in-package :ad3d)


(defclass INFLATABLE-ICON-EDITOR-WINDOW (application-window)
  ((smoothing-cycles :accessor smoothing-cycles :initform 0 :initarg :smoothing-cycles)
   (file :accessor file :initform nil :documentation "shape file")
   (destination-inflatable-icon :accessor destination-inflatable-icon :initform nil :initarg :destination-inflatable-icon :documentation "if present save edited icon into this inflatable icon")
   (close-action :accessor close-action :initform nil :initarg :close-action :documentation "called with self when inflatable icon window is being closed"))
  (:documentation "Editor used to create inflatable icons"))


(defmethod WINDOW-NULL-EVENT-HANDLER ((Self inflatable-icon-editor-window))
  "Called periodically when the window has the focus."
  (let ((Icon-Editor (or (view-named 'icon-editor Self))))
    (when Icon-Editor
      ;; do animation only when there is selection
      (when (or (selection-in-progress Icon-Editor)
                (selection-outline Icon-Editor))
        (animate Icon-Editor 0.06)
        (view-draw-contents Icon-Editor))
      ;; update preview color when eyedropper tool is selected
      (when (eql (selected-tool Self) 'eye-dropper)
        (multiple-value-bind (Col Row) 
                             (screen->pixel-coord Icon-Editor (view-mouse-position Icon-Editor))
          (when (col-row-within-bounds-p Icon-Editor Col Row)
            (multiple-value-bind (Red Green Blue) (get-rgba-color-at Icon-Editor Col Row)
              (draw-preview-color (view-named 'color-swatch Self) (make-color (ash Red 8)
                                                                              (ash Green 8)
                                                                              (ash Blue 8))))))))
    (call-next-method)))



(defmethod WINDOW-CLOSE-EVENT-HANDLER ((Self inflatable-icon-editor-window))
  (close-window-with-warning Self))


(defmethod SELECT-ALL ((Self inflatable-icon-editor-window))
  (select-all (view-named 'icon-editor Self)))


(defmethod VIEW-KEY-EVENT-HANDLER ((Self inflatable-icon-editor-window) Key)
  "Called when a key is typed while the image-editor-window has keyboard focus."
  (let ((Icon-Editor (view-named 'icon-editor Self)))
    (cond
     ((command-key-p)
      (case Key
        (#\l (load-image-from-file Self (choose-file-dialog :directory "ad3d:resources;textures;")))
        (#\a (select-all Icon-Editor))
        (#\d (clear-selection Icon-Editor))
        (#\I (invert-selection Icon-Editor))))
     (t
      (case Key
        (#\ESC (close-window-with-warning Self))
        (#\Delete 
         (cond
          ((option-key-p)
           (fill-selected-pixels Icon-Editor)
           (image-changed-event Icon-Editor))
          (t
           (erase-selected-pixels Icon-Editor)
           (image-changed-event Icon-Editor)))))))
    (call-next-method)))


(defmethod UPDATE-INFLATION ((Self inflatable-icon-editor-window))
  ;; need to update model editor
  (let ((Icon-Editor (or (view-named 'icon-editor (view-window Self)) (error "icon editor missing")))
        (Model-Editor (or (view-named 'model-editor (view-window Self)) (error "model editor missing"))))
    (let ((Inflatable-Icon (inflatable-icon Model-Editor)))
      ;; make sure model and icon editor image buffers are aligned
      (unless (image Inflatable-Icon)
        (get-rgba-color-at Icon-Editor 0 0)   ;; hack: make sure pixel buffer exists by accessing it once
        (setf (image Inflatable-Icon)
              (pixel-buffer Icon-Editor)))
      (labels ((pixel-selected-p-fn (Row Column) 
                 (or (not (selection-outline Icon-Editor))
                     (pixel-selected-p 
                      (selection-mask Icon-Editor)
                      Column
                      (- (height (selection-mask Icon-Editor)) Row)))))
        ;; inflate
        (inflate
         Inflatable-Icon
         :steps 10
         :pressure (pressure Inflatable-Icon)
         :max (max-value Inflatable-Icon)
         :inflate-pixel-p-fn #'pixel-selected-p-fn)
        ;; introduce noise
        (inflate
         Inflatable-Icon
         :steps 1
         :pressure (pressure Inflatable-Icon)
         :max (max-value Inflatable-Icon)
         :noise (noise Inflatable-Icon)
         :inflate-pixel-p-fn #'pixel-selected-p-fn)
        (dotimes (I (smoothing-cycles Self))
          (inflate
           Inflatable-Icon
           :steps 1
           :pressure (pressure Inflatable-Icon)
           :max (max-value Inflatable-Icon)
           :inflate-pixel-p-fn #'pixel-selected-p-fn))
        ;; update 
        (view-draw-contents Model-Editor)))))


(defmethod LOAD-IMAGE-FROM-FILE ((Self inflatable-icon-editor-window) Pathname)
  "Loads the specified image file into the editor window."
  (load-image (image-editor-view Self) Pathname)
  (setf (file Self) Pathname))


(defmethod SAVE-IMAGE-TO-FILE ((Self inflatable-icon-editor-window) Pathname)
  "Saves the current image in the editor window to the specified file."
  (save-image (image-editor-view Self) Pathname))

;______________________________________________
; File menu support                            |
;______________________________________________

(defmethod DOCUMENT-TYPE-NAME ((Self inflatable-icon-editor-window))
  "inflatable icon")


(defmethod DOCUMENT-TYPE-FILE-EXTENSION ((Self inflatable-icon-editor-window))
  "shape")

(defmethod WINDOW-NEEDS-SAVING-P ((Self inflatable-icon-editor-window))
  t)


(defmethod DOCUMENT-ROOT ((Self inflatable-icon-editor-window))
  ;; access by name
  (inflatable-icon (view-named 'model-editor Self)))


(defmethod IMAGE-FILE-NAME ((Self inflatable-icon-editor-window))
  (when (file Self)
    (format nil "~A.png" (pathname-name (file Self)))))


(defmethod IMAGE-FILE ((Self inflatable-icon-editor-window))
  (when (file Self)
    ;; allow icon of inflatable icon to overwrite the default image file name+extension
    (if (and (document-root Self) (icon (document-root Self)))
      (make-pathname
       :directory (pathname-directory (file Self))
       :name (filename-name (icon (document-root Self)))
       :type (filename-extension (icon (document-root Self)))
       :host (pathname-host (file Self))
       :defaults (file Self))
      (make-pathname
       :directory (pathname-directory (file Self))
       :name (pathname-name (file Self))
       :type "png"
       :host (pathname-host (file Self))
       :defaults (file Self)))))


(defmethod WINDOW-FILENAME ((Self inflatable-icon-editor-window))
  ;; needed for proxy icons
  (file Self))


(defmethod WINDOW-SAVE-AS ((Self inflatable-icon-editor-window) &optional External-Format)
  (declare (ignore External-Format))
  (let ((File (choose-new-file-dialog 
               :prompt nil
               :name (format nil "untitled.~A" (document-type-file-extension Self))
               :window-title (format nil "Save ~A As:" (document-type-name Self))
               :directory (document-default-directory Self))))
    (setf (file Self) File)
    (setf (icon (document-root Self)) (image-file-name Self))     ;; set icon attribute
    (store-window-state-in-document-root Self)
    (save-object (document-root Self) File :if-exists :error)
    (add-window-proxy-icon Self File)
    ;; set file to this new file
    (setf (window-needs-saving-p Self) nil)
    (save-image (view-named 'icon-editor Self) (image-file Self))))  ;; save image


(defmethod WINDOW-SAVE :around ((Self inflatable-icon-editor-window))
  (cond
   ;; saved before
   ((file Self) 
    ;; do not overwrite existing icon name
    (unless (icon (document-root Self))
      (setf (icon (document-root Self)) (image-file-name Self)))     ;; set icon attribute
    (call-next-method)
    (save-image (view-named 'icon-editor Self) (image-file Self))  ;; save image
    (store-window-state-in-document-root Self)
    (save-object (document-root Self) (file Self) :if-exists :supersede))
   ;; never saved
   (t 
    ;; indirectly call window save as
    (call-next-method))))

  

;***********************************************
; XMLisp GUI Components                        *
;***********************************************

;________________________________________________
;  Icon Editor                                   |
;________________________________________________

(defclass ICON-EDITOR (image-editor xml-dialog-interface)
  ((dialog-item-action-function :accessor dialog-item-action-function :initform nil))
  (:default-initargs
    :view-position #@(0 0)
    :view-size #@(200 200))
  (:documentation "Edit icons with me"))


(defmethod INSTALL-VIEW-IN-WINDOW :after ((Self icon-editor) Window)
  (declare (ignore Window))
  ;; center camera straight over image
  (setf (camera Self) 
        (copy <camera eye-x="0.0" eye-y="0.0" eye-z="1.7379" 
              center-x="0.0" center-y="0.0" center-z="0.0" 
              up-x="0.0" up-y="1.0" up-z="0.0" fovy="60.0" 
              near="0.005" far="2000.0" azimuth="0.0" zenith="0.0"/>))
  (setf (view (camera Self)) Self)
  (init Self)
  ;; create the texture
  (new-image Self (img-width Self) (img-height Self)))


(defmethod PRINT-SLOTS ((Self icon-editor))
  '(view-position view-size img-height img-width))


(defmethod IMAGE-CHANGED-EVENT ((Self icon-editor) &optional Column1 Row1 Column2 Row2)
  (declare (ignore Column1 Row1 Column2 Row2))
  (invoke-action Self))


;________________________________________________
;  Lobster Icon Editor                           |
;________________________________________________


(defclass LOBSTER-ICON-EDITOR (icon-editor)
  ()
  (:documentation "Demo specific Lobster icon"))


(defmethod INSTALL-VIEW-IN-WINDOW :after ((Self lobster-icon-editor) Window)
  (declare (ignore Window))
  ;; center camera straight over image
  (setf (camera Self) 
        (copy <camera eye-x="0.0" eye-y="0.0" eye-z="1.7379" 
              center-x="0.0" center-y="0.0" center-z="0.0" 
              up-x="0.0" up-y="1.0" up-z="0.0" fovy="60.0" 
              near="0.005" far="2000.0" azimuth="0.0" zenith="0.0"/>))
  (setf (view (camera Self)) Self)
  (init Self)
  ;; create the texture
  (load-image Self "ad3d:resources;images;redlobster.png"))


;________________________________________________
;  Inflated-Icon-Editor                          |
;________________________________________________

(defclass INFLATED-ICON-EDITOR (opengl-view xml-layout-interface)
  ((inflatable-icon :accessor inflatable-icon :initarg :inflatable-icon))
  (:default-initargs
    :use-global-gl-context t
    :inflatable-icon (make-instance 'inflatable-icon :auto-compile nil))
  (:documentation "3d inflated icon editor"))


(defmethod INIT ((Self inflated-icon-editor))
  (call-next-method)
  (glShadeModel gl_smooth)
  ;; define material
  (with-rgba-vector Specular (0.5 0.5 0.5 0.0)
    (glmaterialfv gl_front_and_back gl_specular Specular))
  (glmaterialf gl_front_and_back gl_shininess 20.0)
  (with-rgba-vector V (1.0 1.0 1.0 1.0)
    (glmaterialfv gl_front_and_back gl_ambient_and_diffuse V))
  ;; light
  (with-rgba-vector Position (0.0 5.0 5.0 1.0)
    (gllightfv gl_light0 gl_position Position))
  (with-rgba-vector White-Light (1.0 1.0 1.0 1.0)
    (gllightfv gl_light0 gl_diffuse White-Light))
  (with-rgba-vector Specular-Light (1.0 1.0 1.0 1.0)
    (gllightfv gl_light0 gl_specular Specular-Light))
  ;; enablers
  (glenable gl_lighting)
  (glenable gl_light0)
  (glenable gl_depth_test)
  ;; alpha
  (glEnable gl_blend)
  (glBlendFunc gl_src_alpha gl_one_minus_src_alpha)
  ;; top down 45 degree angle view
  (setf (camera Self)
        (copy <camera eye-x="0.0" eye-y="0.89" eye-z="1.0" center-x="0.0" center-y="0.0" center-z="0.0" up-x="0.0" up-y="0.5726468643072211" up-z="-1.5649032618904695" fovy="60.0" aspect="1.0" near="0.004999999888241291" far="2000.0" azimuth="0.0" zenith="0.7200000286102295"/>))
  (setf (view (camera Self)) Self))


(defmethod INITIALIZE-LAYOUT :after ((Self inflated-icon-editor))
  ;; initialize based on values of icon-editor
  ;; (print "initializing inflated icon editor")
  (let ((Icon-Editor (view-named 'icon-editor (view-window Self))))
    (unless Icon-Editor (error "cannot find Icon Editor"))
    ;; (setf (image (inflatable-icon Self)) (pixel-buffer Icon-Editor))
    (setf (altitudes (inflatable-icon Self))
          (make-array (list (img-height Icon-Editor) (img-width Icon-Editor))
                      :element-type 'short-float
                      :initial-element 0s0))))
  
  
(defmethod DISPLAY-SKY-BOX ((Self inflated-icon-editor))
  (glenable gl_texture_2d)
  (glEnable gl_cull_face) ;; cull to see through walls
  (gltexenvi gl_texture_env gl_texture_env_mode gl_modulate)
  (use-texture Self "floorTile.png")
  (gltexparameteri gl_texture_2d gl_texture_wrap_s gl_repeat)
  (gltexparameteri gl_texture_2d gl_texture_wrap_t gl_repeat)
  (glbegin gl_quads)
  ;; floor
  (glnormal3f 0.0 1.0 0.0)
  (gltexcoord2f 0s0 0s0) (glvertex3f -1s0 0s0 -1s0)
  (gltexcoord2f 0s0 10s0) (glvertex3f -1s0 0s0 1s0)
  (gltexcoord2f 10s0 10s0) (glvertex3f  1s0 0s0 1s0)
  (gltexcoord2f 10s0 0s0) (glvertex3f  1s0 0s0 -1s0)
  ;; wall 1
  (glnormal3f 0.0 0.0 1.0)
  (gltexcoord2f 10.0 0.0) (glvertex3f  1s0 0s0 -1s0)
  (gltexcoord2f 10.0 10.0) (glvertex3f  1s0  2s0 -1s0)
  (gltexcoord2f 0.0 10.0) (glvertex3f -1s0  2s0 -1s0)
  (gltexcoord2f 0.0 0.0) (glvertex3f -1s0 0s0 -1s0)
  ;; wall 2
  (glnormal3f 0.0 0.0 -1.0)
  (gltexcoord2f 0.0 0.0) (glvertex3f -1s0 0s0 1.0)
  (gltexcoord2f 0.0 10.0) (glvertex3f -1s0  2s0 1.0)
  (gltexcoord2f 10.0 10.0) (glvertex3f  1s0  2s0 1.0)
  (gltexcoord2f 10.0 0.0) (glvertex3f  1s0 0s0 1.0)
  ;; wall 3
  (glnormal3f  -1.0 0.0 0.0)
  (gltexcoord2f 0s0 0s0) (glvertex3f 1s0 0s0 -1s0)
  (gltexcoord2f 0s0 10s0) (glvertex3f 1s0 0s0  1s0)
  (gltexcoord2f 10s0 10s0) (glvertex3f 1s0 2s0  1s0)
  (gltexcoord2f 10s0 0s0) (glvertex3f 1s0 2s0 -1s0)
  ;; wall 4
  (glnormal3f  1.0 0.0 0.0)
  (gltexcoord2f 10s0 0s0) (glvertex3f -1s0 2s0 -1s0)
  (gltexcoord2f 10s0 10s0) (glvertex3f -1s0 2s0  1s0)
  (gltexcoord2f 0s0 10s0) (glvertex3f -1s0 0s0  1s0)
  (gltexcoord2f 0s0 0s0) (glvertex3f -1s0 0s0 -1s0)
  (glend)
  
  (glDisable gl_cull_face))

  
(defmethod DISPLAY ((Self inflated-icon-editor)) 
  (glClearColor 0.9 0.9 0.9 1.0) 
  (glClear (logior GL_COLOR_BUFFER_BIT gl_depth_buffer_bit))
  (display-sky-box Self)
  (glpushmatrix)
  (glTranslatef -0.5s0 +0.01s0 0.5s0)
  (glRotatef -90s0 1.0s0 0.0s0 0.0s0)
  (display (inflatable-icon Self))
  (glpopmatrix))
  

;*************************************************
;  Component Actions                             *
;*************************************************


;; Tool Bar Actions

(defmethod DRAW-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'draw))


(defmethod ERASE-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'erase))


(defmethod EYE-DROPPER-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'eye-dropper))


(defmethod PAINT-BUCKET-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'paint-bucket))


(defmethod MAGIC-WAND-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'magic-wand))


(defmethod POLYGON-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'select-polygon))


(defmethod RECT-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'select-rect))


(defmethod ELLIPSE-TOOL-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (tool-selection-event Window 'select-ellipse))


(defmethod COLOR-WELL-ACTION ((Window inflatable-icon-editor-window) Color-Well)
  (multiple-value-bind (R G B Alpha)
                       (pen-color (view-named 'icon-editor Window))
    (declare (ignore R G B))
    (set-pen-color
     (view-named 'icon-editor Window)
     (ash (color-red (selected-color Color-Well)) -8)
     (ash (color-green (selected-color Color-Well)) -8)
     (ash (color-blue (selected-color Color-Well)) -8)
     Alpha)))


(defmethod MIRROR-NONE-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (toggle-mirror-lines (view-named 'icon-editor Window) nil nil))


(defmethod MIRROR-VERTICALLY-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (toggle-mirror-lines (view-named 'icon-editor Window) nil t))


(defmethod MIRROR-HORIZONTALLY-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (toggle-mirror-lines (view-named 'icon-editor Window) t nil))


(defmethod MIRROR-BOTH-ACTION ((Window inflatable-icon-editor-window) Button)
  (declare (ignore Button))
  (toggle-mirror-lines (view-named 'icon-editor Window) t t))



;; Content Actions


(defmethod ADJUST-PRESSURE-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Pressure (* 0.08 (+ -1.0 (/ (* 2.0 (get-slider-setting Slider)) 1000)))))
    (let ((Text-View (view-named 'pressuretext Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~4,3F" Pressure))
      (view-draw-contents Text-View)
      ;; update model editor
      (let ((Model-Editor (view-named 'model-editor Window)))
        (setf (pressure (inflatable-icon Model-Editor)) Pressure)
        (update-inflation Window)
        (setf (is-flat (inflatable-icon Model-Editor)) nil)))))


(defmethod ADJUST-CEILING-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Ceiling (* 0.001 (get-slider-setting Slider))))
    (let ((Text-View (view-named 'ceilingtext Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~4,3F" Ceiling))
      (view-draw-contents Text-View)
      ;; update model editor
      (let ((Model-Editor (view-named 'model-editor Window)))
        (setf (max-value (inflatable-icon Model-Editor)) Ceiling)
        (update-inflation Window)))))


(defmethod ADJUST-NOISE-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Noise (* 0.0002 (get-slider-setting Slider))))
    (let ((Text-View (view-named 'noise-text Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~4,2F" Noise))
      (view-draw-contents Text-View)
      ;; update model editor
      (let ((Model-Editor (view-named 'model-editor Window)))
        (setf (noise (inflatable-icon Model-Editor)) Noise)
        (update-inflation Window)
        (setf (is-flat (inflatable-icon Model-Editor)) nil)))))


(defmethod ADJUST-SMOOTH-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Smooth (truncate (get-slider-setting Slider) 200)))
    (let ((Text-View (view-named 'smooth-text Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~A" Smooth))
      (view-draw-contents Text-View)
      ;; update model editor
      (setf (smoothing-cycles Window) Smooth)
      (update-inflation Window))))


(defmethod ADJUST-Z-OFFSET-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Offset (* 0.1 (+ -1.0 (/ (* 2.0 (get-slider-setting Slider)) 1000)))))
    (let ((Text-View (view-named 'z-offset-text Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~4,3F" Offset))
      (view-draw-contents Text-View)
      ;; update model editor
      (let ((Model-Editor (view-named 'model-editor Window)))
        (setf (dz (inflatable-icon Model-Editor)) Offset)
        (view-draw-contents Model-Editor)))))


(defmethod ADJUST-ALPHA-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Alpha (* 0.001 (get-slider-setting Slider))))
    (let ((Text-View (view-named 'alpha-text Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~A%" (truncate (* Alpha 100))))
      (view-draw-contents Text-View))
    ;; update pen color
    (let ((Color-Well (view-named 'color-swatch Window)))
      (set-pen-color
       (view-named 'icon-editor Window) 
       (ash (color-red (selected-color Color-Well)) -8)
       (ash (color-green (selected-color Color-Well)) -8)
       (ash (color-blue (selected-color Color-Well)) -8) 
       (truncate (* 255 Alpha))))))


(defmethod ADJUST-DISTANCE-ACTION ((Window inflatable-icon-editor-window) (Slider slider))
  (let ((Distance (* 0.001 (get-slider-setting Slider))))
    (let ((Text-View (view-named 'distance-text Window)))
      ;; update label
      (set-dialog-item-text Text-View (format nil "~4,2F" Distance))
      (view-draw-contents Text-View))
    ;; set distance of inflatable icon
    (let ((Model-Editor (view-named 'model-editor Window)))
      (setf (distance (inflatable-icon Model-Editor)) Distance)
      (view-draw-contents Model-Editor))))


(defmethod CHANGE-ICON-ACTION ((Window inflatable-icon-editor-window) (Icon-Editor icon-editor))
  (update-inflation Window))


(defmethod UPRIGHT-ACTION ((Window inflatable-icon-editor-window) (Check-Box check-box))
  (let ((Model-Editor (view-named 'model-editor Window)))
    (setf (is-upright (inflatable-icon Model-Editor))
          (check-box-checked-p Check-Box))
    (view-draw-contents Model-Editor)))


;; surface actions

(defmethod FRONT-SURFACE-ACTION ((Window inflatable-icon-editor-window) (Pop-Up-Item pop-up-item))
  (declare (ignore Pop-Up-item))
  (let ((Model-Editor (view-named 'model-editor Window)))
    (setf (surfaces (inflatable-icon Model-Editor)) 'front)
    (view-draw-contents Model-Editor)))


(defmethod FRONT-AND-BACK-SURFACE-ACTION ((Window inflatable-icon-editor-window) (Pop-Up-Item pop-up-item))
  (declare (ignore Pop-Up-item))
  (let ((Model-Editor (view-named 'model-editor Window)))
    (setf (surfaces (inflatable-icon Model-Editor)) 'front-and-back)
    (view-draw-contents Model-Editor)))


(defmethod FRONT-AND-BACK-CONNECTED-SURFACE-ACTION ((Window inflatable-icon-editor-window) (Pop-Up-Item pop-up-item))
  (declare (ignore Pop-Up-item))
  (let ((Model-Editor (view-named 'model-editor Window)))
    (setf (surfaces (inflatable-icon Model-Editor)) 'front-and-back-connected)
    (view-draw-contents Model-Editor)))


(defmethod CUBE-SURFACE-ACTION ((Window inflatable-icon-editor-window) (Pop-Up-Item pop-up-item))
  (declare (ignore Pop-Up-item))
  (let ((Model-Editor (view-named 'model-editor Window)))
    (setf (surfaces (inflatable-icon Model-Editor)) 'cube)
    (view-draw-contents Model-Editor)))


(defmethod EDIT-ICON-FLATTEN-ACTION ((Window inflatable-icon-editor-window) (Button button))
  (let ((Model-Editor (view-named 'model-editor Window)))
    (flatten (inflatable-icon Model-Editor))
    ;; update GUI: pressure is 0.0
    (set-slider-setting (view-named 'pressure_slider Window) 500)
    (set-dialog-item-text (view-named 'pressuretext Window) "0.0")
    (setf (pressure (inflatable-icon (view-named 'model-editor Window))) 0.0)
    ;; enable flat texture optimization
    (setf (is-flat (inflatable-icon (view-named 'model-editor Window))) t)
    (change-icon-action Window (view-named 'icon-editor Window))
    (update-texture-from-image (inflatable-icon (view-named 'model-editor Window)))
    ;; update for user
    (view-draw-contents Model-Editor)))


(defmethod CLOSE-WINDOW-WITH-WARNING ((Self inflatable-icon-editor-window))
  (when (y-or-n-dialog "Close window without saving changes?" :cancel-text nil)
    ;; forget current content
    (when (close-action Self) 
      (funcall (close-action Self) Self))
    (window-close Self)))


(defmethod EDIT-ICON-CANCEL-ACTION ((Window inflatable-icon-editor-window) (Button button))
  (close-window-with-warning Window))


(defmethod EDIT-ICON-APPLY-ACTION ((Window inflatable-icon-editor-window) (Button button))
  (let ((Model-Editor (view-named 'model-editor Window)))
    ;; finalize geometry
    (compute-height (inflatable-icon Model-Editor))
    ;; save
    (when (destination-inflatable-icon Window)
      ;; (format t "~%copy into icon")
      (copy-content-into (inflatable-icon Model-Editor) (destination-inflatable-icon Window))
      (when (is-flat (destination-inflatable-icon Window))
        (setf (update-texture-from-image-p (destination-inflatable-icon Window)) t)))))


(defmethod EDIT-ICON-OK-ACTION ((Window inflatable-icon-editor-window) (Button button))
  ;; finalize geometry
  (compute-height (inflatable-icon (view-named 'model-editor Window)))
  ;; save
  (when (destination-inflatable-icon Window)
    ;;(format t "~%copy into icon")
    (copy-content-into (inflatable-icon (view-named 'model-editor Window)) (destination-inflatable-icon Window)))
  (window-hide Window)
  (window-save Window))

;___________________________________________
; Open & New                                |
;___________________________________________

(defun NEW-INFLATABLE-ICON-EDITOR-WINDOW () "
  Create and return a new inflatable icon editor window"
  (let ((Window (make-instance 'inflatable-icon-editor-window
                  :window-show nil
                  :view-size #@(550 530))))
    (setf (root-view Window) (load-object "ad3d:resources;windows;inflatable-icon-editor.window"))
    (window-show Window)
    Window))


(defun NEW-INFLATABLE-ICON-EDITOR-WINDOW-FROM-IMAGE (Pathname &key Shape-Filename Destination-Inflatable-Icon Close-Action) "
  Create and return an new inflatable icon editor by loading an image. 
  If folder contains .shape file matching image file name then load shape file."
  (let ((Window (make-instance 'inflatable-icon-editor-window
                  :window-show nil
                  :destination-inflatable-icon Destination-Inflatable-Icon
                  :close-action Close-Action
                  :view-size #@(550 530))))
    (setf (root-view Window) (load-object "ad3d:resources;windows;inflatable-icon-editor.window"))
    ;; Load 2D image 
    (let ((Icon-Editor (view-named 'icon-editor Window)))
      (load-image Icon-Editor Pathname)
      ;; Load Shape 
      (let ((Shape-Pathname (make-pathname
                             :directory (pathname-directory Pathname)
                             :name (or Shape-Filename (pathname-name Pathname))
                             :type "shape"
                             :host (pathname-host Pathname)
                             :defaults Pathname)))
        ;; when shape file is missing stick with flat default 
        (when (probe-file Shape-Pathname)
          (let ((Inflated-Icon-Editor (view-named 'model-editor Window)))
            (setf (inflatable-icon Inflated-Icon-Editor) (load-object Shape-Pathname))
            (setf (auto-compile (inflatable-icon Inflated-Icon-Editor)) nil)  ;; keep editable
            ;; use the icon editor image, not the new inflatable icon editor one
            (dispose-vector (image (inflatable-icon Inflated-Icon-Editor)))
            (get-rgba-color-at Icon-Editor 0 0)   ;; hack: make sure pixel buffer exists by accessing it once
            (setf (image (inflatable-icon Inflated-Icon-Editor))
                  (pixel-buffer Icon-Editor)))
          ;; proxy icons
          (setf (file Window) Shape-Pathname)
          (add-window-proxy-icon Window Shape-Pathname))))
    (window-show Window)
    Window))




#| Examples:


(defparameter *Inflatable-Icon-Editor* (new-inflatable-icon-editor-window))

(defparameter *Inflatable-Icon-Editor2* (new-inflatable-icon-editor-window-from-image (choose-file-dialog)))



|#