(in-package :hemlock)

(bind-key "Forward Word" #k"meta-rightarrow")
(bind-key "Select Forward Word" #k"meta-shift-rightarrow")
(bind-key "Forward Form" #k"control-rightarrow")
(bind-key "Select Forward Form" #k"control-meta-shift-rightarrow")

(bind-key "Backward Word" #k"meta-leftarrow")
(bind-key "Select Backward Word" #k"meta-shift-leftarrow")
(bind-key "Backward Form" #k"control-leftarrow")
(bind-key "Select Backward Form" #k"control-meta-shift-leftarrow")

;; casing

(bind-key "Uppercase Region" #k"control-x control-u") 
(bind-key "Lowercase Region" #k"control-x control-l") 
(bind-key "Capitalize Region" #k"control-x control-c") 