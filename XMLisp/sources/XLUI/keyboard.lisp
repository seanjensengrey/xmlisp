;;-*- Mode: Lisp; Package: AD3D -*-
;*********************************************************************
;*                                                                   *
;*            K E Y B O A R D                                        *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2005, AgentSheets Inc.                    *
;* Filename     : Keyboard.lisp                                      * 
;* Last Update  : 08/25/06                                           *
;* Version      :                                                    *
;*    1.0       : 04/05/05                                           *
;*    2.0       : 11/10/05 mac/win key mapping: virtual-key-table    *
;*    2.0.1     : 08/25/06 string-or-null slot type                  *
;* Systems      : G4, MCL 5.2, OS X 10.4.7                           *
;* Abstract     : low level keyboard interface                       *
;*                                                                   *
;*********************************************************************

;; MS virtual keys: http://nehe.gamedev.net/data/articles/article.asp?article=09

(in-package :xlui)

;**************************************
;* Virtual Key Table                  *
;**************************************

(defvar *Virtual-Key-Table* nil)

(defun VIRTUAL-KEY-TABLE () "
  out: Table virtual-key-table.
 Return the shared virual key table"
  (or *Virtual-Key-Table*
      (setq *Virtual-Key-Table* 
            (load-object 
             "lui:resources;virtual-key-table.xml" 
             :package (find-package :xlui)))))

;**************************************
;* Key Codes                          *
;**************************************

(defclass VIRTUAL-KEY-TABLE (xml-serializer)
  ((links :accessor links :initform nil)
   (virtual-keys :accessor virtual-keys :initform nil)
   (comments :accessor comments :initform nil :type string-or-null))
  (:documentation "virtual keys"))


(defclass LINK (xml-serializer)
  ((url :accessor url))
  (:documentation "web link"))


(defclass VIRTUAL-KEY (xml-serializer)
  ((name :accessor name :type string :documentation "the platform independent name of a virtual key")
   (comments :accessor comments :initform nil :type string-or-null :documentation "what does the key do?")
   (key-bindings :accessor key-bindings :initform nil :documentation "platform specific codes and labels"))
  (:documentation "descriptive name and comments"))


(defclass KEY-BINDING (xml-serializer)
  ((label :accessor label  :initform nil :documentation "Label found on actual key. Nil defaults to key name")
   (platform :accessor platform :initform "mac" :type string :documentation "eg., Mac or Win")
   (code :accessor code :type integer :documentation "platform specific code typically extracted in handler")
   (symbol-code :accessor symbol-code :initform nil :type integer-or-null :documentation "character code describing key label as symbol in special font")
   (symbol-font :accessor symbol-font :initform "charcoal cy" :documentation "the font used to print the special character symbol")
   (comments :accessor comments :initform nil :type string-or-null))
  (:documentation "a platform specific key binding"))


(defmethod KEY-CODE-BINDING (Code &key (Platform "mac"))
  (dolist (Key (virtual-keys (virtual-key-table)))
    (dolist (Binding (key-bindings Key))
      (when (and (string-equal Platform (platform Binding))
                 (= Code (code Binding)))
        (return-from key-code-binding 
          (values Binding Key))))))


(defun KEY-CODE->NAME (Code &key (Platform "mac"))
  (multiple-value-bind (Binding Key)
                       (key-code-binding Code :platform Platform)
    (declare (ignore Binding))
    (and Key (name Key))))


(defun KEY-CODE->LABEL (Code &key (Platform "mac"))
  (multiple-value-bind (Binding Key)
                       (key-code-binding Code :platform Platform)
    (let ((Label (or (label Binding) (name Key))))
      (if (symbol-code Binding)  ;; just append the symbol code to the label
        (if (string-equal Label "")
          (string (code-char (symbol-code Binding)))
          (concatenate 'string Label " " (string (code-char (symbol-code Binding)))))
        Label))))


(defun KEY-NAME->CODE (Name &key (Platform "mac"))
  (dolist (Key (virtual-keys (virtual-key-table)))
    (dolist (Binding (key-bindings Key))
      (when (and (string-equal Name (name Key))
                 (string-equal Platform (platform Binding)))
        (return-from key-name->code (code Binding))))))

;; (key-code-binding 100)
;; (key-code->name 100)
;; (key-code->label (key-name->code "Command"))
;; (key-code->label (key-name->code "control"))
;; (key-name->code "f7")

;*******************************************
;* Key Chord Codes                         *
;*   a Key-Chord-Code combines key-codes   *
;*   and modifier keys into a cannonical   *
;*   code, name and label                  * 
;*******************************************
;; chord is a combination of the key code and the modifier keys:
;;  bits 0-7 Mac Key code
;;  bit 8  control
;;  bit 9  option/alt
;;  bit 10 shift
;;  bit 11 command 


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant kKeyChordControlBitIndex 8)
  (defconstant kKeyChordOptionBitIndex 9)
  (defconstant kKeyChordShiftBitIndex 10)
  (defconstant kKeyChordCommandBitIndex 11))

#|
(defun KEY-CHORD-CODE (Key-Code Controlp Optionp Shiftp Commandp)
  (logior
   Key-Code
   (if Controlp #.(lsh 1 kKeyChordControlBitIndex) 0)
   (if Optionp #.(lsh 1 kKeyChordOptionBitIndex) 0)
   (if Shiftp #.(lsh 1 kKeyChordShiftBitIndex) 0)
   (if Commandp #.(lsh 1 kKeyChordCommandBitIndex) 0)))
|#
;; (key-chord-code 0 nil nil t nil)
   
#|
OLD MCL Codes do we need this?
(defun USER-DEFINE-KEY-CHORD (&key Idle-Function) "
  in:  &key Idle-Function lambda ().
  out: Key-Chord-Code.
  Make the user press a combination of one key with modifiers and return that code.
 The <Idle-Function> is called while the user is still making choice. 
 Idle function can be used for animations."
 (let ((Control-Code (key-name->code "control"))
       (Option-Code (key-name->code "option"))
       (Shift-Code (key-name->code "shift"))
       (Command-Code (key-name->code "command")))
   (loop
     (let ((Controlp nil)
           (Optionp nil)
           (Shiftp nil)
           (Commandp nil)
           (Non-Modifier-Code nil))
       ;; find at least one non-modifier key
       (dolist (Code (get-key-codes))
         (cond
          ((= Code Control-Code) (setq Controlp t))
          ((= Code Option-Code) (setq Optionp t))
          ((= Code Shift-Code) (setq Shiftp t))
          ((= Code Command-Code) (setq Commandp t))
          (t (setq Non-Modifier-Code Code))))
       (when (and Non-Modifier-Code (key-code->name Non-Modifier-Code)) ;; it better have a name
         #+:MCL (#_FlushEvents (logior #$keyDownMask #$keyUpMask) 0)
         (return (key-chord-code Non-Modifier-Code Controlp Optionp Shiftp Commandp)))
       (when Idle-Function (funcall Idle-Function))))))
|#        


(defgeneric KEY-CHORDS-MATCH (Chord1 Chord2)
  (:documentation "Two key chords match if their key codes and modifier keys are the same"))


(defmethod KEY-CHORDS-MATCH ((Chord1 integer) (Chord2 integer))
  ;; because - for now - we ignore caps lock this test is trivial
  (= Chord1 Chord2))


(defun KEY-CHORD-CODE->NAME (Code) "
  in: Code integer.
  out: Name string.
  Cannonical name encoding <Code> as XML friendly string without special chars"
 (with-output-to-string (string)
   (when (logbitp kKeyChordControlBitIndex Code) 
     (princ "control+" String))
    (when (logbitp kKeyChordOptionBitIndex Code) 
     (princ "option+" String))
    (when (logbitp kKeyChordShiftBitIndex Code) 
     (princ "shift+" String))
    (when (logbitp kKeyChordCommandBitIndex Code) 
     (princ "command+" String))
    (format String "~A" (key-code->name (logand Code 255)))))

;(KEY-CHORD-CODE->NAME 1)

(defun KEY-CHORD-CODE->LABEL (Code) "
  in: Code integer.
  out: Label string; Font string.
  Cannonical name encoding <Code> as compact string including special char symbols including non-ASCII chars.
 This string should recognizable by user as key combination, e.g., control-alt del"
 (let ((Prefixp nil))
   (with-output-to-string (string)
     (when (logbitp kKeyChordControlBitIndex Code) 
       (format String "~A" (key-code->label (key-name->code "control")))
       (setq Prefixp t))
     (when (logbitp kKeyChordShiftBitIndex Code) 
       (format String "~A" (key-code->label (key-name->code "shift")))
       (setq Prefixp t))
     (when (logbitp kKeyChordOptionBitIndex Code) 
       (format String "~A" (key-code->label (key-name->code "option")))
       (setq Prefixp t))
     (when (logbitp kKeyChordCommandBitIndex Code) 
       (format String "~A" (key-code->label (key-name->code "command")))
       (setq Prefixp t))
     (when Prefixp (princ " " String))
     (format String "~A" (key-code->label (logand Code 255))))))



(defun SPLIT-CHORD-NAME-STRING (String)
  ;; quick and dirty tokenizer
  ;; todo: make more robust for spaces around "+" ?
  (let ((List nil)
        (Token ""))
    (with-input-from-string (Input String)
      (loop
        (let ((Char (read-char Input nil nil)))
          (unless Char
            (unless (string-equal Token "") (push Token List)) ;; what ever is left must be name of key
            (return (reverse List)))
          (case Char
            ;; delimiter
            (#\+ 
             (push Token List)
             (setq Token ""))
            (t 
             (setq Token (concatenate 'string Token (string Char))))))))))

;; (split-chord-name-string "command+control+page up")
#|      
(defun KEY-CHORD-NAME->CODE (Name) "
  in:  Name string.
  out: Code integer.
  Convert name into key chord code."
 (let ((Controlp nil)
       (Optionp nil)
       (Shiftp nil)
       (Commandp nil)
       (Key-Name nil))
   ;; parse token
   (dolist (Token (split-chord-name-string Name))
     (cond
      ((string-equal Token "control") (setq Controlp t))
      ((string-equal Token "option") (setq Optionp t))
      ((string-equal Token "shift") (setq Shiftp t))
      ((string-equal Token "command") (setq Commandp t))
      (t (setq Key-Name Token))))
   (let ((Key-Code (key-name->code Key-Name)))
     (unless Key-Code (error "\"~A\" is not a valid key name" Key-Name))
     (key-chord-code Key-Code Controlp Optionp Shiftp Commandp))))
  |# 
;_________________________________
; Internal functions              |
;_________________________________
#|
(defun GET-KEY-CODES () "
  out: Codes list of AppleKeyCode;
  Return a list of Apple key codes.
  Number of key that can be recognized simulaneously depends on keyboard and on key."
 (let ((Codes nil))
   (rlet ((Key-Map :keymap))
     (#_getKeys Key-Map)
     (dotimes (Key-Code 128 Codes)
       (when (logbitp (mod Key-Code 8) (%get-byte Key-Map (truncate Key-Code 8)))
         (push Key-Code Codes))))))


(defun KEY-DOWN-P (Code) "
  in: Code AppleKeyCode;
  Is Key with AppleKeyCode <code> currently pressed?"
  (rlet ((Key-Map :keymap))
    (#_getKeys Key-Map)
    (logbitp (mod Code 8) (%get-byte Key-Map (truncate Code 8)))))


(defun GET-KEYS (&optional Key-Codes)
  (rlet ((Key-Map :keymap))
    (#_getKeys Key-Map)
    (cond
     (Key-Codes
      (dolist (Key-Code Key-Codes t)
        (unless (<= 0 Key-Code 127) (return-from GET-KEYS nil))
        (unless (logbitp (mod Key-Code 8) (%get-byte Key-Map (truncate Key-Code 8)))
          (return-from GET-KEYS nil))))
     (t (dotimes (I 4 nil)
          (unless (= 0 (%get-long Key-Map (* i 4)))
            (return-from GET-KEYS t)))))))


(defun GET-FIRST-KEY-CODE () "
  out: Code {fixnum}.
  Return the keyboard key code.
  Return nil when no key is pressed."
  (rlet ((Key-Map :keymap))
    (#_getKeys Key-Map)
    (dotimes (Byte 16)
      (let ((Value (%get-byte Key-Map Byte)))
        (dotimes (Bit 8)
          (when (logbitp Bit Value) 
            (return-from get-first-key-code (+ (* Byte 8) Bit))))))))
|#
;_________________________________
; External functions              |
;_________________________________
#|
(defun OPTION/ALT-KEY-DOWN-P ()
  (key-down-p 58))
|#

#| Examples:


;; valid keys

(edit <key label="return"/>)

(edit <key label="a"/>)

(edit <key label="option+a"/>)

(edit <key label="shift+option+ command+right arrow"/>)


;; invalid keys

(edit <key label="command-f5"/>) 

(key-chord-name->code (key-chord-code->name 2066))


(loop
  (let ((Code (user-define-key-chord)))
    (format t "code=\"~A\" name=\"~A\" label=\"~A\"~%"
            Code
            (key-chord-code->name Code)
            (key-chord-code->label Code)))
  (sleep 0.1))


(loop (let ((Codes (get-key-codes)))
        (when Codes (print Codes))))

(loop (print (option/alt-key-down-p)))



(defun MAKE-KEYS (From To)
  (let ((Char From)
        (Keys nil))
    (loop
      (when (char> Char To) (return (reverse Keys)))
      (format t "~%~A:" Char)
      (let ((Code nil))
        (loop
          (setq Code (get-first-key-code))
          (when Code 
            (print Code) (terpri)
            (sleep 1.0)
            (return)))
        (push
         (read-from-string
          (format nil "<virtual-key name=\"~A\">~%<key-binding platform=\"mac\" code=\"~A\"/>~%
          <key-binding platform=\"win\" code=\"~A\"/>~%</virtual-key>"
                  Char 
                  Code
                  (char-code Char)))
         Keys))
      (setq Char (code-char (1+ (char-code Char)))))))


(make-keys #\A #\Z)
(make-keys #\0 #\9)


|#


