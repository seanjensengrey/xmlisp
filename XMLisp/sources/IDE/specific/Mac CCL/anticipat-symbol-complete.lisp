;;; -*- package: hemlock -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     A N T I C I P A T O R Y   SYMBOL COMPLETE          *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexander@agentsheets.com)    *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2012, AgentSheets Inc.                    *
   ;* Filename  : anticipatory-symbol-complete.lisp                  *
   ;* Updated   : 10/13/12                                           *
   ;* Version   :                                                    *
   ;*   1.0     : 06/19/04                                           *
   ;*   1.0.1   : 07/04/04 Peter Paine: custom -color*, nil wait     *
   ;*   1.0.2   : 07/07/04 correct position for fred-dialog-item     *
   ;*   1.1     : 09/08/04 don't get stuck; args and space on tab    *
   ;*   1.1.1   : 09/09/04 use *Package* if Fred has no package      *
   ;*   1.2     : 09/17/04 limited support Traps package, #_blabla   *
   ;*                      cannot find unloaded traps (most)         *
   ;*   1.3     : 09/29/04 save-exit function to be image friendly   *
   ;*   1.4     : 10/06/04 play nice with Glen Foy's Color-Coded     * 
   ;*   1.4.1   : 10/21/04 handle $updateEvt                         *
   ;*   1.4.2   : 12/14/04 XML "<..." and "</..." support            *
   ;*   1.5     : 10/21/05 proactive typo alert                      *
   ;*   1.5.1   : 08/25/06 use "..." instead of ellipsis char        *
   ;*   1.5.2   : 09/22/06 works with LF EOL Fred buffers            *
   ;*   1.5.3   : 08/14/07 symbol-completion-enabled-p method        *
   ;*   1.5.4   : 10/24/07 handle Apple Events as kHighLevelEvent    *
   ;*   2.0     : 04/22/08 Clozure CL, Gail Zacharias                *
   ;*   2.0.1   : 04/25/08 auto enabled, release pool, process fix   *
   ;*   2.0.2   : 12/30/08 kill processes when typing fast           *
   ;*   3.0     : 05/15/09 dropped MCL, API names, insertion point   *
   ;*   3.0.1   : 10/13/12 compatible wih CCL 1.9                    *
   ;* HW/SW     : MacBook Pro, CCL 1.9, OS X 10.6.8                  *
   ;* Abstract  : Attempt symbol completion while user is typing.    *
   ;*             #\tab to complete, show arglist if possible        *
   ;*             #\esc to cancel                                    *
   ;* Status    : good to go                                         *
   ;* License   : LGPL                                               *
   ;******************************************************************

(in-package :hemlock)

(export '(symbol-completion-enabled-p *anticipatory-symbol-completion-enabled-p*))


(defvar *Wait-Time-for-Anticipatory-Symbol-Complete* 0.2 "time in seconds to wait before anticipatory symbol complete begins to search.")

(defvar *Anticipatory-Symbol-Completion-Enabled-p* t)

(defvar *Anticipatory-Symbol-Completion-Font-Color* nil "color used for anticipatory text")

(defun ANTICIPATORY-SYMBOL-COMPLETION-FONT-COLOR ()
  (or *Anticipatory-Symbol-Completion-Font-Color*
      (setf *Anticipatory-Symbol-Completion-Font-Color* (#/grayColor ns:ns-color))))


(defvar *Anticipatory-Symbol-Completion-Background-Color* nil)

(defun ANTICIPATORY-SYMBOL-COMPLETION-BACKGROUND-COLOR ()
  (or *Anticipatory-Symbol-Completion-Background-Color*
      (setq *Anticipatory-Symbol-Completion-Background-Color* 
            (gui::color-values-to-nscolor 55000/65535 55000/65535 64000/65535))))

(defvar *Zero-Completion-Hook* nil "Call this function if there are no completions: could be the sign of a typo. Typically replace with more subtle sound.")

(defvar *Use-API-Names* t "if true convert symbols to same name and case as in API definition")

;; Better enable these CCL compiler preferences to get more meaninglful arglists

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless ccl:*Save-Local-Symbols* (print "ANTICIPATORY SYMBOL COMPLETE hint: To get meaningful arglists for completed functions you should set ccl:*Save-Local-Symbols* to t"))
  (unless ccl:*Fasl-Save-Local-Symbols* (print "ANTICIPATORY SYMBOL COMPLETE hint: To get meaningful arglists for completed functions you should set ccl:*Fasl-Save-Local-Symbols* to t")))

;___________________________________ 
; Completion Overlay Window         |
;___________________________________ 

(defvar *Assistant* nil)


(defclass COMPLETION-OVERLAY (ns:ns-view)
  ((text-attributes :foreign-type :id)
   (text :foreign-type :id))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/drawRect: :void) ((self completion-overlay) (rect :<NSR>ect))
  (ccl::with-autorelease-pool 
      (#/set (#/clearColor ns:ns-color))
    (#_NSRectFill (#/bounds self))
    (ns:with-ns-point (point 0 1)
      (#/drawAtPoint:withAttributes: (slot-value self 'text)
                                     point
                                     (slot-value self 'text-attributes)))))


(defun COMPLETION-OVERLAY-WINDOW () "
  Return current overlay window used for symbol completion. 
  Create one if needed."
  (or *Assistant*
      (setq *Assistant*
            (ns:with-ns-rect (frame 100 100 400 40)
              (let* ((w (make-instance 'ns:ns-window
                          :with-content-rect frame
                          :style-mask #$NSBorderlessWindowMask
                          :backing #$NSBackingStoreBuffered
                          :defer #$YES))
                     (view (make-instance 'completion-overlay
                             :with-frame (#/frame (#/contentView w))))
                     ;; Create attributes to use in window
                     (attribs (make-instance 'ns:ns-mutable-dictionary :with-capacity 3)))
                (#/setObject:forKey: attribs (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font))
                                     #&NSFontAttributeName)
                (#/setObject:forKey: attribs (anticipatory-symbol-completion-font-color)
                                     #&NSForegroundColorAttributeName)
                (#/setObject:forKey: attribs (anticipatory-symbol-completion-background-color)
                                     #&NSBackgroundColorAttributeName)
                (setf (slot-value view 'text-attributes) (#/retain attribs))
                (setf (slot-value view 'text) (#/retain (gui::%make-nsstring "")))
                (#/setContentView: w view)
                ;; Set the background color to clear so that (along with the setOpaque call below)
                ;; we can see through the parts of the window that we're not drawing into
                (#/setBackgroundColor: w (#/clearColor ns:ns-color))
                ;; No transparency for actual drawing into the window
                (#/setAlphaValue: w (gui::cgfloat 1.0))
                ;; Allow see through the parts of the window we're not drawing into
                (#/setOpaque: w #$NO)
                ;; Make it as unobtrusive as possible
                (#/setIgnoresMouseEvents: w #$YES)
                (#/setExcludedFromWindowsMenu: w #$YES)
                (#/setHidesOnDeactivate: w #$YES)
                w)))))


(defun OPEN-OVERLAY-WINDOW (Text Position)
  "Show text at screen position"
  (ccl::with-autorelease-pool 
      (let ((w (completion-overlay-window)))
        (#/setFrameOrigin: w Position)
        (let* ((w (completion-overlay-window))
               (overlay (#/contentView w))
               (nsstring (ccl::%make-nsstring Text)))
          (with-slots (text) overlay
            (#/release text)
            (setf text (#/retain nsstring)))
          (#/display w)
          (#/orderFront: w ccl:+null-ptr+)))))


(defun CLOSE-OVERLAY-WINDOW ()
  "Hide the overlay window"
  (let ((w (completion-overlay-window)))
    (#/orderOut: w ccl:+null-ptr+)))

;___________________________________ 
; Symbol String functions           |
;___________________________________ 

(defun COMMON-PREFIX (String1 String2)
  ;; if one string is a complete substring then return it
  (let ((Short-String (if (< (length String1) (length String2)) String1 String2)))
    (dotimes (I (length Short-String) Short-String)
      (let ((Char1 (char String1 i)))
        (unless (char= Char1 (char String2 i))
          (return (subseq Short-String 0 i)))))))
    

(defun LONGEST-PREFIX (Symbols)
  (when Symbols
    (reduce #'common-prefix (mapcar #'symbol-name Symbols))))

;___________________________________ 
; Completion-Request class          |
;___________________________________ 

(defclass COMPLETION-REQUEST ()
  ((time-stamp :accessor time-stamp :initform (get-internal-real-time))
   (completion-string :accessor completion-string :initform "" :initarg :completion-string)
   (completion-name :accessor completion-name)
   (completion-package :accessor completion-package)
   (completion-prefix :accessor completion-prefix :initform nil)
   (fred-instance :accessor fred-instance :initarg :fred-instance)
   (fred-buffer-start :accessor fred-buffer-start :initarg :fred-buffer-start)
   (fred-buffer-end :accessor fred-buffer-end :initarg :fred-buffer-end))
  (:documentation "captures what the request is, when it was made, and where is what made"))


(defmethod INITIALIZE-INSTANCE :after ((Self completion-request) &rest Args)
  (declare (ignore Args))
  (let ((String (completion-string Self)))
    ;; explore package clues
    (when String
      (setf (completion-name Self) 
            (case (char String 0)
              ((#\: #\#) (subseq (string-upcase String) 1))
              (t (string-upcase String))))
      (setf (completion-package Self) 
            (or (and (char= (char String 0) #\:) (find-package :keyword))
                (and (char= (char String 0) #\#) (find-package :traps))
                (buffer-package (hemlock-view-buffer (fred-instance Self)))
                *Package* )))))


(defmethod NATIVE-TEXT-VIEW ((Self completion-request))
  (gui::text-pane-text-view (hi::hemlock-view-pane (fred-instance Self))))


(defun ADD-SPECIAL-PACKAGE-PREFIX (String Package)
  ;; some packages have a special prefix consisting of a single character
  (cond
   ((equal Package (find-package :keyword)) (format nil ":~A" String))
   ((equal Package (find-package :traps)) (format nil "#~A" String))
   (t String)))


(defmethod PROMISING-PREFIX ((Thing string))
  ;; heuristicly exclude
  (and
   (not (char-equal (char Thing 0) #\\))  ;; char names
   (not (char-equal (char Thing 0) #\"))  ;; beginning of strings
   (not (every ;; numbers
         #'(lambda (Item)
             (member Item '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\d #\D #\s #\S #\e #\E #\. #\/)))
         Thing))))


(defmethod COMPLETION-SCREEN-POSITION ((Self completion-request))
  (let* ((view (fred-instance Self))
         (tv (gui::text-pane-text-view (hi::hemlock-view-pane view))))
    (multiple-value-bind (x y) (gui::charpos-xy tv (fred-buffer-end Self))
      (ns:with-ns-point (pt x (+ y (gui::text-view-line-height tv)))
        (#/convertBaseToScreen: (#/window tv)
                                (#/convertPoint:toView: tv pt gui::+null-ptr+))))))


(defmethod VIEW-ACTIVE-P ((Self completion-request))
  (not (ccl:%null-ptr-p (#/window (hi::hemlock-view-pane (fred-instance Self))))))


(defvar *Completion-Request* nil "Currently active completion request")


(defmethod ANTICIPATORY-SYMBOL-COMPLETE ((Self completion-request)) "
  in: Completion-Request completion-request.
  Explore the opportunity for symbol completion."
  (ccl::with-autorelease-pool
      ;; don't be too eager and wait first a little
      (sleep *Wait-Time-for-Anticipatory-Symbol-Complete*)
    ;; find matching symbols
    (let* ((Local-Symbols (apropos-list (completion-name Self) (completion-package Self)))
           (Symbols (matching-prefix-symbols (completion-name Self) Local-Symbols)))
      ;;; (lui::speak (write-to-string (length Symbols)))
      (cond
       ;; proactive typo alert
       ((and (= (length Symbols) 0)
             (promising-prefix  (completion-name Self)))
        (#/setInsertionPointColor: (native-text-view Self) (#/redColor ns:ns-color))
        (when *Zero-Completion-Hook* (funcall *Zero-Completion-Hook*)))  ;; beep when the number has dropped to zero: usually a sign of a typo
       (t
        ;; some matches
        (#/setInsertionPointColor: (native-text-view Self) (#/greenColor ns:ns-color))))
       ;; completion attempt
      (let ((Prefix (longest-prefix Symbols)))
        (when (and (> (length Prefix) (length (completion-name Self)))
                   (view-active-p Self))
          ;; if we made it this far we better complete things
          (let* ((Extension (string-downcase (subseq Prefix (length (completion-name Self))))))
            (unwind-protect
                (progn
                  (open-overlay-window (if (find-symbol Prefix (completion-package Self))
                                         Extension
                                         (format nil "~A..." Extension))
                                       (completion-screen-position Self))
                  (setf (completion-prefix Self) Prefix)
                  ;; If the user types anything while the window is up, this process gets reset with *Completion-Request*
                  ;; still set, so the Tab command can tell what the Prefix was.  
                  (setq *Completion-Request* Self)
                  (sleep 5)
                  ;; timed out: forget completion request
                  (setq *Completion-Request* nil))
              (close-overlay-window)) ))))))


(defmethod INSERT-COMPLETION ((Self completion-request))
  (let ((Prefix (completion-prefix Self)))
    (when Prefix
      (buffer-replace-string (fred-buffer-start Self)
                             (fred-buffer-end Self)
                             (add-special-package-prefix Prefix (completion-package Self))
                             (completion-string Self))
      (let ((Symbol (find-symbol Prefix (completion-package Self))))
        (when Symbol
          (current-function-arglist-command nil)
          (when *Use-Api-Names*
            (multiple-value-bind (String Start End) (buffer-current-string)
              (let ((Symbol (find-symbol (string-upcase String) (completion-package Self))))
                (when (and Symbol (get Symbol :api-name))
                  (paste-characters Start (- End Start) (get Symbol :api-name))))))))
      (hi::exit-event-handler))))

;___________________________________ 
; Process Management                |
;___________________________________ 

(defvar *Completion-Process* nil "process used to complete symbols in anticipatory way")


(defun COMPLETION-PROCESS ()
  (or *Completion-Process*
      (setq *Completion-Process* (ccl:make-process "Anticipatory Symbol Complete" :priority 0))))


(defun START-SYMBOL-COMPLETE-PROCESS (Request)
  (when *Completion-Process*
    ;; not sure how we get here: before a new completion process is started 
    ;; the old should have been killed already
    (ccl:process-kill *Completion-Process*))
  (setq *Completion-Process*
	(ccl::process-run-function 
	 '(:name "Anticipatory Symbol Complete" :priority 0)
	 #'(lambda () (anticipatory-symbol-complete Request)))))


(defun BEGIN-SYMBOL-COMPLETE ()
  (multiple-value-bind (String Start End) (buffer-current-string)
    (when (> (length String) 1)
      (handler-case (start-symbol-complete-process (make-instance 'completion-request
                                                     :completion-string String
                                                     :fred-instance (current-view)
                                                     :fred-buffer-start start
                                                     :fred-buffer-end end))
        (t (Condition) (format t "condition: ~A" Condition))))))


(defun ABORT-SYMBOL-COMPLETE-PROCESS ()
  (cond
   ;; completion still going on
   (*Completion-Process*
    (ccl:process-kill *Completion-Process*)
    (setq *Completion-Process* nil))
   ;; completion must have timed out: remove completion request
   (t
    (setq *Completion-Request* nil))))

;___________________________________ 
; Symbol-Complete.lisp functions    |
;___________________________________ 

(defun BUFFER-REPLACE-STRING (Start End String &optional Old-String) "
  in: Start End {position}, String {string}, 
       &optional Old-String {string}.
  Delete the current buffer content between <Start> and <End>, insert
  <String> and place insertion marker to <End> position."
  (paste-characters Start (- End Start) 
                    (if Old-String
                      (case (string-format Old-String)
                        (:upper (string-upcase String))
                        (:lower (string-downcase String))
                        (:capital (string-capitalize String)))
                      String)))


(defun STRING-FORMAT (String) "
  in:  String {string}.
  out: Capitalization {keyword} :upper, :lower :capital.
  Return the capitalization status of a string"
  (case (length String)
    (0 :lower)
    (1 (if (lower-case-p (char String 0)) :lower :upper))
    (t (if (char= (char String 0) #\*)
         (string-format (subseq String 1))
         (if (upper-case-p  (char String 0))
           (if (upper-case-p (char String 1))
             :upper
             :capital)
           :lower)))))


(defun MATCHING-PREFIX-SYMBOLS (String Symbols) "
  in:  String {string}, Symbols {list of: {symbol}}.
  out: Symbols {list of: {symbol}}.
  Return only the symbols of which <String> is a prefix."
  (let ((L (length String)))
    (remove-if-not
     #'(lambda (Symbol) (string= String (symbol-name Symbol) :end1 L :end2 L))
     Symbols)))

;___________________________________ 
; Editor extensions                 |
;___________________________________ 

(defun BUFFER-CURRENT-STRING ()
  (with-mark ((Start (current-point))
              (End (current-point)))
    (unless (eq (previous-character Start) #\space)
      ;; scan left for delimiter
      (loop
        (case (previous-character Start)
          ((nil #\space #\tab #\return #\linefeed #\( #\) #\' #\< #\")
           ;; special treatment for "<" versus "</" XML prefix
           (when (eq (next-character start) #\/)
             (mark-after Start))
           (return)))
        (mark-before Start))
      ;; scan right for delimiter
      (loop
        (case (next-character End)
          ((nil #\space #\tab #\return #\linefeed #\( #\)) (return)))
        (mark-after End))
      (values (region-to-string (region Start End))
              (mark-absolute-position Start)
              (mark-absolute-position End)))))

;___________________________________ 
; Key events                        |
;___________________________________ 
 
(defmethod HI::EXECUTE-HEMLOCK-KEY :around ((View hemlock-view) Key)
    ;; make sure this is a regular key
  (cond
   ;; special keys
   ((or (not (equal (type-of Key) 'hi::key-event))
        (not (numberp (hi::key-event-keysym Key))))
    (call-next-method))
   ;; regular key
   (t
    (case (code-char (hi::key-event-keysym Key))
      ;; insert completion if there is one
      (#\Tab 
       (cond
        ;; Tab to insert completion
        ((#/isVisible (completion-overlay-window)) 
         (abort-symbol-complete-process)
         (insert-completion *Completion-Request*))
        ;; regular Tab
        (t
         (call-next-method))))
      (t
       (call-next-method))))))


(defmethod HI::EXECUTE-HEMLOCK-KEY :before ((view hemlock-view) key) 
  ;; make sure this is a regular key
  (when (or (not (equal (type-of Key) 'hi::key-event))
            (not (numberp (hi::key-event-keysym Key))))
    (return-from hi::execute-hemlock-key))
  (case (code-char (hi::key-event-keysym Key))
    ;; when pressing delimiter keys look for subsitutions
    ((#\space #\( #\))
     (when *Use-Api-Names*
       ;; substitute with pretty print name if exists
       (multiple-value-bind (String Start End) (buffer-current-string)
         (let ((Symbol (find-symbol (string-upcase String) (buffer-package (hemlock-view-buffer View)))))
           (when (and Symbol (get Symbol :api-name))
             (paste-characters Start (- End Start) (get Symbol :api-name))))))))
  ;; forget ongoing completions
  (abort-symbol-complete-process))


(defmethod HI::EXECUTE-HEMLOCK-KEY :after ((View hemlock-view) Key)
  ;; make sure this is a regular key
  (when (or (not (equal (type-of Key) 'hi::key-event))
            (not (numberp (hi::key-event-keysym Key))))
    (return-from hi::execute-hemlock-key))
  (case (code-char (hi::key-event-keysym Key))
    ;; delete key
    (#\Rubout
     (begin-symbol-complete))
    ;; symbol is done
    ((#\return #\space #\( #\))
     (#/setInsertionPointColor: 
      (gui::text-pane-text-view (hi::hemlock-view-pane View))
      (#/blackColor ns:ns-color))
     ;; check first if symbol exists?
     (current-function-arglist-command nil))
    (#\tab )
    ;; any other key that could trigger a completion
    (t 
     (when *Anticipatory-Symbol-Completion-Enabled-p*
       (begin-symbol-complete)))))

;___________________________________ 
; save-application support          |
;___________________________________ 

(defun ANTICIPATORY-SYMBOL-COMPLETE-SAVE-EXIT-FUNCTION ()
  (setq *Assistant* nil)
  (setq *Anticipatory-Symbol-Completion-Font-Color* nil)
  (setq *Anticipatory-Symbol-Completion-Background-Color* nil)
  (when *Completion-Process*
    (ccl:process-kill *Completion-Process*)
    (setq *Completion-Process* nil)))
  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'anticipatory-symbol-complete-save-exit-function ccl:*Save-Exit-Functions*))
 

#| Examples:

(time (common-prefix "WITH-OPEN-FILE" "WITH-CLOSED-HOUSE"))

(time (common-prefix "WITH-OPEN-FILE" "WITH-OPEN-FILENAME"))

(time (common-prefix "WITH-OPEN-FILE" "WITH-OPEN-FILE"))



|#

