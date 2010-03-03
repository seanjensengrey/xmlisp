;;; -*- package: ccl -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM      S Y M B O L   C O M P L E T I O N                 *
;*    PACKAGE      :ccl                                              *
;*                                                                   *
;*********************************************************************
   ;* Author:     Alex Repenning, ralex@cs.colorado.edu              *
   ;*             http://www.cs.colorado.edu/~ralex/Home.html        *
   ;*             Copyright (c) 2007 Alex Repenning                  *
   ;*                                                                *
   ;* Address:    Computer Science Department                        *
   ;*             University of Colorado at Boulder                  *
   ;*             Boulder, CO 80309-0430                             *
   ;*                                                                *
   ;* Filename         : symbol-complete.lisp                        *
   ;* Last Update      : 2/26/07                                     *
   ;* Version   :                                                    *
   ;*   1.0   07/24/90  Alex Repenning                               *
   ;*   1.1   04/13/91  Alex & Brigham Bell   completion works also  *
   ;*                   if cursor in the middle of symbol.           *
   ;*   2.0    1/ 6/92  Alex, MCL 2.0                                *
   ;*   2.0.1  3/31/92  left trim "#'", "#`", "#", "'", "`"          *
   ;*   2.0.2  8/25/92  Preserve package qualifier.                  *
   ;*   2.0.3  12/8/92  Show arglist                                 *
   ;*   2.0.4  12/16/92 Use shortest package qualifiers              *
   ;*   2.0.5  10/27/94 Powerbook: *Arglist-On-Space* can be used to *
   ;*                   prevent HD access.                           *
   ;*   2.1.0  5/1/95   MCL 3.0 Ready                                *
   ;*   2.1.1  8/16/96  better minibuffer use                        *
   ;*   2.2    2/26/07  allow tab for anticipatory-symbol-complete   *
   ;*                   compatiblity                                 *
   ;* System    : OS X 10.4.8, MCL 5.2                               *
   ;* Abstract  : A simple line completion mechanism.                *
   ;* Features:                                                      *
   ;*   - Simple one key operation: Since most users do not like to  *
   ;*       remember many different completion functions, this       *
   ;*       completion package combines several completion           *
   ;*       strategies into one function (using a cascading filter   *
   ;*       scheme).                                                 *
   ;*         1) prefix search in window package                     *
   ;*         2) prefix search in all packages                       *
   ;*         3) substring search in window package                  *
   ;*         4) substring search in all packages                    *
   ;*       If no symbol is found the next strategy is employed.     *
   ;*       If only one symbol is found it will be used as           *
   ;*       completion.                                              *
   ;*       If multiple symbols are found then either the symbol     *
   ;*       is completed as far as possible or a menu is offered.    *
   ;*   - Partial Completion (sounds like a contradiction):          *
   ;*       If at any stage in the search there is more than one     *
   ;*       interned symbol matching what you typed so far and if    *
   ;*       the common prefix of these symbols is longer than what   *
   ;*       is typed so far you will get this common prefix.         *
   ;*   - Preserves Case: The completion will assume the same case   *
   ;*       of the string typed so far (lower case, upper case, or   *
   ;*       capitalized),                                            *
   ;*       e.g., "*Apple-" gets completed to "*Apple-Menu*"         *
   ;*   - Works also with Dialog Boxes                               *
   ;*   - It's Small: more comments than code ;-)                    *
   ;* Bugs, Problems: Haven't found a good strategy yet to deal with *
   ;*   package prefixes of partial completions:                     *
   ;*    - use partial completion only if all symbol from same       *
   ;*      package?                                                  *
   ;*    - strip package prefix?                                     *
   ;*      Any ideas?                                                *
   ;*   Completion can be slow in very crowded packages (e.g., :ccl) *
   ;* Acknowledgements:                                              *
   ;*   Guillaume Cartier (cartier@mipsmath.math.uqam.ca)            *
   ;*     preserve keyword qualifier in partial completion           *
   ;*                                                                *
   ;******************************************************************

(in-package :ccl)

(defvar *Maximum-Pop-up-Size* 100
  "if the number of matches exceeds this number then warn user")

;----------------------------------------------
;  high-level completion functions             |
;----------------------------------------------

(defmethod COMPLETE-SYMBOL ((Self fred-mixin)) "
  in: Self {fred-mixin}.
  Try to complete the selected symbol using a decreasingly constraining
  search through existing (interened) symbols."
  (let ((*Package* (or (window-package Self) *Package*)))
    (multiple-value-bind (String Start End Package Package-Qualifier) 
        (selected-symbol-string Self)
      (when String
        (catch :complete-symbol
          (buffer-replace-string
           Self
           Start
           End
           (choose-one-symbol-name
            Self
            String
            (search-for-matching-symbols Self (string-upcase String) Package)
            Package-Qualifier)
           String)
          (when *Arglist-On-Space* (ed-arglist Self)))))))


(defmethod COMPLETE-SYMBOL-OR-TAB ((Self fred-mixin)) "
  in: Self {fred-mixin}.
  Try to complete the selected symbol using a decreasingly constraining
  search through existing (interened) symbols. If there is no symbol to the left do a tab."
 (let ((Expression (buffer-current-sexp (fred-buffer Self))))
   ;; tricky heuristic: complete if there is a symbol, otherwise indent
   (if (and (symbolp Expression) (not (null Expression))) 
     (complete-symbol Self)
     (ed-indent-for-lisp Self))))


(defmethod SELECTED-SYMBOL-STRING ((Self fred-mixin)) "
  in:  Self {fred-mixin}.
  out: String {string} or nil, Start {position}, End {position}, Package {string}.
  Return the selected symbol (the one highlighted or just being to the
  left of the insertion marker."
  (let ((Mark (fred-buffer Self)))
    (multiple-value-bind (Start End) (buffer-current-sexp-bounds Mark)
      (when Start 
        (let* ((Start (case (buffer-char Mark Start)
                        (#\# 
                         (if (< (1+ Start) End)
                           (case (buffer-char Mark (1+ Start))
                             ((#\' #\`) (+ Start 2))
                             (t (1+ Start)))
                           (1+ Start)))
                        ((#\' #\` #\,) (1+ Start))
                        (t Start)))
               (String (buffer-substring Mark End Start)))
          (values
           (string->name String)
           Start
           End
           (package-name (gracefully-find-package 
                          (or (string->package String) 
                              (window-package Self)
                              *Package*)))
           (string->package-qualifier String)))))))


(defmethod CHOOSE-ONE-SYMBOL-NAME ((Self fred-mixin) String Symbols Package-Qualifier) "
  in:  Self {fred-mixin}, String {string}, Symbols {list of: {symbol}},
       Package-Qualifier {string}.
  out: Name {string}.
  Pick a symbol close to <String> using the following strategy:
  |<Symbols>| = 0 -> <String>
  |<Symbols>| = 1 -> (first <Symbols>)
  |<Symbols>| > 1  and |common-prefix| = 0 -> user-choice
  |<Symbols>| > 1  and |common-prefix| > 0 -> common-prefix."
  (case (length Symbols)
    (0 (set-mini-buffer Self "no matches") 
     (throw :complete-symbol String))
    (1 (spn-print-to-string (first Symbols)))
    (t (multiple-value-bind (Prefix Common-Package) 
           (common-symbol-name-prefix Symbols)
         (if (> (length Prefix) (length String))
           (if (and Common-Package
                    (equal (string->package Package-Qualifier)
                           (package-name Common-Package)))
             (concatenate 'string Package-Qualifier Prefix)
             Prefix)
           (cond
            ((<= (length Symbols) *Maximum-Pop-Up-Size*)
             (spn-print-to-string (first (select-item-from-list 
                                      Symbols 
                                      :window-title "select completion"
                                      :table-print-function #'spn-print))))
            (t
             (set-mini-buffer Self "too many matches: ~A" (length Symbols))
             (throw :complete-symbol String))))))))


(defun COMMON-SYMBOL-NAME-PREFIX (Symbols) "
  in:  Symbols {list of: {symbol}}.
  out: Prefix {string} or nil, Common-Package {package}."
  (let ((First-String (symbol-name (first Symbols))))
    (values
     (with-output-to-string (S)
       (block done
         (dotimes (I (length First-String))
           (let ((Char (char First-String I)))
             (dolist (String (mapcar #'symbol-name (rest Symbols)) (princ Char S))
               (when (or (> I (length String)) (char/= (char String I) Char))
                 (return-from done nil)))))))
     (let* ((Packages (mapcar #'symbol-package Symbols))
            (Package (first Packages)))
       (when (every #'(lambda (P) (eq P Package)) Packages) Package)))))


(defmethod SEARCH-FOR-MATCHING-SYMBOLS ((Self fred-mixin) String Package) "
  in:  Self {fred-mixin}, String {string}, Package {string}.
  out: Matches {list of: {symbol}}. "
  ; 1) search prefix, same package
  (set-mini-buffer Self ".")
  (let* ((Local-Symbols (apropos-list String Package))
         (Symbols (matching-prefix-symbols String Local-Symbols)))
    (if Symbols
      Symbols
      ; 2) search prefix, all packages
      (progn
        (set-mini-buffer Self ".")
        (let* ((Global-Symbols (apropos-list String))
               (Symbols (matching-prefix-symbols String Global-Symbols)))
          (if Symbols
            Symbols
            ; 3) search substring, same package
            (progn
              (set-mini-buffer Self ".")
              (if Local-Symbols
                Local-Symbols
                ; 4) return any match: substring in all packages
                (progn
                  (set-mini-buffer Self ".")
                  Global-Symbols)))))))))
    

(defun MATCHING-PREFIX-SYMBOLS (String Symbols) "
  in:  String {string}, Symbols {list of: {symbol}}.
  out: Symbols {list of: {symbol}}.
  Return only the symbols of which <String> is a prefix."
  (let ((L (length String)))
    (remove-if-not
     #'(lambda (Symbol) (string= String (symbol-name Symbol) :end1 L :end2 L))
     Symbols)))

;----------------------------------------------
;  low-level functions                         |
;----------------------------------------------

(defmethod BUFFER-REPLACE-STRING ((Self fred-mixin) Start End String &optional Old-String) "
  in:  Self {fred-mixin}, Start End {position}, String {string}, 
       &optional Old-String {string}.
  Delete the buffer content between <Start> and <End>, insert
  <String> and place insertion marker to <End> position."
  (let ((Mark (fred-buffer Self)))
    (buffer-delete Mark Start End)
    (buffer-insert 
     Mark
     (if Old-String
       (case (string-format Old-String)
         (:upper (string-upcase String))
         (:lower (string-downcase String))
         (:capital (string-capitalize String)))
       String))))
                   

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


(defun GRACEFULLY-FIND-PACKAGE (Package) "
  in:  Package {string}.
  out: Package {string}.
  If FIND-PACKAGE doesn't find <Package> then prompt the user for an
  alternative."
  (or (find-package Package) 
      (find-package
       (first (select-item-from-list 
               (mapcar #'package-name (list-all-packages))
               :window-title "Select an existing Package")))))


(defun STRING->PACKAGE (String) "
  in:  String {string}. out: Package {string} or nil.
  Return package prefix of string (if any)"
  (let ((P (position #\: String)))
    (when P
      (if (zerop P)
        "KEYWORD"
        (string-upcase (subseq String 0 P))))))


(defun STRING->NAME (String) "
  in: String {string}. out: Name {string}.
  Return <string> without a package prefix."
  (let ((P (position #\: String)))
    (if P
      (string-left-trim ":" (subseq String P))
      String)))


(defun STRING->PACKAGE-QUALIFIER (String) "
  in:  String {string}.
  out: Package-Qualifer {string}. "
  (let ((P (position #\: String)))
    (when P
      (if (zerop P)
        ":"
        (if (and (< P (1- (length String))) (char= (char String (1+ P)) #\:))
          (subseq String 0 (+ P 2))
          (subseq String 0 (+ P 1)))))))

;----------------------------------------------
;  Shortest Package Names                      |
;----------------------------------------------

#-:mcl       ; MCL defines this function, but .. just in case
(defun SHORTEST-PACKAGE-NICKNAME (Package) "
  in:  Package {package}.
  out: Name {string}.
  Return the package name or if there are any nicknames
  return the shortest nickname."
  (let ((Nicknames (package-nicknames Package)))
    (if Nicknames
      (let ((Shortest-Name (first Nicknames)))
    (dolist (Name (rest Nicknames))
          (when (< (length Name) (length Shortest-Name))
            (setq Shortest-Name Name)))
        Shortest-Name)
      (package-name Package))))


(defun SPN-PRINT (Symbol &optional Stream) "
  in: Symbol {symbol}, &optional Stream {output-stream}.
  Like PRIN1 for symbols but use shortest package nicknames for 
  package qualifiers (if any)."
  (unless (find-symbol (symbol-name Symbol))
    (let ((Home-Package (symbol-package Symbol)))
      (unless (eq Home-Package (find-package :keyword))
        (princ (shortest-package-nickname Home-Package) Stream))
      (multiple-value-bind (S Status) 
           (find-symbol (symbol-name Symbol) Home-Package)
        (declare (ignore S))
        (ecase Status
          (:internal (princ "::" Stream))
          (:external (princ ":" Stream))))))
  (princ Symbol Stream))


(defun SPN-PRINT-TO-STRING (Symbol) "
  in:  Symbol {symbol}.
  out: String {string}.
  Write <Symbol> to <String>. Use shortest package qualifiers (if any)"
  (with-output-to-string (stream)
    (spn-print Symbol Stream)))
    
;----------------------------------------------
;  keyboard bindings                           |
;----------------------------------------------

;this overwrites the set-mark EMACS command

(comtab-set-key *Comtab* '(:control #\space) 'complete-symbol "complete symbol")
;; (comtab-set-key *Comtab* '(#\tab) 'complete-symbol-or-tab "complete symbol or indent")
  
#| stuff:

(common-symbol-name-prefix '(bla-lkfdflkdf blappppppp bla)) -> "BLA"
(common-symbol-name-prefix (apropos-list "WITH-OP")) -> "WITH-OPEN-"

(string->package "dsdsd")  -> nil
(string->package "ccl::gc")  -> "CCL"
(string->package ":rest") -> "KEYWORD"

(shortest-package-nickname :common-lisp-user) => "CL-USER"

|#