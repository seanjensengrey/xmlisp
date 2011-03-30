;;-*- Mode: Lisp; Package: LUI -*- 
;*********************************************************************
;*                                                                   *
;*                          S P E E C H                              *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2009, AgentSheets Inc.                    *
;* Filename     : speech.lisp                                        *
;* Last Update  : 05/14/09                                           *
;* Version      :                                                    *
;*    0.1       : 01/03/04 hack                                      *
;*    1.0       : 05/14/09                                           *
;* Systems      : G4, OS X 10.5.6                                    *
;* Lisps        : CCL 1.3                                            *
;* Licence      : LGPL                                               *
;*********************************************************************

(in-package :lui)

#-cocotron
(progn

;;___________________________________
;; Native SYNTHESIZER               |
;;___________________________________

(defclass NATIVE-SYNTHESIZER (ns:NS-Speech-Synthesizer)
  ((lui-synthesizer :accessor lui-synthesizer :initform nil :initarg :lui-synthesizer))
  (:metaclass ns:+ns-object))


(defmethod INITIALIZE-INSTANCE :after ((Self native-synthesizer) &rest Args)
  (declare (ignore Args))
  (#/setDelegate: Self Self))


;;___________________________________
;; LUI Synthesizer                   |
;;___________________________________

(defclass SYNTHESIZER ()
  ((native-synthesizer :accessor native-synthesizer :initarg :native-synthesizer))
  (:documentation "Speech Synthesizer"))


(defparameter *Default-Synthesizer* nil)


(defgeneric SPEAK (text &optional voice synthesizer)
  (:documentation "Speak <Text> in <Voice> using <Synthesizer>"))


(defgeneric WILL-SPEAK-WORD (synthesizer word)
  (:documentation "is called before the word is spoken"))


(defgeneric WILL-SPEAK-PHONEME (synthesizer phoneme)
  (:documentation "is called before the phoneme is spoken"))


(defgeneric DID-FINISH-SPEAKING (synthesizer)
  (:documentation "is called after speaking has finished"))


(defun APPLE-VOICE (Name)
  (native-string (format nil "com.apple.speech.synthesis.voice.~A" Name)))


(defparameter *Speech-Lock* nil "look preventing speech interupting each other")


(defmethod SPEAK ((Text string) &optional Voice Synthesizer )
  (unless *Speech-Lock* 
    (setq *Speech-Lock* (make-lock "Speech")))
  (process-run-function 
   "as we speak"
   #'(lambda ()
       (ccl::with-autorelease-pool
           (with-lock-grabbed (*Speech-Lock*)
             (unless Synthesizer
               (unless *Default-Synthesizer* 
                 (if synthesizer-type
                   (setf *Default-Synthesizer* (make-instance synthesizer-type))
                   (setf *Default-Synthesizer* (make-instance 'synthesizer)))
                 (setf (native-synthesizer *Default-Synthesizer*) (make-instance 'native-synthesizer :lui-synthesizer *Default-Synthesizer*)))
               (setq Synthesizer *Default-Synthesizer*))
             (cond
              (Voice
               (unless (#/setVoice: (native-synthesizer Synthesizer) (apple-voice Voice))
                 (unless (member Voice (available-voices) :test #'string=)
                   (error "Voice ~S is not one of ~S" Voice (available-voices)))))
              (t
               (#/setVoice: (native-synthesizer Synthesizer) (%null-ptr))))
             (when (should-start-speaking synthesizer)
               (#/startSpeakingString: (native-synthesizer Synthesizer) (native-string Text))
               (loop
                 (unless (#/isSpeaking (native-synthesizer Synthesizer)) (return))
                 (sleep 0.1))))))))


(defun AVAILABLE-VOICES ()
  (let ((Voices nil) (Native-Voices (#/availableVoices ns:NS-Speech-Synthesizer)))
    (dotimes (i (#/count Native-Voices) (reverse Voices))
      (let ((Name (ccl::lisp-string-from-nsstring (#/objectAtIndex: Native-Voices i))))
        (push (subseq Name (1+ (position #\. Name :from-end t))) Voices)))))


;;; Default Callback implementation

(defmethod WILL-SPEAK-WORD ((Self synthesizer) Word)
  (declare (ignore Word))
  ;(format t "~%word:~S" Word)
  )


(defmethod SHOULD-START-SPEAKING ((Self synthesizer))
  t)



(defmethod WILL-SPEAK-PHONEME ((Self synthesizer) Phoneme)
  (declare (ignore Phoneme))
  ;(format t "~%phoneme code:~S" Phoneme)
  )


(defmethod DID-FINISH-SPEAKING ((Self synthesizer))
  ;(format t "~%done")
  )

;;___________________________________
;; Callbacks                         |
;;___________________________________

(objc:defmethod (#/speechSynthesizer:willSpeakWord:ofString: :void) ((Self native-synthesizer) sender (wordToSpeak #>NSRange) text)
  (declare (ignore sender))
  (will-speak-word 
   (lui-synthesizer Self) 
   (subseq (ccl::lisp-string-from-nsstring text) 
           (rref wordToSpeak #>NSRange.location) 
           (+ (rref wordToSpeak #>NSRange.location)
              (rref wordToSpeak #>NSRange.length)))))


(objc:defmethod (#/speechSynthesizer:willSpeakPhoneme: :void) ((Self native-synthesizer) sender (phonemeOpcode :short))
  (declare (ignore sender))
  (will-speak-phoneme (lui-synthesizer Self) phonemeOpcode))


(objc:defmethod (#/speechSynthesizer:didFinishSpeaking: :void) ((Self native-synthesizer) sender (didFinishSpeaking :<BOOL>))
  (declare (ignore sender))
  (did-finish-speaking (lui-synthesizer Self)))


#| Examples:

(speak "Hello")

(speak "Speech Synthesis, also called text-to-speech (TTS), parses text and converts it into audible speech. 
 It offers a concurrent feedback mode that can be used in concert with or in place of traditional visual and aural notifications")

(speak "Hello you! Hello you?" "Victoria")
(speak "Hello you! Hello you?" "GoodNews")
(speak "Hello you! Hello you?" "Goodnews")  ;; should raise error: typo in voice name

(available-voices)

|#
)