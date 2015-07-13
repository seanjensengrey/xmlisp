(in-package :xlui)


(defclass OBJ-OBJECT (agent-3d)
  ((vertices :accessor vertices :initform nil :documentation "List of vertices")
   (file :accessor file :initform nil :documentation "file name")
   (faces :accessor faces :initform nil :documentation "List of faces")))


(defmethod INITIALIZE-INSTANCE :after ((Self obj-object) &rest Initargs)
  (declare (ignore Initargs))
  (when (file Self)
    (read-obj-file Self (file Self))))


(defmethod DRAW ((Self obj-object))
  (dolist (Face (faces Self))
    (glBegin GL_POLYGON)
    (dolist (Face-Indicies Face)
      ;; normal
      (let ((Normal (aref (vertices Self) (1- (third Face-Indicies)))))
        (glNormal3f (first Normal) (second Normal) (third Normal)))
      ;; vertex
      (let ((Vertex (aref (vertices Self) (1- (first Face-Indicies)))))
        (glVertex3f (first Vertex) (second Vertex) (third Vertex))))
     (glEnd)))


(defun SKIP-SPACES (File)
  (loop
    (let ((Char (read-char File)))
      (case Char
        (#\space)
        (t (unread-char Char File)
           (return))))))


(defun READ-INTEGER (File)
  (skip-spaces File)
  (let ((Value 0))
    (loop
      (let ((Char (read-char File)))
        (cond
         ;; #\0 .. #\9
         ((digit-char-p Char)
          (setf Value (+ (* Value 10) (- (char-code Char) (char-code #\0)))))
         ;; we are done
         (t
          (unread-char Char file)
          (return Value)))))))


(defun READ-FACES (File)
  (let ((Faces nil))
    (loop
      (let ((Face (read-integer File)))
        (push Face Faces)
        (let ((Next-Char (read-char File)))
          (if (char= Next-Char #\Newline)
            (return (reverse Faces))
            (unread-char Next-Char File)))))))


(defmethod READ-OBJ-FILE ((Self obj-object) Pathname)
  (with-open-file (File Pathname :direction :input)
    (loop
      (let ((Command (read-char File nil nil)))
        (unless Command (return Self))
        (case Command
          ;; comment
          (#\# 
           (print "comment")
           (read-line File))
          ;; vertex
          (#\v
           (print "vertex")
           ;;(format t "~%vertex: x=~A, y=~A, z=~A" (read File) (read File) (read File))
           (push (list (read File) (read File) (read File)) (vertices Self)))
          ;; face
          (#\f
           (print "face")
           (push (read-faces File) (faces Self)))
          ;; material
          (#\m
           (unread-char Command File)
           (format t "~%found: ~A" (read File))
           (read-line File))
          ;; otherwise
          (t
           (format t "~%found: ~A" Command)
           (read-line File)))))
    ;; fix order
    (setf (vertices Self) (coerce (reverse (vertices Self)) 'array))  ;; keep vertices as arrays
    (setf (faces Self) (reverse (faces Self)))
    ;; return the object itself
    Self))




#| Examples:

(inspect <obj-object file="/Volumes/Home Common Ground/sources/XMLisp/The Scary game/obj models/Withnormals.obj"/>)


(inspect  (make-instance 'obj-object))

(read-obj-file "~/Desktop/testing test.obj")

(read-obj-file "~/Desktop/obj with 3 faces.obj")

(ed "~/Desktop/testing test.obj")



<application-window margin="0">
  <agent-3d-view>
    <obj-object file="~/Desktop/testing test.obj"/>
  </agent-3d-view>
</application-window>


<application-window margin="0">
  <agent-3d-view>
    <obj-object file="~/Desktop/2cubes.obj/"/>
  </agent-3d-view>
</application-window>


<application-window margin="0">
  <agent-3d-view>
    <obj-object file="~/Desktop/airboat.obj"/>
  </agent-3d-view>
</application-window>


<application-window margin="0">
  <agent-3d-view>
    <obj-object file="~/Desktop/shuttle.obj"/>
  </agent-3d-view>
</application-window>






|#