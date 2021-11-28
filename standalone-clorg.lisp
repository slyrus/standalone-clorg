
(in-package #:clim-user)

(defclass org-item ()
  ((name :accessor name :initarg :name :initform nil)
   (role :accessor role :initarg :role :initform nil))
  (:documentation "Base class of items to go on an org chart"))

(defclass employee (org-item) ())

(defclass org-chart ()
  ((count :accessor org-chart-employee-counter :initform 1)
   (hash-table :accessor org-chart-hash-table :initform (make-hash-table :test 'equal))))

(defun get-employee (org-chart id)
  (gethash id (org-chart-hash-table org-chart)))

(defun add-employee (org-chart emp-spec)
  (let ((name (getf emp-spec :name))
        (role (getf emp-spec :role)))
    (let* ((emp (make-instance 'employee :name name :role role))
           (emp-id (org-chart-employee-counter org-chart)))
      (setf (gethash emp-id (org-chart-hash-table org-chart)) emp)
      (incf (org-chart-employee-counter org-chart))
      emp-id)))

(defun make-org-chart (employee-spec-list)
  (let ((org-chart (make-instance 'org-chart)))
    (loop for emp-spec in employee-spec-list
       do (add-employee org-chart emp-spec))
    org-chart))

(defparameter *eu-employees*
  '((:name "Ursula Von Der Leyen" :role "President of The European Comission")))

(defparameter *eu-org-chart* (make-org-chart *eu-employees*))

(define-application-frame clorg ()
  ()
  (:pane
   (scrolling ()
     (make-pane :interactor)))
  (:menu-bar nil)
  (:default-initargs :width 1000 :height 800))

(defun running-sum (list)
  (let ((sum 0))
    (mapcar (lambda (x) (incf sum x))
            list)))

(defun org-chart-arc-drawer (stream from-node to-node x1 y1 x2 y2
                             &rest drawing-options
                             &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (let ((mid-y (/ (+ y1 y2) 2)))
    (apply #'draw-line* stream x1 y1 x1 mid-y drawing-options)
    (apply #'draw-line* stream x1 mid-y x2 mid-y drawing-options)
    (apply #'draw-line* stream x2 mid-y x2 y2 drawing-options)))

(define-presentation-type org-chart-node ())

(defparameter *box-color* (clim:make-rgb-color 0.84 0.88 0.89))

(define-presentation-method present (emp (type employee) pane view &key)
  (let* ((name (name emp))
         (role (role emp))
         (sizes (mapcar (lambda (x)
                         (if x
                             (multiple-value-list (text-size pane x))
                             (list 0 0)))
                        (list name role)))
         (widths (mapcar #'first sizes))
         (max-width (apply #'max widths))
         (heights (mapcar #'second sizes))
         (offsets (running-sum heights)))
    (when name
      (climi::invoke-surrounding-output-with-border
       pane
       (lambda (pane)
         (draw-text* pane name
                     (/ max-width 2)
                     (first offsets)
                     :align-x :center
                     :align-y :bottom
                     :text-face :bold)
         (when role
           (draw-text* pane (role emp)
                       (/ max-width 2)
                       (second offsets)
                       :align-x :center
                       :align-y :bottom)))
       :shape :rectangle
       :filled t
       :ink *box-color*
       :outline-ink *box-color*
       :line-thickness 3
       :padding-left 12
       :padding-right 12
       :padding-top 4
       :padding-bottom 4))))

(defclass clorg-pane (application-pane)
  ((org-chart :initarg :org-chart :initform nil :accessor pane-org-chart)
   (orientation :initarg :orientation :initform :vertical :accessor pane-orientation)))

(define-presentation-method present (org-chart-node (type org-chart-node) pane view &key)
  (let* ((org-chart (pane-org-chart pane))
         (emp (get-employee org-chart org-chart-node)))
    (present emp 'employee)))

(define-presentation-type org-chart ())

(define-presentation-method present (org-chart (type org-chart) pane view &key)
  (with-drawing-options (pane :text-style (make-text-style nil nil 12))
    (flet ((node-children (node)
             (declare (ignore node))
             ))
    (format-graph-from-roots
     (list 1)
     (lambda (node s)
       (present (get-employee org-chart node) 'employee :stream s))
     #'node-children
     :arc-drawer #'org-chart-arc-drawer
     :arc-drawing-options (list :ink +gray20+ :line-thickness 2)
     :orientation (typecase pane
                    (clorg-pane (pane-orientation pane))
                    (t :vertical))
     :stream pane))))



;; to run, enter the following in the clim-listener:
;;
;; (with-room-for-graphics (*standard-output* :first-quadrant nil)
;;   (present clim-user::*eu-org-chart*))
;;
