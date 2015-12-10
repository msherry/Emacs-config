;;; fxrd-validators.el --- Validators for fixed-field-width files  -*- lexical-binding:t -*-
;;; We need lexical-binding so we can create closures.

(require 'eieio-base)

(defclass fxrd-validator (eieio-named)
  (;; Public slots
   (pad :initarg :pad
        :initform ""
        :type string
        :custom string
        :documentation "The value to pad with")
   (align :initarg :align
          :initform "RIGHT"
          :type string
          :custom string
          :documentation "The alignment of the field")
   (const :initarg :const
          :initform nil
          :type (or null integer string)
          :custom (or null integer string)
          :documentation "A constant value for this field")
   (enum :initarg :enum
         :initform nil
         :type list
         :custom list
         :documentation "Possible enum values for this field")
   (min :initarg :min
        :initform nil
        :type (or null integer)
        :custom (or null integer)
        :documentation "Minimum value for this field")
   (max :initarg :max
        :initform nil
        :type (or null integer)
        :custom (or null integer)
        :documentation "Maximum value for this field")
   ;; Private slots
   (comp-transform :initform #'identity
                   :documentation "Transform to be used when comparing fields")
   (const-eq :initform #'eq
             :documentation "Equality function for const values")
   (regex :initform "^.*$"
          :documentation "Regex to validate field against"))
  "The base validator class for all field validation types")
(defmethod fxrd-validate (val field)
  "Validate the field with the given validator"
  (fxrd-general-validator val field))

(defun fxrd-general-validator (val field-value)
  (let ((const (slot-value val 'const))
        (enum (slot-value val 'enum))
        (comp-transform (slot-value val 'comp-transform))
        (const-eq (slot-value val 'const-eq))
        (pad (slot-value val 'pad))
        (regex (slot-value val 'regex)))
    ;; TODO: alignment goes here
    (and (string-match (concat "^" pad "*" regex "$") field-value)
         (if enum (member (funcall comp-transform field-value) enum)
           t)
         (if const (funcall const-eq const (funcall comp-transform field-value))
           t))))


(defclass fxrd-numeric-v (fxrd-validator)
  ((pad :initform "0")
   (comp-transform :initform #'string-to-int)
   (regex :initform "[[:digit:]]*"))
  "Integer fields")
(defmethod fxrd-validate ((val fxrd-numeric-v) field-value)
  (let ((value (funcall (slot-value val 'comp-transform) field-value))
        (min (slot-value val 'min))
        (max (slot-value val 'max)))
    (and (fxrd-general-validator val field-value)
         (cond ((and min max) (<= min value max))
               (min (<= min value))
               (max (<= value max))
               (t t)))))

(defclass fxrd-decimal-v (fxrd-numeric-v)
  ((comp-transform :initform #'string-to-number)
   (regex :initform "[[:digit:]]*\\(\\.[[:digit:]]+\\)"))
  "Numeric fields with a decimal point (floating-point values)")

(defclass fxrd-alphanumeric-v (fxrd-validator)
  ((pad :initform " ")
   (const-eq :initform #'string=)
   (regex :initform "[[:print:]]*" field-value))
  "Any printable characters")

(provide 'fxrd-validators)
