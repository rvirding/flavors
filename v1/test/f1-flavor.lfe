(defmodule f1-flavor
  (export ($handle_undefined_function 2))
  (export (x 2) (y 2) (z 2) (set-x 2) (set-y 2) (set-z 2))
  (export (one 2) (two 2)))

(defun $handle_undefined_function (m _)
  (error (tuple 'undefined_method m)))

(defun x (self args)
  (f1-flavor-core:method 'x self args))

(defun y (self args)
  (f1-flavor-core:method 'y self args))

(defun z (self args)
  (f1-flavor-core:method 'z self args))

(defun set-x (self args)
  (f1-flavor-core:method 'set-x self args))

(defun set-y (self args)
  (let* (((tuple ret self) (f1-flavor-core:method 'set-y self args))
	 (self (f1-flavor-core:after-daemon 'set-y self args)))
    (tuple ret self)))

(defun set-z (self args)
  (f1-flavor-core:method 'set-z self args))

(defun one (self args)
  (let* ((self (f1-flavor-core:before-daemon 'one self args))
	 ((tuple ret self) (f1-flavor-core:method 'one self args))
	 (self (f1-flavor-core:after-daemon 'one self args)))
    (tuple ret self)))

(defun two (self args)
  (let* ((self (f1-flavor-core:before-daemon 'two self args))
	 ((tuple ret self) (f1-flavor-core:method 'two self args)))
    (tuple ret self)))
