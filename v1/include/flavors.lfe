;; Copyright (c) 2015 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : flavors.lfe
;; Author  : Robert Virding
;; Purpose : Basic macros for LFE Flavors.

(defmacro defflavor
  (`(,name ,vars ,comps . ,opts)
   (flavors_comp:defflavor name vars comps opts)))

(defmacro defmethod
  (`(,method . ,def) (flavors_comp:defmethod method def)))

(defmacro endflavor (name) (flavors_comp:endflavor name))

(defmacro make-instance
  (`(,name . ,opts) `(flavors:instantiate-flavor ,name ,opts)))

(defmacro send
  (`(,self ,message . ,args) `(flavors:send ,self ,message ,args)))
