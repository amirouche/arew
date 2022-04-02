;; This file was part of SINK, a Scheme-based Interpreter for
;; Not-quite Kernel. Reworked under the name: aeqa.
;;
;; Copyright (c) 2009 John N. Shutt
;; Copyright (c) 2022 Amirouche A. BOUBEKKI
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public License
;; as published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for more details.
;;
;; You should have received a copy of the GNU Library General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  This
;; file was part of SINK, a Scheme-based Interpreter for Not-quite
;; Kernel

(import (arew))



(define kernel-eval
  (lambda (exp env ctx)

    (define combine
      (lambda (combiner operands env ctx)
        (cond
         ((kernel-operative? combiner) (operate combiner operands env ctx))
         ((kernel-applicative? combiner)
          (combine (kernel-applicative-unwrap combiner)
                   (map (lambda (o) (kernel-eval o env context)) operands)))
         (else (error-pass (make-error-descriptor (list "Tried to call a non-combiner: " (list combiner)))
                           context)))))

    (cond
     ((kernel-pair? exp) (combine (kernel-eval (kernel-car exp) env ctx) (kernel-cdr exp) env ctx))
     ((symbol? exp) (environment-ref exp env ctx))
     (else exp))))
