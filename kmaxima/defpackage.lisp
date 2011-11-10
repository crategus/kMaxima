;;; ----------------------------------------------------------------------------
;;; defpackage.lisp
;;;
;;; Copyright (C) 2011 Dr. Dieter Kaiser
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------------------

(in-package :cl-user)

(defpackage :kmaxima
  (:nicknames :kmaxima)
  (:use :cl)
  (:shadow #:float)
  (:export
    ;; Constants
    #:$%e #:$%pi #:$%gamma #:$%phi
    
    ;; General symbols
    #:$false
    #:$true
    #:displayinput
    #:simp
    #:mspec
    
    ;; mmacros.lisp
    #:ncons #:while #:defun-prop #:defmspec #:defmvar #:errset
    
    ;; parser.lisp
    #:$+     #:$-      #:$*    #:$**   #:$^    #:$^^
    #:$<     #:$<=     #:$=    #:$>    #:$>=   #:|$(|
    #:|$)|   #:|$[|    #:|$]|  #:|$,|  #:|$:|  #:|$:=|
    #:|$::|  #:|$::=|  #:|$!|  #:|$#|  #:|$'|  #:|$''|
    #:|$$|   #:|$;|    #:|$&|  #:|$&&|
    
    #:*maxima-operators*
    #:*parse-stream*
    #:*parse-stream-eof*
    #:*parse-tyi*
    #:*scan-buffered-token*
    #:gobble-comment
    #:tyi
    #:parse-tyi
    #:parse-tyi-peek
    #:unparse-tyi
    #:mread
    #:peek-one-token
    #:scan-one-token
    #:scan-operator-token
    #:scan-string
    
    ;; nformat.lisp
    #:$powerdispflag
    #:$ratdispflag
    #:$%edispflag
    #:$exptdispflag
    #:$sqrtdispflag
    #:$negsumdispflag
    #:nformat
    #:nformat-all
    #:nformat-mplus
    #:nformat-mtimes
    #:nformat-mexpt
    
    ;; display.lisp
    #:*in-display-p*
    #:*linearray*
    #:*lines*
    #:*level*
    #:*break*
    #:*size*
    #:*bkpt*
    #:*bkptout*
    #:*bkptwd*
    #:*bkptdp*
    #:*bkptht*
    #:*bkptlevel*
    #:*width*
    #:*height*
    #:*depth*
    #:*right*
    #:*maxht*
    #:*maxdp*
    #:*oldrow*
    #:*oldcol*
    #:*mratp*
    
    #:mdisplay
    #:dimension
    #:dimension-nary
    #:dimnary
    #:dim-mquotient
    #:d-hbar
    #:dratio
    #:checkfit
    
    ;; float.lisp
    
    #:bigfloatzero
    #:bigfloatone
    #:bfhalf
    
    #:fpformat
    #:$bfloat
    
    
    ;; More symbols ...
    #:$quit
    #:$sqrtdispflag
    #:add2lnc
    #:alphabetp
    #:defprop
    #:exploden
    #:fixnump
    #:getprop
    #:getpropl
    #:implode
    #:mexpt
    #:mfunctionp
    #:mlist
    #:mlistp
    #:mminus
    #:mminusp
    #:mparen
    #:mplus
    #:mquotient
    #:mtimes
    #:putprop
    #:rat
    
    #:%sqrt
    #:*alphabet*
    
    ))

