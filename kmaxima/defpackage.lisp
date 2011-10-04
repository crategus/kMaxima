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
  (:shadow float)
  (:export
    ;; General symbols
    #:$false
    #:$true
    #:simp
    
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
    
    
    #:$%e
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
    #:parse-tyi
    #:parse-tyipeek
    #:peek-one-token
    #:putprop
    #:rat
    #:scan-one-token
    #:tyi
    #:unparse-tyi
    
    #:%sqrt
    #:*alphabet*
    #:*parse-stream*
    #:*parse-stream-eof*
    
    ))

