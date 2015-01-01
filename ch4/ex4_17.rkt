;;; Ex 4.17

(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

;;; Sequential
;;;
;;; In the sequential model, there is only one frame for the lambda and the
;;; definitions are installed in it. When <e3> is evaluated, u and v
;;; have been installed.
;;;
;;; First <e1> is evaluated and assigned to u, then <e2> is evaluated
;;; and assigned to v. At the time <e1> is evaluated, u and v have not
;;; been installed in the current frame. At the time <e2> is
;;; evaluated, v has not been installed. This can affect how they are
;;; evaluated if <e1> or <e2> uses u or v.
;;;
;;; ===========
;;; = lambda       =
;;; ===========
;;; = <vars>  =
;;; = u: <e1> =
;;; = v: <e2> =
;;; ===========

;;; Scanned out

(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u <e1>)
    (set! v <e2>)
    <e3>))

;;; In the scanned out version, there is a new frame for the let bindings.
;;;
;;; ===========       ==========
;;; = let     =       = lambda      =
;;; =========== ----> ==========
;;; = u: <e1> =       = <vars> =
;;; = v: <e2> =       ==========
;;; ===========
;;;
;;; First u and v are installed as bindings in the let frame,
;;; initially set to *unassigned*. Then <e1> is assigned to u, then
;;; <e2> is assigned to v. If <e1> or <e2> uses u or v, it is the u
;;; and v bound in the current let frame.
;;;
;;; Sequential vs scanned out won't make a difference as long as at
;;; the time <e1> and <e2> are evaluated the evaluation doesn't depend
;;; on the u and v installed in the current frame. I think this is
;;; what is meant by a correct program. If the evaluation does depend
;;; on u and v in the current frame, then the scanned out version
;;; could produce an error by trying to evaluate *unassigned* while
;;; the sequential version would look for a u or v from a different
;;; binding.
;;;
;;; Instead of scanning out defines and building a let environment,
;;; the interpreter could first scan for defines and install them
;;; directly in the current environment with a binding initially set
;;; to *unassigned*. This would make internal definitions have
;;; simultaneous scope without creating an extra environment frame.
