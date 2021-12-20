(alias fixnum? chez-fixnum?)

(alias fx* chez-fx*)

(alias fx*/carry chez-fx*/carry)

(alias fx+ chez-fx+)

(alias fx+/carry chez-fx+/carry)

(alias fx- chez-fx-)

(alias fx-/carry chez-fx-/carry)

(alias fx-greatest chez-greatest-fixnum)

(alias fx-least chez-least-fixnum)

(alias fx-width chez-fixnum-width)

(alias fx<=? chez-fx<=?)

(alias fx<? chez-fx<?)

(alias fx=? chez-fx=?)

(alias fx>=? chez-fx>=?)

(alias fx>? chez-fx>?)

(alias fxabs chez-fxabs)

(alias fxand chez-fxand)

(alias fxarithmetic-shift chez-fxarithmetic-shift)

(alias fxarithmetic-shift-left chez-fxarithmetic-shift-left)

(alias fxarithmetic-shift-right chez-fxarithmetic-shift-right)

(alias fxbit-count chez-fxbit-count)

(alias fxbit-field chez-fxbit-field)

(alias fxbit-field-reverse chez-fxreverse-bit-field)

(define fxbit-field-rotate (lambda (i c s e) (chez-fxrotate-bit-field i s e c)))

(alias fxbit-set? chez-fxbit-set?)

(alias fxfirst-set-bit chez-fxfirst-bit-set)

(alias fxcopy-bit chez-fxcopy-bit)

(alias fxeven? chez-fxeven?)

(alias fxif chez-fxif)

(alias fxior chez-fxior)

(alias fxlength chez-fxlength)

(alias fxmax chez-fxmax)

(alias fxmin chez-fxmin)

(define fxneg (lambda (i) (fx- i)))

(alias fxnegative? chez-fxnegative?)

(alias fxnot chez-fxnot)

(alias fxodd? chez-fxodd?)

(alias fxpositive? chez-fxpositive?)

(alias fxquotient chez-fxquotient)

(alias fxremainder chez-fxremainder)

(define fxsqrt (lambda (i) (exact-integer-sqrt i)))

(define fxsquare (lambda (i) (fx* i i)))

(alias fxxor chez-fxxor)

(alias fxzero? chez-fxzero?)
