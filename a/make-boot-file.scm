(import (chezscheme))


(apply make-boot-file "arew.boot" '()
       (format #f "../boot/~a/petite.boot" (machine-type))
       (format #f "../boot/~a/scheme.boot" (machine-type))
       (cdr (command-line)))
