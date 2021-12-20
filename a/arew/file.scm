(alias call-with-input-file chez-call-with-input-file)

(alias call-with-output-file chez-call-with-output-file)

(alias delete-file chez-delete-file)

(alias file-exists? chez-file-exists?)

(define (open-binary-input-file file)
  (r6rs-open-file-input-port file))

(define (open-binary-output-file file)
  (r6rs-open-file-output-port file))

(alias open-input-file chez-open-input-file)

(alias open-output-file chez-open-output-file)

(alias with-input-from-file chez-with-input-from-file)

(alias with-output-to-file chez-with-output-to-file)
