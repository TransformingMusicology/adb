;;; the whole world's a little-endian machine
(defun write-ub32 (ub32 out)
  (dotimes (i 4)
    (write-byte (ldb (byte 8 (* i 8)) ub32) out)))

(defun write-df (df out)
  (write-ub32 (sb-kernel:double-float-low-bits df) out)
  (write-ub32 (sb-kernel:double-float-high-bits df) out))

(defun csv-to-ffte (line first out)
  (when first
    (write-ub32 (- (length line) 2) out))
  (dolist (thing (cddr line))
    (let* ((obj (read-from-string thing))
           (df
            (typecase obj
              (number (float obj 1d0))
              ((eql -inf) sb-ext:double-float-negative-infinity))))
      (write-df df out))))

(let ((infile (elt sb-ext:*posix-argv* 1))
      (outfile (elt sb-ext:*posix-argv* 2))
      (first-line t))
  (with-open-file (out outfile
                       :if-exists :supersede
                       :direction :output
                       :element-type '(unsigned-byte 8))
    (flet ((frob (line)
             (csv-to-ffte line first-line out)
             (setf first-line nil)))
      (csv-parser:map-csv-file infile #'frob))))
