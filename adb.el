(require 'json)

(defvar adb-directory)

(defvar adb-vamp-path
  (let ((env (getenv "VAMP_PATH")))
    (cond
     (env (split-string env (case system-type
                              ((ms-dos windows-nt) ";")
                              ((cygwin) ":")
                              (t ":"))))
     (t (case system-type
          ;; FIXME Program Files vs Program Files (x86)
          ((ms-dos windows-nt) (list "C:\\Program Files\\Vamp Plugins"))
          ;; FIXME
          ((cygwin) (list))
          ((darwin) (list (expand-file-name "~/Library/Audio/Plug-Ins/Vamp")
                          "/Library/Audio/Plug-Ins/Vamp"))
          (t (list (expand-file-name "~/vamp")
                   "/usr/local/lib/vamp")))))))

(defun adb-list-all-vamp-n3s ()
  (mapcan '(lambda (dir) (file-expand-wildcards (format "%s/*.n3" dir)))
          adb-vamp-path))

(defvar adb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'adb-list-ids)
    (define-key map (kbd "D") 'adb-select-db)
    (define-key map (kbd "Q") 'adb-do-query)
    (define-key map (kbd "i c") 'adb-select-qid-content)
    (define-key map (kbd "i l") 'adb-select-qid-length)
    (define-key map (kbd "i L") 'adb-select-qid-sequence-length)
    (define-key map (kbd "i s") 'adb-select-qid-start)
    (define-key map (kbd "i SPC") 'adb-qid-play-or-stop)
    (define-key map (kbd "SPC") 'adb-play-or-stop)
    (define-key map (kbd "$ a") 'adb-sort-album)
    (define-key map (kbd "$ c") 'adb-sort-artist)
    (define-key map (kbd "$ t") 'adb-sort-title)
    (define-key map (kbd "$ i") 'adb-sort-id)
    map))

(define-derived-mode adb-mode special-mode "adb"
  "Major mode for interacting with AudioDBs in adb format.\\<adb-mode-map>"
  (setq mode-name '("adb" (adb-current-db ("[" (:eval (adb-db-string adb-current-db)) "]")))))

(defun adb-status (&optional directory)
  (interactive "D")
  (setq directory (file-name-as-directory directory))
  (let* ((dirname (file-name-nondirectory (directory-file-name directory)))
         (buffer-name (format "*adb %s*" dirname))
         (buffer (get-buffer-create buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (adb-mode)
      (make-local-variable 'adb-directory)
      (make-local-variable 'adb-all-content)
      (make-local-variable 'adb-current-db)
      (make-local-variable 'adb-current-qid)
      (make-local-variable 'post-command-hook)
      (setq adb-directory directory
            default-directory (file-name-as-directory directory))
      (add-hook 'post-command-hook 'adb-post-command-hook))))

(defvar adb-all-content nil)

(defun adb-list-ids ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (setq adb-all-content nil)
  (let* ((default-directory adb-directory)
         (dirs (file-expand-wildcards "id/*/*/*")))
    (dolist (dir dirs)
      (let* ((id (file-name-nondirectory dir))
             (content (adb-content-for-id id))
             (title (adb-tag-value "title" content))
             (album (adb-tag-value "album" content))
             (artist (adb-tag-value "artist" content))
             (tracknumber (string-to-number (adb-tag-value "tracknumber" content)))
             (discnumber (string-to-number (adb-tag-value "discnumber" content))))
        (insert (propertize id 'adb-id id 'adb-content content
                            'adb-title title 'adb-album album
                            'adb-artist artist 'adb-tracknumber tracknumber
                            'adb-discnumber discnumber)
                (propertize "\n" 'intangible t))
        (push content adb-all-content))))
  (read-only-mode +1))

(defun adb-post-command-hook ()
  (let ((content (get-text-property (point) 'adb-content)))
    (adb-update-metadata-window content)))

(defun adb-clear-intangibles ()
  (save-excursion
    (goto-char (point-min))
    (let* ((state (get-text-property (point) 'intangible))
           (start (if state (point))))
      (while (< (point) (point-max))
        (let ((next (next-single-property-change (point) 'intangible)))
          (if next
              (progn
                (goto-char next)
                (if state
                    (progn
                      (delete-region start next)
                      (when (> start (point-min))
                        (insert (propertize "\n" 'intangible t)))
                      (setq state nil))
                  (setq state t start next)))
            (insert (propertize "\n" 'intangible t))
            (delete-region (point) (point-max))))))))

(defun adb-add-sections (property &optional function)
  (unless function
    (setq function 'identity))
  (goto-char (point-min))
    (let ((current))
      (while (< (point) (point-max))
        (let ((new (funcall function (get-text-property (point) property))))
          (unless (and current (string= new current))
            (insert
             (propertize new 'face '(:weight bold) 'intangible t)
             (propertize "\n" 'intangible t))
            (setq current new)))
        (forward-line))))

(defun adb-sort-id ()
  (interactive)
  (save-excursion
    (read-only-mode -1)
    (adb-clear-intangibles)
    (adb-sort '(adb-id)))
  (read-only-mode +1))

(defun adb-sort-title ()
  (interactive)
  (save-excursion
    (read-only-mode -1)
    (adb-clear-intangibles)
    (adb-sort '(adb-title adb-artist))
    (adb-add-sections 'adb-title (lambda (x) (substring x 0 1))))
  (read-only-mode +1))

(defun adb-sort-artist ()
  (interactive)
  (save-excursion
    (read-only-mode -1)
    (adb-clear-intangibles)
    (adb-sort '(adb-artist adb-title))
    (adb-add-sections 'adb-artist))
  (read-only-mode +1))

(defun adb-sort-album ()
  (interactive)
  (save-excursion
    (read-only-mode -1)
    (adb-clear-intangibles)
    (adb-sort '(adb-album adb-discnumber adb-tracknumber))
    (adb-add-sections 'adb-album))
  (read-only-mode +1))

(defun adb-sort-predicate (a b)
  (when a
    (cond
     ((numberp (car a))
      (if (< (car a) (car b))
          t
        (if (= (car a) (car b))
            (adb-sort-predicate (cdr a) (cdr b)))))
     ((stringp (car a))
      (if (string< (car a) (car b))
          t
        (if (string= (car a) (car b))
            (adb-sort-predicate (cdr a) (cdr b))))))))

(defun adb-sort (props)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (1- (point-max)))
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line
                   (lambda ()
                     (let ((result
                            (mapcar (lambda (p) (get-text-property (point) p)) props)))
                       result))
                   nil
                   'adb-sort-predicate)))))

(defun adb-tag-value (tag content)
  (cadr
   (split-string
    (with-output-to-string
      (with-current-buffer standard-output
        (call-process "metaflac" nil t nil (format "--show-tag=%s" tag)
                      (adb-24ify content "content" "content"))))
    "[=\n]")))

(defun adb-content-metadata (content)
  (insert
   (propertize (adb-tag-value "artist" content) 'face '(:weight bold))
   ", "
   (propertize (adb-tag-value "title" content)
               'face '(:weight bold :slant italic))
   "\n"
   (propertize (adb-tag-value "album" content))
  "\n"))

(defun adb-update-metadata-window (content)
  (let ((buffer (get-buffer-create "*adb metadata*")))
    (let ((split-height-threshold 0))
      (display-buffer buffer nil t))
    (with-current-buffer buffer
      (erase-buffer)
      (setq truncate-lines t)
      (when content
        (adb-content-metadata content))
      (when (tabbar-mode-on-p)
        (tabbar-local-mode 1))
      (set-window-text-height (get-buffer-window buffer) 4)
      (set-window-parameter (get-buffer-window buffer) 'no-other-window t)
      (set-window-dedicated-p (get-buffer-window buffer) t))))

(defun adb-24ify (thing prefix postfix)
  (let ((thing2 (subseq thing 0 2))
        (thing4 (subseq thing 0 4)))
    (format "%s/%s/%s/%s/%s" prefix thing2 thing4 thing postfix)))

(defun adb-play (content)
  (interactive
   (list (get-text-property (point) 'adb-content)))
  (if (adb-player-process)
      (message "adb player already playing")
    (start-process "adb player" (adb-player-buffer) "play" "-q" (adb-24ify content "content" "content"))))

(defun adb-play-qid (qid)
  (interactive (list (adb-current-qid)))
  (unless (adb-qid-content qid)
    (error "No content in qid"))
  (when (adb-qid-start qid)
    (unless (adb-qid-length qid)
      (error "Start but no length in qid")))
  (if (adb-player-process) 
      (message "adb player already playing")
    (if (adb-qid-start qid)
        (let* ((length (adb-qid-length qid))
               (length-seconds (adb-lengthspec-seconds length)))
          (start-process "adb player" (adb-player-buffer) "play" "-q" (adb-24ify (adb-qid-content qid) "content" "content") "trim" (format "%s" (adb-qid-start qid)) (format "%s" length-seconds)))
      (adb-play (adb-qid-content qid)))))

(defun adb-player-buffer ()
  (let ((buffer (get-buffer-create "*adb player*")))
    buffer))
(defun adb-player-process ()
  (let ((buffer (adb-player-buffer)))
    (get-buffer-process buffer)))

(defun adb-stop ()
  (interactive)
  (let* ((process (adb-player-process)))
    (when process
      (kill-process process))))

(defun adb-play-or-stop (content)
  (interactive
   (list (get-text-property (point) 'adb-content)))
  (let ((process (adb-player-process)))
    (if process
        (adb-stop)
      (adb-play content))))

(defun adb-qid-play-or-stop (qid)
  (interactive
   (list (adb-current-qid)))
  (let ((buffer (get-buffer "*adb player*")))
    (if (and buffer (get-buffer-process buffer))
        (adb-stop)
      (adb-play-qid qid))))

(defun adb-content-for-id (id)
  (let* ((target (file-symlink-p (adb-24ify id "id" "content"))))
    (when target
      (file-name-nondirectory
       (directory-file-name (file-name-directory target))))))

(defun adb-ids-for-content (cid)
  )

(cl-defstruct (adb-db (:constructor adb-make-db)) feature power)
(defun adb-db-string (db)
  (if (adb-db-power db)
      (format "%s/%s" (adb-db-feature db) (adb-db-power db))
    (adb-db-feature db)))

(cl-defstruct (adb-qid (:constructor adb-make-qid)) content length start)

(defvar adb-current-db)
(defun adb-current-db ()
  (unless (boundp 'adb-current-db)
    (setq adb-current-db (adb-make-db)))
  adb-current-db)
(defvar adb-current-qid)
(defun adb-current-qid ()
  (unless (boundp 'adb-current-qid)
    (setq adb-current-qid (adb-make-qid)))
  adb-current-qid)

(defun adb-select-db (feature power)
  (interactive
   (let* ((feature-files (file-expand-wildcards "features/*"))
          (feature-names (mapcar 'file-name-nondirectory feature-files))
          (feature (completing-read "Feature: " feature-names nil t)))
     (list feature
           (completing-read "Power feature: " feature-names
                            (lambda (f)
                              (and (= (adb-feature-step-size f) (adb-feature-step-size feature))
                                   (= (adb-feature-bin-count f) 1)))
                            t))))
  (setq adb-current-db (adb-make-db :feature feature))
  (unless (string= power "")
    (setf (adb-db-power adb-current-db) power)))

(defun adb-roqet-json-single-binding (sparql &rest files)
  (let* ((roqet-file-args (cl-mapcan (lambda (x) (list "-D" x)) files))
         (json (with-output-to-string
                 (with-current-buffer standard-output
                   (apply 'call-process
                          "roqet" nil t nil "-q" "-r" "json" "-i" "sparql"
                          "-e" sparql roqet-file-args))))
         (plist (let ((json-object-type 'plist)) (json-read-from-string json)))
         (results (plist-get plist :results))
         (bindings (plist-get results :bindings))
         (var (cadr (aref bindings 0))))
    (plist-get var :value)))

(defvar adb-sparql-bin-count
  "PREFIX vamp: <http://purl.org/ontology/vamp/>

SELECT ?binCount WHERE { 
  ?t a vamp:Transform. 
  ?t vamp:output ?output.
  ?output vamp:bin_count ?binCount. }")

(defun adb-feature-bin-count (feature)
  (string-to-number (apply 'adb-roqet-json-single-binding
                           adb-sparql-bin-count
                           (format "features/%s" feature)
                           (adb-list-all-vamp-n3s))))

(defvar adb-sparql-step-size
  "PREFIX vamp: <http://purl.org/ontology/vamp/>

SELECT ?o WHERE { ?s vamp:step_size ?o }")

(defun adb-feature-step-size (feature)
  (string-to-number (adb-roqet-json-single-binding adb-sparql-step-size (format "features/%s" feature))))

(defun adb-current-db-step-size ()
  (let ((feature (adb-db-feature (adb-current-db))))
    (when feature
      (adb-feature-step-size feature))))

(defun adb-select-qid-content (content)
  (interactive
   (list (get-text-property (point) 'adb-content)))
  (unless content
    (error "no adb-content at point"))
  (setf (adb-qid-content (adb-current-qid)) content))

(defun adb-read-number-or-nil (prompt &optional default)
  (let ((n t)
	(default1 (if (consp default) (car default) default)))
    (when default1
      (setq prompt
	    (if (string-match "\\(\\):[ \t]*\\'" prompt)
		(replace-match (format " (default %s)" default1) t t prompt 1)
	      (replace-regexp-in-string "[ \t]*\\'"
					(format " (default %s) " default1)
					prompt t t))))
    (while
	(progn
	  (let ((str (read-from-minibuffer
		      prompt nil nil nil nil
		      (when default
			(if (consp default)
			    (mapcar 'number-to-string (delq nil default))
			  (number-to-string default))))))
	    (condition-case nil
		(setq n (cond
			 ((zerop (length str)) default1)
			 ((stringp str) (string-to-number str))))
	      (error nil)))
	  (unless (or (not n) (numberp n))
	    (message "Please enter a number (or RET to unset).")
	    (sit-for 1)
	    t)))
    n))

(defun adb-lengthspec (n units)
  (if (null n)
      nil
    (cons n units)))

(defun adb-lengthspec-seconds (lengthspec)
  (if (null lengthspec)
      (error "null lengthspec not yet implemented")
    (case (cdr lengthspec)
      ((:seconds) (car lengthspec))
      ((:vectors) (* (car lengthspec) (/ (adb-current-db-step-size) 44100.0))))))

(defun adb-lengthspec-vectors (lengthspec)
  (if (null lengthspec)
      (error "null lengthspec not yet implemented")
    (case (cdr lengthspec)
      ((:seconds) (round (* (car lengthspec) (/ 44100.0 (adb-current-db-step-size)))))
      ((:vectors) (car lengthspec)))))

(defun adb-select-qid-length (length)
  (interactive (list (adb-read-number-or-nil "Query length (seconds): ")))
  (if (or (not length) (<= 0 length))
      (setf (adb-qid-length (adb-current-qid)) (adb-lengthspec length :seconds))
    (message "not suitable for qid length: %s" length)))

(defun adb-select-qid-sequence-length (length)
  (interactive (list (adb-read-number-or-nil "Query length (vectors): ")))
  (if (or (not length) (<= 0 length))
      (setf (adb-qid-length (adb-current-qid)) (adb-lengthspec length :vectors))
    (message "not suitable for qid length: %s" length)))

(defun adb-select-qid-start (start)
  (interactive (list (adb-read-number-or-nil "Query start (seconds): ")))
  (if (or (not start) (<= 0 start))
      (setf (adb-qid-start (adb-current-qid)) start)
    (message "not suitable for qid start: %s" start)))

(defun adb-dbfile (adb)
  (unless (adb-db-feature adb)
    (error "featureless db: %s" adb))
  (if (adb-db-power adb)
      (format "audiodb/db/%s.%s.db" (adb-db-feature adb) (adb-db-power adb))
    (format "audiodb/db/%s.db" (adb-db-feature adb))))

(defun adb-qid-args (qid)
  (append
   (list "-k" (adb-qid-content qid))
   (list "-l" (format "%d" (adb-lengthspec-vectors (adb-qid-length qid))))
   (if (adb-qid-start qid)
       (list "-p" (format "%d" (adb-lengthspec-vectors (cons (adb-qid-start qid) :seconds))))
     (list "-e"))))

(defvar adb-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'adb-query-play-match)
    map))

(define-derived-mode adb-query-mode special-mode "adbq"
  "Major mode for audioDB query results.\\<adb-query-mode-map>")

(defun adb-do-query ()
  (interactive)
  (let* ((db (adb-current-db))
         (dbfile (adb-dbfile db))
         (qid (adb-current-qid))
         (qid-args (adb-qid-args qid))
         (adb-db-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create "*adb query*")
      (adb-query-mode)
      (read-only-mode -1)
      (set (make-local-variable 'adb-buffer) adb-db-buffer)
      (add-hook (make-local-variable 'post-command-hook) 'adb-query-post-command-hook)
      (goto-char (point-max))
      (let ((start (point)))
        (insert (propertize "Database" 'face '(:weight bold)) "\n"
                "  " (adb-db-string db)
                "\n"
                (propertize "Query" 'face '(:weight bold)) "\n"
                "  content: " (adb-qid-content qid) "\n"
                "  length: " (format "%s" (adb-qid-length qid)) "\n"
                (if (adb-qid-start qid)
                    (format "  start: %s" (adb-qid-start qid))
                  "  exhaustive") "\n")
        (let ((exec-path (cons "/home/csr21/goldsmiths/research/src/git/audioDB" exec-path)))
          (apply 'start-process "audioDB" (current-buffer) "audioDB"
                 "-d" dbfile "-Q" "sequence" "-n" "1" qid-args))
        (add-text-properties start (point-max) `(adb-db ,db abd-qid ,qid)))
      (read-only-mode +1))))

(defun adb-query-post-command-hook ()
  (let ((line (thing-at-point 'line))
        (word (thing-at-point 'word)))
    (when (= (length word) 32)
      (with-current-buffer adb-buffer
        (display-buffer (current-buffer))
        (with-selected-window (get-buffer-window (current-buffer))
          (adb-go-to-content word)
          (adb-update-metadata-window word))))))

(defun adb-query-play-match ()
  (interactive)
  (let ((line (thing-at-point 'line))
        (word (thing-at-point 'word)))
    (when (= (length word) 32)
      (let ((qid (with-current-buffer adb-buffer (adb-current-qid)))
            (split-line (split-string line " ")))
        (if (adb-player-process)
            (message "currently playing")
          (start-process "adb player" (adb-player-buffer) "play" "-M" "-q"
                         (format "|sox %s -p trim %d %d channels 1"
                                 (adb-24ify (adb-qid-content qid) "content" "content")
                                 (string-to-number (elt split-line 2))
                                 (adb-lengthspec-seconds (adb-qid-length qid)))
                         (format "|sox %s -p trim %d %d channels 1"
                                 (adb-24ify word "content" "content")
                                 (string-to-number (elt split-line 3))
                                 (adb-lengthspec-seconds (adb-qid-length qid)))))))))

(defun adb-go-to-content (content)
  (interactive (list (completing-read "Content: " adb-all-content)))
  (let (newpos)
    (save-excursion
      (goto-char (point-min))
      (let ((prop (get-text-property (point) 'adb-content)))
        (if (string= content prop)
            (setq newpos (point))
          (while (and (< (point) (point-max))
                      (not (string= content prop)))
            (forward-line)
            (when (string= content (get-text-property (point) 'adb-content))
              (setq newpos (point)))))))
    (if newpos
        (goto-char newpos)
      (error "content %s not found" content))))

(provide 'adb)
