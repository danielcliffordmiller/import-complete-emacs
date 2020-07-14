;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(require 'json)
(require 'magit)
(require 'popup)

(setq import-tag-file-name "java-classes-tags.json")

(if (not (boundp 'tags-hash-table))
    (setq tags-hash-table (make-hash-table :test 'equal)))

(defun get-class-package-seq (class-name)
  "reads the current git directory and looks up the dictionary list for the symbol under the point"
  (let* ((project-dir
          (replace-regexp-in-string
           (expand-file-name "~") "~"
           (file-name-directory (directory-file-name (magit-git-dir)))))
         (hash-result (gethash project-dir tags-hash-table))
         (tags-alist
          (if hash-result
              hash-result
            (let ((read-tags (json-read-file (concat project-dir import-tag-file-name))))
              (puthash project-dir read-tags tags-hash-table)
              read-tags))))
    
    (alist-get class-name tags-alist)))

(defun select-package (class-symbol)
  "function command to select package from class symbol"
  (let ((package-names (get-class-package-seq class-symbol)))
    (cond ((import-exists class-symbol)
        )
          ((> (length package-names) 1)
           (popup-menu* (concatenate 'list package-names)))
          ((= (length package-names) 1)
           (elt package-names 0))
          (t (progn (message "\"%s\" could not be found in tag file" class-symbol) nil)))))

(defun import-class ()
  (interactive)
  (let ((class-symbol (symbol-at-point)))
    (if (import-exists class-symbol)
        (message "import already exists for \"%s\"" class-symbol)
      (let ((selected-package (select-package class-symbol)))
        (if selected-package (add-import selected-package (symbol-name class-symbol)))))))

(defun import-exists (class-symbol)
  (string-match
   (concat "^import .*" (symbol-name class-symbol) "$")
   (buffer-substring-no-properties 1 (point))))

(defmacro join-to-string (list-of-strings &optional character)
  "join a list of strings together with the optinal joining character"
  (if (not character) (setq character ""))
  (let ((strings (gensym)))
    `(let ((,strings ,list-of-strings))
       (if ,strings
           (reduce
            (lambda (a b) (concat a ,character b))
            (seq-subseq ,strings 1)
            :initial-value (first ,strings))
         ""))))

(defun add-import (&optional package class)
  (let ((package-point
         (string-match "^package" (buffer-substring-no-properties 1 (point))))
        (package-list (split-string package "\\."))
        (import-start)
        (init-marker (make-marker)))
    (set-marker init-marker (point))
    (while (progn
             (message "Searching for %s" (join-to-string package-list "."))
             (setq import-start (string-match
                                 (concat "^import " (join-to-string package-list "."))
                                 (buffer-substring-no-properties 1 (point))))
             (if (not import-start)
                 (setq package-list (butlast package-list)))))
    (undo-boundary)
    (if import-start
        (progn
          (goto-char import-start)
          (insert (concat "\nimport " (join-to-string (list package class) ".")))
          (backward-paragraph)
          (let ((start-of-paragraph (point)))
            (forward-paragraph)
            (sort-lines nil start-of-paragraph (point))))
      (progn
        (goto-char package-point)
        (next-line)
        (newline)
        (insert (concat "import " (join-to-string (list package class) ".") "\n"))))
    (goto-char init-marker)
    (set-marker init-marker nil)))
