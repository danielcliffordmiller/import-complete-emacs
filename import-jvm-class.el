;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(require 'json)
(require 'magit)
(require 'popup)
(require 's)

(setq import-tag-file-name "java-classes-tags.json")

(if (not (boundp 'tags-hash-table))
    (setq tags-hash-table (make-hash-table :test 'equal)))

(defun get-class-package-seq (class-name)
  "reads the current git directory and looks up the dictionary list for the symbol under the point"
  (let* ((project-dir
          (replace-regexp-in-string
           (expand-file-name "~") "~"
           (magit-toplevel)))
         (hash-result (gethash project-dir tags-hash-table))
         (tags-alist
          (if hash-result
              hash-result
            (let ((read-tags (json-read-file (concat project-dir import-tag-file-name))))
              (puthash project-dir read-tags tags-hash-table)
              read-tags))))
    
    (alist-get (intern class-name) tags-alist)))

(defun select-package (class-name)
  "function command to select package from class symbol"
  (let ((package-names (get-class-package-seq class-name)))
    (cond ((> (length package-names) 1)
           (popup-menu* (concatenate 'list package-names)))
          ((= (length package-names) 1)
           (elt package-names 0))
          (t (progn (message "\"%s\" could not be found in tag file" class-name) nil)))))

(defun import-class ()
  (interactive)
  (let ((class-name (word-at-point)))
    (if (import-exists class-name)
        (message "import already exists for \"%s\"" class-name)
      (let ((selected-package (select-package class-name)))
        (if selected-package (add-import selected-package class-name))))))

(defun import-exists (class-name)
  (string-match
   (concat "^import .*\\." class-name "$")
   (buffer-substring-no-properties 1 (point))))

(defun add-import (&optional package class)
  (let ((package-point
         (string-match "^package" (buffer-substring-no-properties 1 (point))))
        (package-list (split-string package "\\."))
        (import-start)
        (init-marker (make-marker)))
    (set-marker init-marker (point))
    (while (progn
             ;; (message "Searching for %s" (s-join "." package-list))
             (setq import-start (string-match
                                 (concat "^import " (s-join "." package-list))
                                 (buffer-substring-no-properties 1 (point))))
             (if (not import-start)
                 (setq package-list (butlast package-list)))))
    (undo-boundary)
    (if import-start
        (progn
          (goto-char import-start)
          (insert (concat "\nimport " (s-join "." (list package class))))
          (backward-paragraph)
          (let ((start-of-paragraph (point)))
            (forward-paragraph)
            (sort-lines nil start-of-paragraph (point))))
      (progn
        (goto-char package-point)
        (next-line)
        (newline)
        (insert (concat "import " (s-join "." (list package class)) "\n"))))
    (goto-char init-marker)
    (set-marker init-marker nil)))
