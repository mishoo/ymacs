;; -*- lexical-binding: t; -*-

(require 'simple-httpd)
(require 'cl-lib)

(defvar ymacs-eldir (file-name-directory (or load-file-name buffer-file-name)))
(defvar ymacs-srcdir (expand-file-name "../.." ymacs-eldir))

(load (expand-file-name "ymacs-color-theme.el" ymacs-eldir))

(defvar ymacs-httpd-port 8977)

(setf httpd-port ymacs-httpd-port)
(setf httpd-root ymacs-srcdir)
(httpd-start)

(defservlet ymacs-current-theme.css "text/css; charset=utf-8" ()
  (ymacs-color-theme-print "_current" t)
  ;; should cleanup SCSS variables.. are they even used?
  (goto-char (point-min))
  (flush-lines "^[[:space:]]*\\$"))

(defservlet ymacs-buffer-list application/json ()
  (insert (json-encode (mapcar #'buffer-name
                               (cl-remove-if-not #'buffer-file-name
                                                 (buffer-list))))))

(defservlet ymacs-buffer-get application/json (path)
  (let ((buffer-name (replace-regexp-in-string "^/.*?/" "" path)))
    (insert
     (with-current-buffer buffer-name
       (save-excursion
        (save-restriction
         (widen)
         (json-encode `((mode . ,major-mode)
                        (point . ,(point))
                        (code . ,(buffer-substring-no-properties (point-min) (point-max)))))))))))

(defservlet ymacs-buffer-save application/json (path _ headers)
  (let* ((buffer-name (replace-regexp-in-string "^/.*?/" "" path))
         (json (cadr (assoc "Content" headers #'string=)))
         (args (json-read-from-string (decode-coding-string json 'utf-8)))
         (code (cdr (assoc 'code args)))
         (point (cdr (assoc 'point args))))
    (with-current-buffer buffer-name
      (erase-buffer)
      (insert code)
      (when point
        (goto-char point))
      (save-buffer))
    (insert (json-encode `((ok . ,buffer-name))))))

(defun httpd/ymacs-magit-status (proc path &rest _)
  (let ((buffer-name (replace-regexp-in-string "^/.*?/" "" path)))
    (switch-to-buffer buffer-name)
    (select-frame-set-input-focus
     (selected-frame))
    (magit-status-setup-buffer)
    (with-httpd-buffer proc "application/json"
      (insert (json-encode `((ok . ,buffer-name)))))))
