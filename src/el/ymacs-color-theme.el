(require 'cl)
(require 'nxml-mode)
(require 'info)

(defvar *ymacs-faces*)                  ; XXX: defparameter for Elisp?

(defvar *ymacs-default-font-size*)

(setq *ymacs-faces*
  '(
    ( "type"                  font-lock-type-face                                               )
    ( "builtin"               font-lock-builtin-face                                            )
    ( "function-name"         font-lock-function-name-face                                      )
    ( "variable-name"         font-lock-variable-name-face                                      )
    ( "constant"              font-lock-constant-face                                           )
    ( "string"                font-lock-string-face                                             )
    ( "string-starter"        font-lock-string-face                                             )
    ( "string-stopper"        font-lock-string-face                                             )
    ( "regexp"                font-lock-string-face                                             )
    ( "regexp-starter"        font-lock-string-face                                             )
    ( "regexp-stopper"        font-lock-string-face                                             )
    ( "regexp-modifier"       font-lock-string-face                                             )
    ( "keyword"               font-lock-keyword-face                                            )
    ( "comment"               font-lock-comment-face                                            )
    ( "mcomment"              font-lock-comment-face                                            )
    ( "comment-starter"       font-lock-comment-delimiter-face                                  )
    ( "mcomment-starter"      font-lock-comment-delimiter-face                                  )
    ( "mcomment-stopper"      font-lock-comment-delimiter-face                                  )
    ( "number"                font-lock-constant-face                                           )
    ( "operator"                                                                                )
    ( "error"                 font-lock-warning-face                                            )
    ( "xml-open-tag"          font-lock-function-name-face                                      )
    ( "xml-close-tag"         font-lock-function-name-face                                      )
    ( "xml-attribute"         font-lock-variable-name-face                                      )
    ( "xml-entity-starter"    nxml-entity-ref-delimiter                                         )
    ( "xml-entity-stopper"    nxml-entity-ref-delimiter                                         )
    ( "xml-entity"            nxml-entity-ref-name                                              )
    ( "xml-open-bracket"      nxml-tag-delimiter                                                )
    ( "xml-close-bracket"     nxml-tag-delimiter                                                )
    ( "xml-closetag-slash"    nxml-tag-slash                                                    )
    ( "xml-cdata"             nxml-cdata-section-content font-lock-comment-face                 )
    ( "xml-cdata-starter"     nxml-cdata-section-delimiter font-lock-comment-delimiter-face     )
    ( "xml-cdata-stopper"     nxml-cdata-section-delimiter font-lock-comment-delimiter-face     )
    ( "lisp-keyword"          font-lock-constant-face                                           )
    ( "markdown-heading1"     Info-title-1-face                                                 )
    ( "markdown-heading2"     Info-title-2-face                                                 )
    ( "markdown-heading3"     Info-title-3-face                                                 )
    ( "markdown-heading4"     Info-title-4-face                                                 )
    ( "markdown-heading5"     bold-italic                                                       )
    ( "markdown-heading6"     bold                                                              )
    ( "markdown-blockquote"   font-lock-comment-face                                            )
    ( "markdown-blockquote1"  font-lock-comment-face                                            )
    ( "markdown-blockquote2"  font-lock-comment-face                                            )
    ( "markdown-blockquote3"  font-lock-comment-face                                            )
    ))

(defun ymacs-color-css (color)
  (let ((rgb (color-values color)))
    (apply 'format "#%02x%02x%02x"
           (mapcar (lambda (x)
                     (* 255 (/ x 65535.0))) rgb))))

(defun ymacs-make-font-size (size)
  (if (= size *ymacs-default-font-size*)
      nil
    (progn
      (format "%.3fem" (/ size *ymacs-default-font-size*)))))

(defun ymacs-face-css (faces &optional no-font)
  (let* ((fg (find-if (lambda (f) (face-foreground f nil t)) faces))
         (bg (find-if (lambda (f) (face-background f nil t)) faces))
         (bold (find-if #'face-bold-p faces))
	 (face (first faces))
	 (font-size (and face
                         (not no-font)
                         (ymacs-make-font-size
                          (plist-get (font-face-attributes (face-font face)) :height)))))
    (when fg
      (insert " color: " (ymacs-color-css (face-foreground fg nil t)) ";"))
    (when bg
      (insert " background-color: " (ymacs-color-css (face-background bg nil t)) ";"))
    (when bold
      (insert " font-weight: bold;"))
    (when font-size
      (insert " font-size: " font-size ";"))))

(defun ymacs-color-theme-print ()
  (interactive)
  (let ((*ymacs-default-font-size* (+ 0.0 ; force float :-/
                                      (plist-get (font-face-attributes (face-font 'default)) :height))))
    (interactive)
    (switch-to-buffer (get-buffer-create "*Ymacs Theme*"))
    (erase-buffer)
    (insert ".Ymacs-Theme-NONAME .Ymacs_Frame {")
    (ymacs-face-css '(default) t)
    (insert " }\n")
    (loop for i in *ymacs-faces*
          for class = (car i)
          for faces = (cdr i)
          do
          (insert ".Ymacs-Theme-NONAME .Ymacs_Frame ." class " {")
          (ymacs-face-css faces t)
          (insert " }\n"))))
