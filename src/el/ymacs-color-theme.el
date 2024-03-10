(require 'cl)
(require 'nxml-mode)
(require 'info)

(defvar *ymacs-faces*)                  ; XXX: defparameter for Elisp?

(defvar *ymacs-default-font-size*)

(setq *ymacs-faces*
  '(
    ( ".Ymacs_Overlay.selection div" region )
    ( ".Ymacs_Overlay.isearch div" isearch query-replace )
    ( ".Ymacs_Overlay.isearch-lazy div" lazy-highlight )
    ( ".Ymacs_Overlay.match-paren div" show-paren-match )
    ( ".Ymacs_Modeline" mode-line-inactive mode-line )
    ( ".Ymacs_Frame-active .Ymacs_Modeline" mode-line-active mode-line )
    ( ".minibuffer-prompt" minibuffer-prompt )
    ( ".mode-line-buffer-id" mode-line-buffer-id )

    ( ".type"                  font-lock-type-face                                               )
    ( ".builtin"               font-lock-builtin-face                                            )
    ( ".function-name"         font-lock-function-name-face                                      )
    ( ".variable-name"         font-lock-variable-name-face                                      )
    ( ".constant"              font-lock-constant-face                                           )
    ( ".string"                font-lock-string-face                                             )
    ( ".string-starter"        font-lock-string-face                                             )
    ( ".string-stopper"        font-lock-string-face                                             )
    ( ".regexp"                font-lock-string-face                                             )
    ( ".regexp-starter"        font-lock-string-face                                             )
    ( ".regexp-stopper"        font-lock-string-face                                             )
    ( ".regexp-modifier"       font-lock-string-face                                             )
    ( ".keyword"               font-lock-keyword-face                                            )
    ( ".comment"               font-lock-comment-face                                            )
    ( ".mcomment"              font-lock-comment-face                                            )
    ( ".comment-starter"       font-lock-comment-delimiter-face                                  )
    ( ".mcomment-starter"      font-lock-comment-delimiter-face                                  )
    ( ".mcomment-stopper"      font-lock-comment-delimiter-face                                  )
    ( ".number"                font-lock-constant-face                                           )
    ( ".operator"                                                                                )
    ( ".error"                 font-lock-warning-face                                            )
    ( ".xml-open-tag"          font-lock-function-name-face                                      )
    ( ".xml-close-tag"         font-lock-function-name-face                                      )
    ( ".xml-attribute"         font-lock-variable-name-face                                      )
    ( ".xml-entity-starter"    nxml-entity-ref-delimiter                                         )
    ( ".xml-entity-stopper"    nxml-entity-ref-delimiter                                         )
    ( ".xml-entity"            nxml-entity-ref-name                                              )
    ( ".xml-open-bracket"      nxml-tag-delimiter                                                )
    ( ".xml-close-bracket"     nxml-tag-delimiter                                                )
    ( ".xml-closetag-slash"    nxml-tag-slash                                                    )
    ( ".xml-cdata"             nxml-cdata-section-content font-lock-comment-face                 )
    ( ".xml-cdata-starter"     nxml-cdata-section-delimiter font-lock-comment-delimiter-face     )
    ( ".xml-cdata-stopper"     nxml-cdata-section-delimiter font-lock-comment-delimiter-face     )
    ( ".lisp-keyword"          font-lock-builtin-face font-lock-constant-face                    )
    ( ".markdown-heading1"     info-title-1                                                      )
    ( ".markdown-heading2"     info-title-2                                                      )
    ( ".markdown-heading3"     info-title-3                                                      )
    ( ".markdown-heading4"     info-title-4                                                      )
    ( ".markdown-heading5"     bold-italic                                                       )
    ( ".markdown-heading6"     bold                                                              )
    ( ".markdown-blockquote"   font-lock-comment-face                                            )
    ( ".markdown-blockquote1"  font-lock-comment-face                                            )
    ( ".markdown-blockquote2"  font-lock-comment-face                                            )
    ( ".markdown-blockquote3"  font-lock-comment-face                                            )
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
  (let* ((fgface (find-if (lambda (f) (face-foreground f nil t)) faces))
         (bgface (find-if (lambda (f) (face-background f nil t)) faces))
         (boxface (find-if (lambda (f)
                             (let ((box (face-attribute f :box nil t)))
                               (and box (not (eq box 'unspecified)))))
                           faces))
         (box (and boxface (face-attribute boxface :box nil t)))
         (bold (find-if #'face-bold-p faces))
         (face (first faces))
         (font-size (and face
                         (not no-font)
                         (ymacs-make-font-size
                          (plist-get (font-face-attributes (face-font face)) :height)))))
    (when fgface
      (insert " color: " (ymacs-color-css
                          (if (face-inverse-video-p fgface nil t)
                              (face-background fgface nil t)
                              (face-foreground fgface nil t)))
              ";"))
    (when bgface
      (insert " background-color: " (ymacs-color-css
                                     (if (face-inverse-video-p bgface nil t)
                                         (face-foreground bgface nil t)
                                         (face-background bgface nil t)))
              ";"))
    (destructuring-bind (&key (line-width 1) color) box
      (when color
        (insert (format " border: %dpx solid %s;" line-width (ymacs-color-css color)))))
    (when bold
      (insert " font-weight: bold;"))
    (when font-size
      (insert " font-size: " font-size ";"))))

(defun ymacs-color-theme-print (&optional name)
  (interactive
   (list
    (read-string "Theme name: " (if custom-enabled-themes
                                    (symbol-name (car custom-enabled-themes))
                                    "NONAME"))))
  (let ((*ymacs-default-font-size* (+ 0.0 ; force float :-/
                                      (plist-get (font-face-attributes (face-font 'default)) :height)))
        (prefix (concat ".Ymacs-Theme-" name)))
    (interactive)
    (switch-to-buffer (get-buffer-create "*Ymacs Theme*"))
    (erase-buffer)

    ;; cursor
    (insert prefix " { ")
    (insert "--ymacs-cursor-bg: " (ymacs-color-css (face-background 'cursor nil t)) "; ")
    (insert "--ymacs-cursor-fg: " (ymacs-color-css (face-background 'default nil t)) "; ")
    (insert "}\n")

    ;; main text
    (insert prefix " {")
    (ymacs-face-css '(default) t)
    (insert " }\n")

    ;; window divider (the SplitCont resize bar)
    (let ((main (face-attribute 'window-divider :foreground))
          (first (face-attribute 'window-divider-first-pixel :foreground))
          (last (face-attribute 'window-divider-last-pixel :foreground)))
      (when main
        (insert prefix " .Ymacs_SplitCont .bar { background-color: " (ymacs-color-css main) "; }\n"))

      (insert prefix " .Ymacs_SplitCont.horiz .bar {")
      (when first
        (insert " border-top: 1px solid " (ymacs-color-css first) ";"))
      (when last
        (insert " border-bottom: 1px solid " (ymacs-color-css last) ";"))
      (insert " }\n")

      (insert prefix " .Ymacs_SplitCont.vert .bar {")
      (when first
        (insert " border-left: 1px solid " (ymacs-color-css first) ";"))
      (when last
        (insert " border-right: 1px solid " (ymacs-color-css last) ";"))
      (insert " }\n"))

    (loop for (class . faces) in *ymacs-faces* do
      (insert prefix " " class " {")
      (ymacs-face-css faces)
      (insert " }\n"))))
