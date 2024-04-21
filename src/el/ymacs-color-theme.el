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

    ( ".directive"             font-lock-preprocessor-face                                       )
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
    ( ".isearch-fail"          isearch-fail                                                      )
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
    ( ".heading1"              info-title-1                                                      )
    ( ".heading2"              info-title-2                                                      )
    ( ".heading3"              info-title-3                                                      )
    ( ".heading4"              info-title-4                                                      )
    ( ".markdown-blockquote"   font-lock-comment-face                                            )
    ( ".markdown-blockquote1"  font-lock-comment-face                                            )
    ( ".markdown-blockquote2"  font-lock-comment-face                                            )
    ( ".markdown-blockquote3"  font-lock-comment-face                                            )
    ))

(defun ymacs-color-css (color)
  (let ((rgb (color-values color)))
    (apply 'format "#%02x%02x%02x"
           (mapcar (lambda (x)
                     (* 255 (/ x 65535.0)))
                   rgb))))

(defun ymacs-make-font-size (size)
  (if (= size *ymacs-default-font-size*)
      nil
    (progn
      (format "%.3fem" (/ size *ymacs-default-font-size*)))))

(cl-defun ymacs-face-css (faces &key no-font no-bg)
  (let* ((fgface (find-if (lambda (f) (face-foreground f nil t)) faces))
         (bgface (find-if (lambda (f) (face-background f nil t)) faces))
         (boxface (find-if (lambda (f)
                             (let ((box (face-attribute f :box nil t)))
                               (and box (not (eq box 'unspecified)))))
                           faces))
         (box (and boxface (face-attribute boxface :box nil t)))
         (bold (find-if (lambda (f) (face-bold-p f nil t)) faces))
         (italic (find-if (lambda (f) (face-italic-p f nil t)) faces))
         (face (first faces))
         (font-size (and face
                         (not no-font)
                         (ymacs-make-font-size
                          (plist-get (font-face-attributes (face-font face)) :height)))))
    (with-output-to-string
      (when fgface
        (princ (format " %s: %s;"
                       (if (face-inverse-video-p fgface nil t) "background-color" "color")
                       (ymacs-color-css (face-foreground fgface nil t)))))
      (when (and bgface (not no-bg))
        (princ (format " %s: %s;"
                       (if (face-inverse-video-p bgface nil t) "color" "background-color")
                       (ymacs-color-css (face-background bgface nil t)))))
      (when box
        (destructuring-bind (&key (line-width 1) color style) box
          (princ (format " border: %dpx %s %s;"
                         (abs line-width)
                         (cond
                           ((eq 'released-button style)
                            (unless color
                              (setf color (face-background bgface nil t)))
                            "outset")
                           (t "solid"))
                         (ymacs-color-css color)))))
      (when bold
        (princ " font-weight: bold;"))
      (when italic
        (princ " font-style: italic;"))
      (when font-size
        (princ (format " font-size: %s;" font-size))))))

(defmacro ymacs-style (faces prop)
  `(let ((face (cl-find-if (lambda (face)
                             (let ((p ,prop))
                               (and p (not (eq p 'unspecified)))))
                           ,faces)))
     (when face ,prop)))



(defun ymacs-color-theme-print (&optional name)
  (interactive
   (list
    (read-string "Theme name: " (if custom-enabled-themes
                                    (symbol-name (car custom-enabled-themes))
                                  "NONAME"))))
  (let ((*ymacs-default-font-size* (+ 0.0 ; force float :-/
                                      (plist-get (font-face-attributes (face-font 'default)) :height)))
        (prefix (concat ".Ymacs-Theme-" name)))
    (switch-to-buffer (get-buffer-create "*Ymacs Theme*"))
    (erase-buffer)

    ;; SASS variables
    (insert
     (format "$general_bg: %s;\n" (ymacs-color-css (ymacs-style '(default)
                                                                (face-background face nil t))))
     (format "$general_fg: %s;\n" (ymacs-color-css (ymacs-style '(default)
                                                                (face-foreground face nil t))))
     (format "$modeline_in_bg: %s;\n" (ymacs-color-css (ymacs-style '(mode-line-inactive mode-line)
                                                                    (face-background face nil t))))
     (format "$modeline_in_fg: %s;\n" (ymacs-color-css (ymacs-style '(mode-line-inactive mode-line)
                                                                    (face-foreground face nil t))))
     (format "$modeline_ac_bg: %s;\n" (ymacs-color-css (ymacs-style '(mode-line-active mode-line)
                                                                    (face-background face nil t))))
     (format "$modeline_ac_fg: %s;\n" (ymacs-color-css (ymacs-style '(mode-line-active mode-line)
                                                                    (face-foreground face nil t))))
     (format "$cursor_bg: %s;\n" (ymacs-color-css (face-background 'cursor nil t)))
     (format "$cursor_fg: %s;\n" (ymacs-color-css (face-background 'default nil t)))

     "\n")

    ;; cursor
    (insert prefix " { ")
    (insert "--ymacs-cursor-bg: " (ymacs-color-css (face-background 'cursor nil t)) "; ")
    (insert "--ymacs-cursor-fg: " (ymacs-color-css (face-background 'default nil t)) "; ")
    (insert "}\n")

    ;; main text
    (insert prefix " {")
    (insert (ymacs-face-css '(default) :no-font t))
    (insert " }\n")

    ;; window divider (the SplitCont resize bar)
    (let ((main (face-attribute 'window-divider :foreground))
          (first (face-attribute 'window-divider-first-pixel :foreground))
          (last (face-attribute 'window-divider-last-pixel :foreground)))
      (when main
        (insert prefix " .Ymacs_SplitCont > .bar { background-color: " (ymacs-color-css main) "; }\n"))

      (insert prefix " .Ymacs_SplitCont.horiz > .bar {")
      (when first
        (insert " border-top: 1px solid " (ymacs-color-css first) ";"))
      (when last
        (insert " border-bottom: 1px solid " (ymacs-color-css last) ";"))
      (insert " }\n")

      (insert prefix " .Ymacs_SplitCont.vert > .bar {")
      (when first
        (insert " border-left: 1px solid " (ymacs-color-css first) ";"))
      (when last
        (insert " border-right: 1px solid " (ymacs-color-css last) ";"))
      (insert " }\n"))

    ;; current line and line numbers /highlight
    (let ((cline (ymacs-style '(hl-line) (face-background face nil t))))
      (when cline
        (insert (format "%s .Ymacs_Frame-active .Ymacs-current-line { background-color: %s63 }\n"
                        prefix (ymacs-color-css cline)))))
    (insert (format "%s .Ymacs-frame-content div.line:before {%s }\n"
                    prefix (ymacs-face-css '(line-number) :no-bg t)))
    (insert (format "%s .Ymacs_Frame-active div.line.Ymacs-current-line:before {%s }\n"
                    prefix (ymacs-face-css '(line-number-current-line) :no-bg t)))
    (insert (format "%s .Ymacs-frame-content:before {%s }\n"
                    prefix (ymacs-face-css '(line-number))))

    (insert prefix " {\n")

    (insert
     (format ".Ymacs_Popup {
  --ymacs-popup-bg: %s;
  --ymacs-popup-fg: %s;
  --ymacs-popup-scrollbar-thumb: var(--ymacs-popup-fg);
}\n"
             (ymacs-color-css (ymacs-style '(company-tooltip default) (face-background face nil t)))
             (ymacs-color-css (ymacs-style '(company-tooltip default) (face-foreground face nil t))))

     ".Ymacs_Popup .Ymacs_Menu {\n"
     (format ".Ymacs_Menu_Item:hover, .Ymacs_Menu_Item.selected, .Ymacs_Menu_Item.selected:hover {%s }\n"
             (ymacs-face-css '(company-tooltip-selection)))
     "}\n")

    (cl-loop for (class . faces) in *ymacs-faces* do
          (insert class " {")
          (insert (ymacs-face-css faces))
          (insert " }\n"))
    (insert "}\n")))

(defun ymacs-generate-themes ()
  (interactive)
  (load-library "hl-line")
  (load-library "company")
  (load-library "ef-themes")
  (let ((names '(whiteboard
                 base16-apathy
                 material
                 sanityinc-tomorrow-blue
                 sanityinc-tomorrow-day
                 sanityinc-tomorrow-night
                 sanityinc-tomorrow-eighties
                 sanityinc-tomorrow-bright
                 standard-dark
                 standard-light
                 ef-elea-dark
                 ef-deuteranopia-light
                 ef-cyprus
                 ef-autumn
                 ef-maris-dark
                 ef-maris-light)))
    (loop for theme in names
          for prefix = (concat ".Ymacs-Theme-" (symbol-name theme))
          do (load-theme theme t t)
          (mapc (lambda (theme)
                  (disable-theme theme))
                custom-enabled-themes)
          (enable-theme theme)
          (ymacs-color-theme-print (symbol-name theme))
          (write-file (format "~/ymacs/src/css/themes/_%s.scss" theme))
          (kill-buffer)
          (let ((cust (format "~/ymacs/src/css/themes/%s.scss" theme)))
            (unless (file-exists-p cust)
              (with-current-buffer (get-buffer-create "*Ymacs Theme*")
                (erase-buffer)
                (insert (format "@use \"./_%s.scss\" as theme;\n" theme)
                        prefix " {\n"
                        "    @import \"./_customize.scss\";\n"
                        "}\n")
                (write-file cust)))))))
