;; MNML Modeline
;; By: V Dheeraj Shenoy

;; MNML Group

(defgroup mnml-group
  nil
  "Group for the MNML Modeline"
  :group 'mnml-group)

;; FACES

(defface mnml-buffer-name-face
  '((t))
  "Face for Buffer Name Module"
  :group 'mnml-group)

(defface mnml-line-position-face
  '((t))
  "Face for Line Position Module"
  :group 'mnml-group)

(defface mnml-evil-state-face
  '((t))
  "Face for Evil State Module"
  :group 'mnml-group)

(defface mnml-vc-face
  '((t))
  "Face for Buffer Name Module"
  :group 'mnml-group)

(set-face-attribute 'mnml-buffer-name-face nil
     :box nil
     :underline nil
     :overline nil
     :foreground (face-foreground 'highlight)
     :background nil)

(set-face-attribute 'mnml-evil-state-face nil
     :box nil
     :underline nil
     :overline nil
     :foreground (face-foreground 'highlight)
     :background nil)

(set-face-attribute 'mnml-line-position-face nil
     :box nil
     :underline nil
     :overline nil
     :foreground (face-foreground 'highlight)
     :background nil)

(set-face-attribute 'mnml-vc-face nil
     :box nil
     :underline nil
     :overline nil
     :italic nil
     :bold t
     :foreground (face-foreground 'highlight)
     :background nil)


;; HELPER FUNCTIONS

(defun mnml--get-buffer-name()
  "Get the buffer filename of the currently opened buffer"
  (if (buffer-modified-p)
	  (progn (set-face-attribute 'mnml-buffer-name-face nil :italic t :bold t)
			 (format " %s " (buffer-name)))
	(progn (set-face-attribute 'mnml-buffer-name-face nil :italic nil :bold nil)
		   (format " %s " (buffer-name)))))

(defun mnml--get-evil-state()
  "Get the current evil state"
  (if (not mnml-evil-state-minimal)
      (when (mode-line-window-selected-p)
        (setq mnml--evil-state
              (pcase evil-state
                ('insert "INSERT")
                ('normal "NORMAL")
                ('visual "VISUAL"))))
    (when (mode-line-window-selected-p)
        (setq mnml--evil-state
              (pcase evil-state
                ('insert "I")
                ('normal "N")
                ('visual "V")))))
        (format " %s " mnml--evil-state))


(defun mnml--get-vc-branch ()
  "Get the current VC branch name from `vc-mode`."
  (when vc-mode
    (let ((backend (vc-backend buffer-file-name))
          (branch (substring-no-properties vc-mode
                                           (+ (length (symbol-name (vc-backend buffer-file-name))) 2))))
      (format " Git: %s " branch))))

;; VARIABLES

(defcustom mnml-evil-state-minimal nil
  "Evil state Module minimal version")


;; MODULES

;; `EVIL STATE'
(defvar-local mnml-module-evil-state
	'(:eval
	  (propertize (mnml--get-evil-state) 'face 'mnml-evil-state-face))
  "Evil State Module")

;; `BUFFER NAME'
(defvar-local mnml-module-buffer-name
	'(:eval
	  (propertize (mnml--get-buffer-name) 'face 'mnml-buffer-name-face))
  "Buffer name Module")

;; VC INFO
(defvar-local mnml-module-vc-branch
    '(:eval
      (propertize (mnml--get-vc-branch) 'face 'mnml-vc-face))
  "VC Branch Module")

;; `LINE POSITION'
(defvar-local mnml-module-line-position
	'(:eval
	  (propertize " %l:%c " 'face 'mnml-line-position-face))
  "Line Position Module")

;; SET THE RISKY-LOCAL-VARIABLE to T
(dolist (construct '(mnml-module-buffer-name
					 mnml-module-evil-state
					 mnml-module-vc-branch
					 mnml-module-line-position))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
      '(""
        mnml-module-evil-state
        mnml-module-buffer-name
        mode-line-format-right-align
        mnml-module-line-position
        mnml-module-vc-branch))

(defun mnml--update-modeline-theme()
  (let ((active-bg (face-background 'mode-line))          ;; Use highlight background color
        (active-fg (face-foreground 'mode-line))            ;; Use default foreground color
        (inactive-bg (face-background 'mode-line-inactive))          ;; Use default background color for inactive
        (inactive-fg (face-foreground 'mode-line-inactive)))          ;; Use shadow face foreground for inactive
    (set-face-attribute 'mode-line nil
                        :background active-bg
                        :foreground active-fg
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :background inactive-bg
                        :foreground inactive-fg
                        :box nil)))

(defun mnml--mode-line-active-default-face-attribute ()
  (let ((active-bg (face-background 'mode-line))          ;; Use highlight background color
        (active-fg (face-foreground 'mode-line)))            ;; Use default foreground color)
    (set-face-attribute 'mode-line nil
                        :background active-bg
                        :foreground active-fg
                        :box nil)))

(defun mnml--mode-line-inactive-default-face-attribute ()
  (let ((inactive-bg (face-background 'mode-line-inactive))          ;; Use default background color for inactive
        (inactive-fg (face-foreground 'mode-line-inactive)))          ;; Use shadow face foreground for inactive
    (set-face-attribute 'mode-line-inactive nil
                        :background inactive-bg
                        :foreground inactive-fg
                        :box nil)))

(mnml--mode-line-active-default-face-attribute)
(mnml--mode-line-inactive-default-face-attribute)

(add-hook 'minibuffer-setup-hook #'mnml--mode-line-inactive-default-face-attribute)
(add-hook 'minibuffer-exit-hook #'mnml--mode-line-active-default-face-attribute)

(add-hook 'custom-theme-set-variables-hook 'mnml--update-modeline-theme)

(provide 'mnml)
