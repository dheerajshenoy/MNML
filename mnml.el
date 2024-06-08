;; MNML Modeline
;; By: V Dheeraj Shenoy

;; MNML Group

(defgroup mnml-group
    nil
    "Group for the MNML Modeline"
    :group 'mnml-group)

;; FACES

(defface mnml-buffer-name-face
    '((t
       ;; :foreground "#FFFFFF"
       ;; :background (face-background 'highlight)
       ;; :box '(line-width 3 :color "#5e81ac")
       :underline nil
       :overline nil
       ))
    "Face for Buffer Name Module"
    :group 'mnml-group)

(set-face-attribute 'mnml-buffer-name-face nil
                    :background (face-background 'highlight)
                    :foreground (face-foreground 'default))


(defface mnml-line-position-face
    '((t :foreground "#FFFFFF" :italic nil :bold nil))
    "Face for Line Position Module"
    :group 'mnml-group)

(defface mnml-evil-state-face
    '((t
       :foreground "#FFFFFF"
       :background "#353644"
       :bold nil
       :overline nil
       :underline nil))
    "Face for Evil State Module"
    :group 'mnml-group)

;; HELPER FUNCTIONS

(defun mnml--get-buffer-name()
    (if (buffer-modified-p)
			(progn (set-face-attribute 'mnml-buffer-name-face nil :italic t :bold t)
				   (format " %s " (buffer-name)))
		(progn (set-face-attribute 'mnml-buffer-name-face nil :italic nil :bold nil)
			   (format " %s " (buffer-name)))))

(defun mnml--get-evil-state()
    "Get the current evil state"
    (when (mode-line-window-selected-p)
		(setq mnml--evil-state
			  (pcase evil-state
				  ('insert "INSERT")
				  ('normal "NORMAL")
				  ('visual "VISUAL")
				  ))
		(format " %s " mnml--evil-state)))

(defun mnml--get-vc-branch()
	"Get the current file VC branch name"
	vc-mode)

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
		  (propertize vc-mode 'face 'mnml-buffer-name-face))
    "Buffer name Module")

;; `LINE POSITION'
(defvar-local mnml-module-line-position
		'(:eval
		  (propertize " %l " 'face 'mnml-line-position-face))
    "Line Position Module")

;; SET THE RISKY-LOCAL-VARIABLE to T
(dolist (construct '(mnml-module-buffer-name
					 mnml-module-evil-state
					 mnml-module-vc-btanch
					 mnml-module-line-position))
    (put construct 'risky-local-variable t))

;; SET THE MODE LINE
(setq-default mode-line-format
			  '(""
				mnml-module-evil-state
				mnml-module-buffer-name
				mnml-module-line-position
				mnml-module-vc-branch))

(setq mode-line-format
      '(""
		;; mnml-module-evil-state
		mnml-module-buffer-name
		mnml-module-line-position
		mnml-module-vc-branch))

;; BEAUTIFUL BOUNDING BOX FOR THE MODE LINE
(defun mnml--mode-line-active-default-face-attribute()
    (set-face-attribute 'mode-line nil
                        :background (face-background 'highlight)))


(defun mnml--mode-line-inactive-default-face-attribute()
    (set-face-attribute 'mode-line-inactive nil
                        :background "#FF5000"
                        :foreground "black"
                        ;; :box '(:line-width 10 :color "#565063")
                        :overline nil
                        :underline nil))

(mnml--mode-line-active-default-face-attribute)
(mnml--mode-line-inactive-default-face-attribute)

(add-hook 'minibuffer-setup-hook #'mnml--mode-line-active-default-face-attribute)
(add-hook 'minibuffer-exit-hook #'mnml--mode-line-active-default-face-attribute)

(defun mnml--update-modeline-theme()
    (set-face-attribute 'mnml-buffer-name-face nil
                        :background (face-background 'default)
                        :foreground (face-foreground 'default))
    (set-face-attribute 'mode-line nil
                        :background (face-background 'highlight))


(add-hook 'custom-theme-set-variables-hook 'mnml--update-modeline-theme)
