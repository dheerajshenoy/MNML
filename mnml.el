;; MNML Modeline
;; By: V Dheeraj Shenoy

;; MNML Group

(defgroup mnml-group
    nil
    "Group for the MNML Modeline"
    :group 'mnml-group)

;; FACES

(defface mnml-buffer-name-face
    '((t :foreground "#000000" :background "#FFFFFF"))
    "Face for Buffer Name Module"
    :group 'mnml-group)

(defface mnml-line-position-face
    '((t :foreground "#FFFFFF" :italic t :bold t))
    "Face for Line Position Module"
    :group 'mnml-group)

(defface mnml-evil-state-face
    '((t :foreground "#FFFFFF" :bold t))
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
	(format " %s " mnml--evil-state)
	)
    )

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

;; `LINE POSITION'
(defvar-local mnml-module-line-position
	'(:eval
	  (propertize " %l:%c " 'face 'mnml-line-position-face))
    "Line Position Module")

;; SET THE RISKY-LOCAL-VARIABLE to T
(dolist (construct '(mnml-module-buffer-name
		     mnml-module-evil-state
		     mnml-module-spacer
		     mnml-module-line-position))
    (put construct 'risky-local-variable t))

;; BEAUTIFUL BOUNDING BOX FOR THE MODE LINE
(set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "white"
                    :box '(:line-width 10 :color "#353644")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 10 :color "#565063")
                    :overline nil
                    :underline nil)

;; SET THE MODE LINE
(setq mode-line-format
      '(""
	mnml-module-evil-state
	mnml-module-buffer-name
	mnml-module-line-position
	))
