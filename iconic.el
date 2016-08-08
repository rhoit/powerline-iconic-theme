;;; iconic.el
;; Description: Uncluttered powerline-theme
;; Author: rho <rho.rhoit@gmail.com>
;; URL: https://github.com/rhoit/powerline-iconic-theme

;;; Commentary
;; This package provides a a powerline theme

;;; Code

(require 'powerline)
(require 'mode-icons)
;; (require 'window-numbering)

(mode-icons-mode)
(setq powerline-default-separator 'wave)

;; Make a face for the octicons font (must be installed on your system)
;; https://octicons.github.com/
(make-face 'octicons)
(set-face-attribute 'octicons nil
                    :family "octicons")
(setq octicon-mark-github " ")
(setq octicon-rocket "")

;; version control
;; TODO: move to modeline
;; (defun git-status-in-modeline()
;;   (if (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode)))
;;       (git--update-all-state-marks)))
;; (add-hook 'find-file-hook 'git-status-in-modeline t)


(defun powerline-simpler-vc-mode(s)
  (if s
      (replace-regexp-in-string " Git[:-]" "" s)
    s))

;; (:propertize s
;;              face powerline-active1)
;; (set-face-attribute s :foreground '"red")

(defun vc-theme(&optional face pad)
  (concat
   ;; (powerline-raw octicon-mark-github face2)
   ;; (powerline-raw `("-- " firemagit local-map;; face2)
   ;;                  (:propertize
   ;;                   help-echo "magit stuff")) face2)
   (powerline-raw
    (when (and (buffer-file-name (current-buffer)) vc-mode)
      (if window-system
          (format-mode-line '(vc-mode vc-mode))
        (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
          (when backend
            (format " %s %s"
                    (char-to-string #xe0a0)
                    (vc-working-revision (buffer-file-name (current-buffer)) backend))))))
    face pad)))

(defun powerline-iconic-theme-unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "❻")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "0" str) "➓")))

(setq which-func-format
      `(" "
        (:propertize which-func-current local-map
                     (keymap
                      (mode-line keymap
                                 (mouse-3 . end-of-defun)
                                 (mouse-2 . narrow-to-defun)
                                 (mouse-1 . beginning-of-defun)))
                     face which-func
                     mouse-face mode-line-highlight
                     help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
        " "))

(setq column-number-mode nil)
(setq line-number-mode nil)

(defun powerline-iconic-theme ()
  "Setup the powerline-iconic theme"
  (interactive)
  (setq-default
   mode-line-format
   '(" %e"

     mode-line-modified
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*" nil 'l)
                        (when powerline-display-mule-info
                          (powerline-raw mode-line-mule-info nil 'l))
                        (powerline-raw " ")
                        (funcall separator-left nil face2)
                        (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face2 'l)
                        (powerline-raw " " face2)
                        (funcall separator-right face2 face1)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        ;;            (powerline-zigzag-left face1 nil)
                        ;;            (powerline-raw " " nil)
                        ))
             (center (list
                      (when (and (boundp 'which-func-mode) which-func-mode)
                        (powerline-arrow-left face1 face2))
                      (when (and (boundp 'which-func-mode) which-func-mode)
                        (powerline-raw which-func-format face2 'l))
                      (when (and (boundp 'which-func-mode) which-func-mode)
                        (powerline-raw " " face2))
                      (when (and (boundp 'which-func-mode) which-func-mode)
                        (powerline-zigzag-right face2 nil))
                      ))
             (rhs (list (powerline-raw global-mode-string nil 'r)
                        (when (vc-backend buffer-file-name)
                          (funcall separator-left nil face2))
                        (when (vc-backend buffer-file-name)
                          (powerline-raw octicon-mark-github face2))
                        ;; (powerline-simpler-vc-mode (powerline-vc face2 'r))
                        (vc-theme face2 'r)
                        (when (vc-backend buffer-file-name)
                          (funcall separator-right face2 nil))
                        (powerline-raw " " nil)
                        (powerline-zigzag-left nil face1)
                        (powerline-raw "%3c," face1 'r)
                        ;; (string-match "\\`[1-9][0-9]*\\'" mode-line-position)
                        ;; (replace-regexp-in-string "[^a-z]" "&%"
                        ;;                           (powerline-raw mode-line-position face1 'r))
                        (powerline-raw mode-line-position face1 'r)
                        ;; (if (string-match "\\`[0-9]\\'" (powerline-raw mode-line-position))
                        ;;     ;; # FIX the movement
                        ;;     (powerline-raw "% " face1))
                        (powerline-zigzag-right face1 nil)
                        (powerline-raw "  " nil)
                        )))
        (concat (powerline-render lhs)
                (powerline-render center)
                (powerline-fill nil (powerline-width rhs))
                (powerline-render rhs))
        )))))

(provide 'powerline-iconic-theme)
