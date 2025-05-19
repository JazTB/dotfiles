(in-package :stumpwm)

(defun def-keys (&rest stuff)
  (let ((keymap (car stuff)))
    (dolist (binding (cadr stuff))
      (define-key keymap (kbd (car binding)) (cadr binding)))))

(defun def-map (keybind upper-map)
  (let ((m (make-sparse-keymap)))
    (define-key upper-map (kbd keybind) m)
    m))

(set-prefix-key (kbd "s-ESC"))

(defvar *win-map* (def-map "s-w" *top-map*))
;(defvar *win-map* *top-map*)
(defvar *exec-map* (def-map "s-e" *top-map*))
(defvar *rsize-map* (def-map "s-r" *top-map*))
(defvar *main-map* (def-map "s-t" *top-map*))

(def-keys *top-map*
  (list
    '("s-C-SPC" "exec playerctl play-pause")
    '("XF86AudioRaiseVolume" "exec amixer sset Master 2%+,2%+")
    '("XF86AudioLowerVolume" "exec amixer sset Master 2%-,2%-")
    '("XF86AudioMute" "exec amixer sset Master toggle")
    '("XF86AudioMicMute" "exec amixer sset Capture toggle")
    '("XF86MonBrightnessUp" "exec xbacklight -inc 5")
    '("XF86MonBrightnessDown" "exec xbacklight -dec 5")))

(def-keys *top-map* ; window management keybinds in top-map for efficiency
  (list
    '("s-x"       "remove-split")
    '("s-s"       "vsplit")
    '("s-S"       "hsplit")
    '("s-Up"      "move-focus up")
    '("s-S-Up"    "move-window up")
    '("s-Left"    "move-focus left")
    '("s-S-Left"  "move-window left")
    '("s-Down"    "move-focus down")
    '("s-S-Down"  "move-window down")
    '("s-Right"   "move-focus right")
    '("s-S-Right" "move-window right")))

(def-keys *win-map*
  (list
    '("SPC"     "toggle-float")
    '("ESC"     "abort")
    '("r"       "iresize")
    '("q"       "delete")
    '("f"       "fullscreen")
    '("s-w"     "pull-hidden-next")
    '("t"       "pull-hidden-next")
    '("T"       "pull-hidden-previous")
    '("C-t"     "pull-hidden-other")
    '("x"       "remove-split")
    '("s"       "vsplit")
    '("S"       "hsplit")
    '("="       "toggle-always-on-top")
    '("TAB"     "windowlist")
    '("Up"      "move-focus up")
    '("S-Up"    "move-window up")
    '("Left"    "move-focus left")
    '("S-Left"  "move-window left")
    '("Down"    "move-focus down")
    '("S-Down"  "move-window down")
    '("Right"   "move-focus right")
    '("S-Right" "move-window right")))

(def-keys *exec-map*
  (list
    '("l" "exec firefox")
    '("L" "exec librewolf")
    '("b" "exec surf")
    (list "RET" (format nil "exec ~a -e tmux" *terminal*))
    (list "n" (format nil "exec ~a -e tmux new nvim" *terminal*))
    '("ESC" "abort")
    '(";" "colon")
    '(":" "eval")
    '("d" "exec")))

(def-keys *rsize-map*
  (list
    '("Up" "resize-direction up")
    '("Left" "resize-direction left")
    '("Down" "resize-direction down")
    '("Right" "resize-direction right")
    '("k" "resize-direction up")
    '("h" "resize-direction left")
    '("j" "resize-direction down")
    '("l" "resize-direction right")))

(def-keys *main-map*
  (list
    '("r" "restart-hard")
    '("ESC" "abort")
    '("q" "quit-confirm")
    '(";" "colon")
    '(":" "eval")
    '("d" "exec")))

(dolist (workspace *workspaces*)
(let ((name (getf workspace :n))
      (selkeybind (getf workspace :s))
      (movekeybind (getf workspace :m)))
  (define-key *win-map*
    (kbd selkeybind) (format nil "gselect ~a" name))
  (define-key *win-map*
    (kbd movekeybind) (format nil "gmove ~a" name))
  (define-key *top-map*
    (kbd (format nil "s-~a" selkeybind)) (format nil "gselect ~a" name))
  (define-key *top-map*
    (kbd (format nil "s-~a" movekeybind)) (format nil "gmove ~a" name))))
