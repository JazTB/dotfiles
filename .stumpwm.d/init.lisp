(ql:quickload "asdf")
(in-package :stumpwm)

(mapcar
  #'add-to-load-path
  (build-load-path
    (merge-pathnames *data-dir* "/modules/stumpwm-contrib")))
(mapcar
  #'add-to-load-path
  (build-load-path
    (merge-pathnames *data-dir* "/modules")))

(defun load-relative (fname)
  "Load a script"
  (load (format nil "~a"
    (merge-pathnames fname
      (make-pathname :directory
        (pathname-directory *load-truename*))))))

(set-float-focus-color "#e18bbe")
(set-float-unfocus-color "#2d1523")
(set-focus-color "#40639f")
(set-unfocus-color "#934674")
(set-win-bg-color "#181818")

(defvar *workspaces*
  ;; name, select, move-to
  '((:n "1" :s "1" :m "!")
    (:n "2" :s "2" :m "@")
    (:n "3" :s "3" :m "#")
    (:n "4" :s "4" :m "$")
    (:n "5" :s "5" :m "%")
    (:n "6" :s "6" :m "^")
    (:n "7" :s "7" :m "&")
    (:n "8" :s "8" :m "*")
    (:n "9" :s "9" :m "(")
    (:n "0" :s "0" :m ")")))

;(gnewbg-dynamic (getf (nth 0 *workspaces*) :n))
;(gselect (getf (nth 0 *workspaces*) :n))
;(gkill-other)
;(dolist (workspace (cdr *workspaces*))
;  (gnewbg-dynamic (getf workspace :n)))
;; or:
(dolist (workspace (cdr *workspaces*))
  (gnewbg (getf workspace :n)))
(grename (getf (nth 0 *workspaces*) :n))

(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 10
      *maxsize-border-width* 0
      *normal-border-width* 2
      *transient-border-width* 4
      *float-window-border* 4
      *float-window-title-height* 4
      *mouse-focus-policy* :click
      *default-package* :stumpwm)

(defcommand toggle-float () ()
  "Toggle floating for a window"
  (if (typep (current-window) 'STUMPWM::FLOAT-WINDOW)
    (unfloat-this)
    (float-this))
  (toggle-always-on-top))

;(defcommand set-terminal (newterm) ((:string "What terminal?> "))
;  (setq *terminal* newterm)
;  (run-commands "restart-soft"))

(defvar *terminal* "alacritty")

(load-relative "commands.lisp")
(load-relative "key.lisp")

(asdf:run-shell-command "$HOME/.stumpwm.d/autostart.sh")
