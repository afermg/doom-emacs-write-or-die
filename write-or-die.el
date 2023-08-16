;;; write-or-die.el --- deletes word from buffer if you stop typing for to long

;; Version: 0.1a

;;; Commentary:
;;
;; An emacs version of Dr. Wicked's "Write-or-die" webapp, by Duncan Mortimer <dmorti@gmail.com>
;;
;; As soon as it is write-or-die-go is invoked, the time spent writing and the number of
;; words written are tracked.
;;
;; However, if you stop writing for some period of time, the buffer
;; changes colour to give you some warning, and then words are deleted
;; from the end of the document at a rate of 1 per second.  Beginning
;; writing again stops this.
;;
;; TODO:
;;
;; - Need to work out what variables need to be made buffer-local etc.
;;   They don't behave quite as I expected them to...
;; - Reinstate write-or-die-timer each second, rather than use the repeat feature.
;;   (i.e. so that bad things don't happen if emacs happens to freeze for a while...)
;; - Get it to work smoothely with colour themes; i.e. instead of changing to a white background by
;;   default, need to change to the initial background colour. (17/12/2008)
;;
;; Please let me know if this is useful to you, or if you have any suggestions!

;;; History:
;;
;; - 2009-12-09 incorporated Tom Breton's suggested change to word-count (see wiki page)

;;; Code:

(require 'timer)

(define-minor-mode write-or-die-mode
  "A mode to keep you motivated to continue writing.  Based on
  'write-or-die' by Dr. Wicked (http://lab.drwicked.com/writeordie.html)."
  :lighter write-or-die-mode-text
  :group write-or-die
  (if write-or-die-mode
      (add-hook 'post-command-hook 'write-or-die-post-command-hook)
    (progn
      (write-or-die-stop)
      (remove-hook 'post-command-hook 'write-or-die-post-command-hook))))

;; Customisable variables
(defcustom write-or-die-target-words 500
  "The target number of words to be written after write-or-die-go is invoked."
  :group 'write-or-die)
(defcustom write-or-die-target-time 1200
  "The target time (in seconds) for which you want to write after
  write-or-die-go is invoked."
  :group 'write-or-die)
(defcustom write-or-die-progress-format " [%s of %s, TIMER: %s]"
  "A format string that controls how your current progress is
displayed"
  :group 'write-or-die)
(defcustom write-or-die-warning-period 15
  "Number of seconds of idleness before warning occurs."
  :group 'write-or-die)
(defcustom write-or-die-grace-period 20
  "Number of seconds of idleness before aversive stimulus occurs."
  :group 'write-or-die)

(defcustom write-or-die-grace-states-n 4
  "Number of states (colours) when in warning state."
  :group 'write-or-die)




;; Working variables:
(defvar write-or-die-mode-text " Write!")

                                        ; state = 0: off
                                        ; state = 1: on and going well
                                        ; state = 2: warning
                                        ; state = 3: Zap!!
(defvar write-or-die-state 0)

                                        ; number of words when 'write-or-die-go' most recently called
(defvar write-or-die-num-words-begin 0)

                                        ; timers:
                                        ;  write-or-die-timer:
                                        ;     how long since 'w-o-d-go' called, with w-o-d-state = 0
                                        ;  write-or-die-warning-timer:
                                        ;     sets the state to 2 when w-o-d-warning-period seconds pass while idle.
                                        ;  write-or-die-grace-timer:
                                        ;     sets the state to 3 when w-o-d-grace-period seconds pass while idle.
(defvar write-or-die-timer nil)
(defvar write-or-die-warning-timer nil)
(defvar write-or-die-grace-timer nil)
(defvar write-or-die-time-so-far 0)

(defvar write-or-die-initial-background nil)
(defvar write-or-die-grace-interval
  (/ write-or-die-grace-period
     write-or-die-grace-states-n))

(make-variable-buffer-local 'write-or-die-time-so-far)

(defun write-or-die-update ()
  "This is called every second, and updates word count etc. / calls
warning routine or stimulus routine."
  (message "%i" write-or-die-state)
  (if (> write-or-die-state 0)
      (let
          ((num-words-written
            (- (word-count) write-or-die-num-words-begin))
           (time-to-go
            (- write-or-die-target-time write-or-die-time-so-far)))

        (setq write-or-die-time-so-far
              (+ 1 write-or-die-time-so-far))
        (setq write-or-die-mode-text
              (format write-or-die-progress-format
                      num-words-written
                      write-or-die-target-words
                      time-to-go))))

  (setq write-or-die-mode-text " write!")
  ;; If we're being warned about not concentrating on our writing....
  (if (> write-or-die-state 1)
    (write-or-die-warning) (write-or-die-reset-initial-bg))

  ;; If we're being punished for not writing for too long!
  (if (> write-or-die-state 4)
      (write-or-die-stimulus))

  ;; (message "%s" (current-idle-time))

  (force-mode-line-update))


(defun write-or-die-reset-initial-bg ()
  "Reset to initial background."
  ( unless (eq (cdr (assoc 'background-color (frame-parameters nil))) write-or-die-initial-background)
    ( set-background-color write-or-die-initial-background)))

(defun write-or-die-post-command-hook ()
  "Used to reset the 'stimulus/warning' after you start typing again..."
  (if (> write-or-die-state 1)
      (setq write-or-die-state 1)))

(defun word-count ()
  "Replace original function with updated version that is much shorter."
  (count-words (point-min) (point-max)))

(defun write-or-die-go ()
  "Start incentivised writing!"
  (interactive)
  (if (= 0 write-or-die-state)
      (progn
        (setq write-or-die-initial-background (cdr (assoc 'background-color (frame-parameters nil))))
        ;; (setq write-or-die-warning-backround "dark red")
        (setq write-or-die-state 1)
        (setq write-or-die-num-words-begin (word-count))
        (setq write-or-die-time-so-far 0)
        (setq write-or-die-timer
              (run-with-timer
               0 1  ;; i.e. update once per second, starting NOW!
               'write-or-die-update))
        (setq write-or-die-warning-timer
              (run-with-idle-timer write-or-die-warning-period
                                   1
                                   (lambda () (if (< write-or-die-state 2) (write-or-die-increase-state)))))

        (setq write-or-die-grace-timer
              (run-with-timer 1 write-or-die-grace-interval
                                   (lambda () (if (> write-or-die-state 1) (write-or-die-increase-state))))))))


(defun write-or-die-increase-state ()
  (setq write-or-die-state (min (+ write-or-die-state 1) (+ 1 write-or-die-grace-states-n))))

(defun write-or-die-stop ()
  "Stop incentivised writing!"
  (interactive)
  (setq write-or-die-state 0)
  (cancel-timer write-or-die-timer)
  (cancel-timer write-or-die-warning-timer)
  (cancel-timer write-or-die-grace-timer)
  (write-or-die-reset-initial-bg)
  ;; (load-theme write-or-die-initial-theme)
  (write-or-die-update))


(defun write-or-die-warning ()
  "Change the backround to the current state."
  (message "%s" "Warning runs")
  (let ((current-background-color (cdr (assoc 'background-color (frame-parameters nil))))
        (state-color (concat "firebrick" (number-to-string (- 6 write-or-die-state)))))
      (unless (eq current-background-color state-color)
        (set-background-color state-color))))

(defun backward-kill-whole-word ()
  (interactive)
  (let ((evil-initial-state evil-state))
    (evil-forward-word-end)
    (evil-insert-state)
    ;; (evil-forward-char 1)
    (backward-kill-word 1)
    (evil-change-state evil-initial-state)))



(defun write-or-die-stimulus ()
  "Kill the word from where you are"
  (backward-kill-whole-word))


(provide 'write-or-die)
