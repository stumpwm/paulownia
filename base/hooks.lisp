(in-package :paulownia)

(export '(*command-mode-start-hook*
          *command-mode-end-hook*
          *urgent-window-hook*
          *new-window-hook*
          *destroy-window-hook*
          *focus-window-hook*
          *place-window-hook*
          *start-hook*
          *quit-hook*
          *internal-loop-hook*
          *event-processing-hook*
          *focus-frame-hook*
          *new-frame-hook*
          *split-frame-hook*
          *message-hook*
          *top-level-error-hook*
          *focus-group-hook*
          *key-press-hook*
          *root-click-hook*
          *new-mode-line-hook*
          *destroy-mode-line-hook*
          *mode-line-click-hook*
          *pre-command-hook*
          *post-command-hook*
          add-hook
          remove-hook
          run-hook
          run-hook-with-args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *command-mode-start-hook* '(command-mode-start-message)
  "A hook called whenever command mode is started")

(defvar *command-mode-end-hook* '(command-mode-end-message)
  "A hook called whenever command mode is ended")

(defvar *urgent-window-hook* '()
  "A hook called whenever a window sets the property indicating that
  it demands the user's attention")

(defvar *map-window-hook* '()
  "A hook called whenever a window is mapped.")

(defvar *unmap-window-hook* '()
  "A hook called whenever a window is withdrawn.")

(defvar *new-window-hook* '()
  "A hook called whenever a window is added to the window list. This
includes a genuinely new window as well as bringing a withdrawn window
back into the window list.")

(defvar *destroy-window-hook* '()
  "A hook called whenever a window is destroyed or withdrawn.")

(defvar *focus-window-hook* '()
  "A hook called when a window is given focus. It is called with 2
arguments: the current window and the last window (could be nil).")

(defvar *place-window-hook* '()
  "A hook called whenever a window is placed by rule. Arguments are
window group and frame")

(defvar *start-hook* '()
  "A hook called when stumpwm starts.")

(defvar *quit-hook* '()
  "A hook called when stumpwm quits.")

(defvar *internal-loop-hook* '()
  "A hook called inside stumpwm's inner loop.")

(defvar *event-processing-hook* '()
  "A hook called inside stumpwm's inner loop, before the default event
  processing takes place. This hook is run inside (with-event-queue ...).")

(defvar *focus-frame-hook* '()
  "A hook called when a frame is given focus. The hook functions are
called with 2 arguments: the current frame and the last frame.")

(defvar *new-frame-hook* '()
  "A hook called when a new frame is created. the hook is called with
the frame as an argument.")

(defvar *split-frame-hook* '()
  "A hook called when a frame is split. the hook is called with
the old frame (window is removed), and two new frames as arguments.")

(defvar *message-hook* '()
  "A hook called whenever stumpwm displays a message. The hook
function is passed any number of arguments. Each argument is a
line of text.")

(defvar *top-level-error-hook* '()
  "Called when a top level error occurs. Note that this hook is
run before the error is dealt with according to
*top-level-error-action*.")

(defvar *focus-group-hook* '()
  "A hook called whenever stumpwm switches groups. It is called with 2 arguments: the current group and the last group.")

(defvar *key-press-hook* '()
  "A hook called whenever a key under *top-map* is pressed.
It is called with 3 argument: the key, the (possibly incomplete) key
sequence it is a part of, and command value bound to the key.")

(defvar *root-click-hook* '()
  "A hook called whenever there is a mouse click on the root
window. Called with 4 arguments, the screen containing the root
window, the button clicked, and the x and y of the pointer.")

(defvar *new-mode-line-hook* '()
  "Called whenever the mode-line is created. It is called with argument,
the mode-line")

(defvar *destroy-mode-line-hook* '()
  "Called whenever the mode-line is destroyed. It is called with argument,
the mode-line")

(defvar *mode-line-click-hook* '()
  "Called whenever the mode-line is clicked. It is called with 4 arguments,
the mode-line, the button clicked, and the x and y of the pointer.")

(defvar *pre-command-hook* '()
  "Called before a command is called. It is called with 1 argument:
the command as a symbol.")

(defvar *post-command-hook* '()
  "Called after a command is called. It is called with 1 argument:
the command as a symbol.")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function definitions 
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it."
  (handler-case
      (with-simple-restart (abort-hooks "Abort running the remaining hooks.")
        (with-restarts-menu
            (dolist (fn hook)
              (with-simple-restart (continue-hooks "Continue running the remaining hooks.")
                (apply fn args)))))
    (t (c) (message "^B^1*Error on hook ^b~S^B!~% ^n~A" hook c) (values nil c))))

(defun run-hook (hook)
  "Call each function in HOOK."
  (run-hook-with-args hook))

(defmacro add-hook (hook fn)
  "Add @var{function} to the hook @var{hook-variable}. For example, to
display a message whenever you switch frames:

@example
\(defun my-rad-fn (to-frame from-frame)
  (stumpwm:message \"Mustard!\"))

\(stumpwm:add-hook stumpwm:*focus-frame-hook* 'my-rad-fn)
@end example"
  `(setf ,hook (adjoin ,fn ,hook)))

(defmacro remove-hook (hook fn)
"Remove the specified function from the hook."
  `(setf ,hook (remove ,fn ,hook)))
