;;; ethan-wspace.el
;;; Whitespace customizations.
;;
;; For more information on the design of this package, please see the
;; README that came with it.
;;
;; Editing whitespace. I don't like to forcibly clean[1] whitespace
;; because when doing diffs/checkins, it pollutes changesets. However,
;; I prefer clean whitespace -- often I've deleted whitespace by
;; accident and been unable to "put it back" for purposes of diffing.
;;
;; Therefore, my approach is hybrid -- maintain clean whitespace where
;; possible, and avoid disturbing messy whitespce when I come across
;; it. I add a find-file-hook to track whether the whitespace of a
;; given file was clean to begin with, and add a before-save-hook that
;; uses this information to forcibly clean whitespace only if it was
;; clean at start.
;;
;; Viewing whitespace. Originally I had a font-lock-mode hook that
;; always called show-trailing-whitespace and show-tabs, but this
;; turns out to be annoying for a lot of reasons. Emacs internal
;; buffers like *Completions* and *Shell* would get highlit. So I
;; decided that a more sophisticated approach was called for.
;;
;; 1) If a buffer does not correspond to a file, I almost certainly do
;; not care whether the whitespace is clean. I'm not going to be
;; saving it, after all.
;;
;; 2) If a buffer corresponds to a file that was originally clean, I
;; still don't really care about the whitespace, since my hook (above)
;; would ensure that it was clean going forward.
;;
;; 3) If a buffer corresponds to a file that was not originally clean,
;; I do care about seeing whitespace, because I do not want to edit it
;; by accident.
;;
;; There are a few exceptions -- if I'm looking at a patch, or hacking
;; a Makefile, whitespace is different. More about those later.
;;
;; This file adds hooks to make this stuff happen -- check whether the
;; whitespace is clean when a file is first found, and preserve that
;; cleanliness if so, and highlight that dirtiness if not. It does
;; this by setting show-trailing-whitespace when necessary.
;;
;; NOTE: take out any customizations like this:
;; '(show-trailing-whitespace t)
;; show-trailing-whitespace will be turned on by ethan-wspace.
;;
;; I would re-implement show-trailing-whitespace, but it's handled
;; specially by the emacs core. Probably just as well..
;;
;; FIXME: treat separately the trailing-whitespace, tabs, and
;; end-with-newline thing? But basically all three are the same --
;; check on find-file whether it's fine, and fix it if it is, and
;; highlight if it's not?
;;
;; FIXME: trailing-newlines aren't there at all?
;;
;; FIXME: What's the "best" approach to looping in elisp? I'm using dolist,
;; but that may require cl-macs?

(defvar ethan-wspace-buffer-errors nil
  "Keep track, per-buffer, which whitespace errors a file has.

For possible values, see ethan-wspace-errors.")
(make-variable-buffer-local 'ethan-wspace-buffer-errors)

;; (defvar ethan-wspace-builtin-errors '(tabs trailing trailing-newline)
;;   "The list of errors that are recognized by default.")

(defvar ethan-wspace-errors '(tabs) ;;; '(tabs trailing trailing-newline)
  "The list of errors that a user wants recognized.

FIXME: This variable should be customizable.")

(defface ethan-wspace-face
  '((t (:background "red")))
  "FIXME: compute this from color-theme or something")


;; Need to set up something like yas/global-mode, which turns on/off
;; ethan-wspace globally

;; Each type of whitespace has:
;; - a find function, which when run, returns (begin end) of the next instance
;;   of this whitespace (or nil if none)
;; - a clean function, which is run to eliminate this kind of whitespace.
;; - a highlight function, which is run with one argument -- 1 to turn
;;   it on, -1 to turn it off. This is probably be a minor mode.
;; Probably should be defining minor modes for each of these?
(defmacro ethan-wspace-declare-type (name &rest args)
  "Declare a whitespace type.

Options recognized: :find :clean :highlight :description
:description is used in docstrings and should be plural."

   (let* ((name-str (symbol-name name))
        (clean-mode-name (concat "ethan-wspace-clean-" name-str "-mode"))
        (clean-mode (intern clean-mode-name))
        (highlight-mode-name (concat "ethan-wspace-highlight-" name-str "-mode"))
        (highlight-mode (intern highlight-mode-name))
        (description (or (plist-get args :description)
                         name-str)))
  `(progn
     (setq ethan-wspace-types (cons (ethan-wspace-make-type ',name ',args) ethan-wspace-types))
     (define-minor-mode ,clean-mode
       ,(format "Verify that %s are clean in this buffer.

Works as a save-file hook that cleans %s.
Turning this mode on turns off highlighting %s, and vice versa." description description description)
       :init-value nil :lighter nil :keymap nil
       ;; FIXME: Body goes here
)


     ,(let ((disable-highlight (intern (concat clean-mode-name "-disable-highlight")))
            (disable-clean     (intern (concat highlight-mode-name "-disable-clean"))))
        ;; Defining these as symbols so that multiple loads don't
        ;; add multiple functions
        `(progn
           (defun ,disable-highlight ()
             (when ,clean-mode (,highlight-mode -1)))
           (defun ,disable-clean ()
             (when ,highlight-mode (,clean-mode -1)))
           (add-hook ',(intern (concat clean-mode-name "-hook"))
                     ',disable-highlight)
           (add-hook ',(intern (concat highlight-mode-name "-hook"))
                     ',disable-clean)))
   )
))

;; We store the whitespace types here.
;; Currently each whitespace type is represented as an association list
;; with keys :check, :clean, and :highlight, and values symbols of functions
;; or whatever.
(defvar ethan-wspace-types nil
  "The list of all known whitespace types.")

;; Define the format/structure for the each wspace type. Right now it's
;; (name . (:foo bar :baz quux)).
(defun ethan-wspace-make-type (name args)
  (cons name args))

(defun ethan-wspace-get-type (name)
  (or (assq name ethan-wspace-types)
      (error "Type '%s' does not exist" name)))

(defun ethan-wspace-type-get-field (type field)
  (plist-get (cdr type) field))

(defun ethan-wspace-type-find (type-or-name)
  (let* ((type (if (symbolp type-or-name)
                  (ethan-wspace-get-type type-or-name)
                type-or-name))
         (find-func (ethan-wspace-type-get-field type :find)))
    (apply find-func nil)))

(defun ethan-wspace-type-check (type-or-name)
  "Check for any instance of this wspace type at all.

Returns t or nil."
  (when (ethan-wspace-type-find type-or-name) t))

(defun ethan-wspace-type-clean (type-or-name &optional begin end)
  (let* ((type (if (symbolp type-or-name)
                  (ethan-wspace-get-type type-or-name)
                type-or-name))
         (clean-func (ethan-wspace-type-get-field type :clean))
         (real-begin (or begin (point-min)))
         (real-end (or end (point-max))))
    (apply clean-func (list real-begin real-end))))


(defun ethan-wspace-type-activate (type-name)
  (if (ethan-wspace-type-check type-name)
     (ethan-wspace-type-activate-highlight type-name)
    (ethan-wspace-type-activate-clean type-name)))

(defun ethan-wspace-type-activate-clean (type-name)
  ;;; FIXME: robust against non-stringy type-name?
  ;;; FIXME: refactor with combined activate-highlight?
  (let* ((name-str (if (symbolp type-name) (symbol-name type-name) type-name))
         (clean-mode-name (concat "ethan-wspace-clean-" name-str "-mode"))
         (clean-mode (intern clean-mode-name)))
    (apply clean-mode '(1))))

(defun ethan-wspace-type-clean-mode-active (type-name)
  (let* ((name-str (if (symbolp type-name) (symbol-name type-name) type-name))
         (clean-mode-name (concat "ethan-wspace-clean-" name-str "-mode"))
         (clean-mode (intern clean-mode-name))
         (clean-mode-active (eval clean-mode)))
    clean-mode-active))

(defun ethan-wspace-type-activate-highlight (type-name)
  (let* ((name-str (if (symbolp type-name) (symbol-name type-name) type-name))
         (highlight-mode-name (concat "ethan-wspace-highlight-" name-str "-mode"))
         (highlight-mode (intern highlight-mode-name)))
    (apply highlight-mode '(1))))

;;; TABS
(defvar ethan-wspace-type-tabs-regexp "\t+"
  "The regexp to use when searching for tabs.")

(defun ethan-wspace-type-tabs-find ()
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward ethan-wspace-type-tabs-regexp nil t)))
      (when pos (cons (match-beginning 0) (match-end 0))))))

(defun ethan-wspace-untabify (start end)
  "Like `untabify', but be more respectful of point.
The standard `untabify' does not keep track of point when
deleting tabs and replacing with spaces. This means point can
jump to the beginning of a tab if it was at the end of a tab when
untabify was called.

Since we clean tabs implicitly, we don't want point to jump around.
For details on how we accomplish this, see the source."
  ;; Stock untabify does three things that cause this problem together:
  ;;
  ;; 1. untabify deletes tabs before inserting spaces, which collapses
  ;;    the point-before-tabs and point-after-tabs cases.
  ;;
  ;; 2. save-excursion saves position of point using a marker, but
  ;;    since the insertion type of that marker is nil, inserting
  ;;    spaces before the marker don't move the marker. This is fine
  ;;    when point-before-tabs but wrong when point-after-tabs.  (We
  ;;    use insert-before-markers instead of indent-to-column to
  ;;    handle this.)
  ;;
  ;; 3. Point could be between two tabs, but untabify replaces
  ;;    multiple tabs at once, which is guaranteed to give you the
  ;;    wrong result.
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) end)
      (goto-char start)
      (while (search-forward "\t" nil t)        ; faster than re-search
        (forward-char -1)
        (let ((tab-start (point))
              (column-start (current-column)))
          (forward-char 1)
          (let ((column-end (current-column)))
            ;; Insert text after tabs, but before markers, so that
            ;; point-marker (if after tab) will be advanced.
            (insert-before-markers (make-string (- column-end column-start) ?\ ))
            ;; Delete tabs, so we can handle before-tabs and after-tabs
            ;; separately.
            (delete-region tab-start (1+ tab-start))))))))

(defvar ethan-wspace-type-tabs-keyword
  (list ethan-wspace-type-tabs-regexp 0 (list 'quote 'ethan-wspace-face) t)
  "The font-lock keyword used to highlight tabs.")

(define-minor-mode ethan-wspace-highlight-tabs-mode
  "Minor mode to highlight tabs.

With arg, turn tab-highlighting on if arg is positive, off otherwise.
This supercedes (require 'show-wspace) and show-ws-highlight-tabs."
  :init-value nil :lighter nil :keymap nil
  (if ethan-wspace-highlight-tabs-mode
      (font-lock-add-keywords nil (list ethan-wspace-type-tabs-keyword))
    (font-lock-remove-keywords nil (list ethan-wspace-type-tabs-keyword)))
  (font-lock-fontify-buffer))

(ethan-wspace-declare-type tabs :find ethan-wspace-type-tabs-find
                           :clean ethan-wspace-untabify :highlight ethan-wspace-highlight-tabs-mode
                           )

(define-minor-mode ethan-wspace-mode
  "Minor mode for coping with whitespace.

This just activates each whitespace type in this buffer."
  :init-value nil :lighter nil :keymap nil
  ;(message "Turning on ethan-wspace mode for %s" (buffer-file-name))
  (dolist (type ethan-wspace-errors)
    (ethan-wspace-type-activate type)))

(defun ethan-wspace-is-buffer-appropriate ()
  ;;; FIXME: not OK for diff mode, non-file modes, etc?
  (when (buffer-file-name)
    (ethan-wspace-mode 1)))

(define-global-minor-mode global-ethan-wspace-mode
  ethan-wspace-mode ethan-wspace-is-buffer-appropriate
  :init-value t)

(defun ethan-wspace-clean-before-save-hook ()
  (when ethan-wspace-mode
    (dolist (type ethan-wspace-errors)
      (when (ethan-wspace-type-clean-mode-active type)
          (ethan-wspace-type-clean type)))))

(add-hook 'before-save-hook 'ethan-wspace-clean-before-save-hook)

(defun clean-whitespace ()
  "Clean the whitespace in a buffer -- strip trailing whitespace and untabify."
  ; FIXME: (interactive)?
  ; Clean active region if any, otherwise whole buffer.
; This could use nuke-trailing-whitespace and friends, but that wouldn't
; untabify
  (save-excursion
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max)))
  nil)

  ; FIXME: if interactive, report current status of ws

;;; Showing whitespace
;;
;; We want to show whitespace if it's a real file and the original ws
;; was broken. If it was clean originally, we assume it'll be correct
;; (we'll clean it every save anyhow).
;;
;; We also turn it off for real files with dont-show-ws-this-buffer set to t.
;; Thus for .patch or .diff files, we can use special diff-mode magic.
(defvar dont-show-ws-this-buffer nil)
(make-variable-buffer-local 'dont-show-ws-this-buffer)

;; (require 'show-wspace)
;; (add-hook 'font-lock-mode-hook
;;           (lambda ()
;;             (if (and (buffer-file-name)
;;                      (not dont-show-ws-this-buffer)
;;                      (not (buffer-whitespace-clean-p)))
;;                 (progn
;;                   (show-ws-highlight-tabs)
;;                   (setq show-trailing-whitespace t)))))

(defun dont-show-ws ()
;  (setq show-trailing-whitespace nil)  ; I have this variable customized
  (setq dont-show-ws-this-buffer t))

(add-hook 'diff-mode-hook 'dont-show-ws)

;; Diff mode.
;;
;; Every line in a diff starts with a plus, a minus, or a space -- so
;; empty lines in common with both files will show up as lines with
;; just a space in the diff. As a result, we only highlight trailing
;; whitespace for non-empty lines.
;;
;; The regex matches whitespace that only comes at
;; the end of a line with non-space in it.
;;
; FIXME: This also makes the diff-mode font lock break a little --
; changes text color on lines that match.
;
; FIXME: "bzr diff" format contains tabs?
(add-hook 'diff-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\S-\\([\240\040\t]+\\)$"
                                       (1 'show-ws-trailing-whitespace t))))))


; FIXME: compute this color based on the current color-theme
(setq space-color "#562626")
;(set-face-background 'show-ws-tab space-color)
; FIXME: show-ws-trailing-whitespace should be strongly deprecated!
;(set-face-background 'show-ws-trailing-whitespace space-color)
;(set-face-background 'trailing-whitespace space-color)

(provide 'ethan-wspace)
