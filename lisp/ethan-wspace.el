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
;
; FIXME: These are all essentially minor modes, should be able to turn
; them off?

(defvar ethan-wspace-buffer-errors nil
  "Keep track, per-buffer, which whitespace errors a file has.

For possible values, see ethan-wspace-errors.")
(make-variable-buffer-local 'ethan-wspace-buffer-errors)

(defvar ethan-wspace-builtin-errors '(tabs trailing trailing-newline)
  "The list of errors that are recognized by default.")

(defvar ethan-wspace-errors '(tabs trailing trailing-newline)
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
;;   it on, -1 to turn it off.
(defun ethan-wspace-declare-type (name &rest args)
  "Declare a whitespace type.

Options recognized: :find :clean :highlight"
  (setq ethan-wspace-types (cons (ethan-wspace-make-type name args) ethan-wspace-types)))

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
  (assq name ethan-wspace-types))

(defun ethan-wspace-type-get-field (type field)
  (plist-get (cdr type) field))

(defun ethan-wspace-type-find (type-or-name)
  (let* ((type (if (symbolp type-or-name)
                  (ethan-wspace-get-type type-or-name)
                type-or-name))
         (find-func (ethan-wspace-type-get-field type :find)))
    (apply find-func)))

(defun ethan-wspace-type-check (type-or-name)
  "Check for any instance of this wspace type at all.

Returns t or nil."
  (not (null (ethan-wspace-type-find type-or-name))))

(ethan-wspace-declare-type 'tabs :find 'ethan-wspace-type-tabs-find
                           )

(defun ethan-wspace-find-file-hook ()
  (let ((wspace-types (ethan-wspace-normalize-types ethan-wspace-errors)))
    (mapcar '(lambda (type)
               (if ((ethan-wspace-type-check type))
                   ()))
            ))

            wspace-types)


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

(defun clean-whitespace-tentative ()
  "If the whitespace for this buffer started clean, preserve cleanliness.

Used as a save-file-hook."
  (if buffer-whitespace-was-clean
      (clean-whitespace)
    nil))

(defun clean-whitespace-check ()
  "Sets buffer-local variable buffer-whitespace-was-clean if there's nothing weird in the whitespace.

Used as a find-file-hook. (Seems to run after font-lock-mode hooks.)"
  ; FIXME: weird buffers, like if you open a binary file?
  ; FIXME: if interactive, report current status of ws
  (interactive)
  (setq buffer-whitespace-was-clean (buffer-whitespace-clean-p)))

(defun buffer-whitespace-clean-p ()
  (save-excursion
    (goto-char (point-min))
    (if (not (or
         (re-search-forward "\t" nil t)
         (re-search-forward "[ \t]+$" nil t)))
        t
      nil)))

;(add-hook 'find-file-hook 'clean-whitespace-check)
; FIXME: gosh, write-files-functions, before-save-hook, write-contents-functions
;(add-hook 'before-save-hook 'clean-whitespace-tentative)

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

(require 'show-wspace)
(add-hook 'font-lock-mode-hook
          (lambda ()
            (if (and (buffer-file-name)
                     (not dont-show-ws-this-buffer)
                     (not (buffer-whitespace-clean-p)))
                (progn
                  (show-ws-highlight-tabs)
                  (setq show-trailing-whitespace t)))))

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
