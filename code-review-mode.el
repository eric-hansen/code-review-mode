(defvar code-review-mode-hook nil "A hook for code-review-mode that doesn't do anything at the moment.")
(defvar code-review-provider "stash" "Who to use? Possible values: github/stash")
(defvar code-review-username nil "Username to log into the provider.")
(defvar code-review-password nil "Password for the username.")
(defvar code-review-url nil "The API URL base (not used for github).")
(defvar code-review-debug f "Do we want to provide debug output for stuff (when possible)?")

(defvar code-review-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c p") 'code-review-mode/create-review)
    (define-key map (kbd "C-c l") 'code-review-mode/load-providers)
    map)
  "Keymap for code review mode")

(add-to-list 'auto-mode-alist '("*\\.*\\'" . code-review-mode))

(define-minor-mode code-review-mode
  "Minor mode for creating code reviews"
  :lighter " CR"
  :keymap code-review-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Required libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'subr-x)
(require 'json)
(require 'request)
(require 'code-review-util)

; We need to also load in any providers.
(code-review-mode/load-providers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun code-review-mode/load-providers ()
  "(Re)loads the providers needed (should reside in the same directory as the minor-mode.

They must be prefixed by code-review-provider- however.  So if you write a provider for BitBucket, it should be
code-review-provider-bitbucket.el.  This really helps eliminate any name mangling or conflicts."
  (interactive)
  (let ((load (mapcar #'car load-history)))
    (dolist (file (directory-files (file-name-directory load-file-name) t "code-reivew-provider-\.+\\.elc?$"))
      (unless (catch 'foo
		(dolist (done loaded)
		  (when (equal file done) (throw 'foo t)))
		nil)
	(load (file-name-sans-extension file))
	(push file loaded)))))

(defun code-review-mode/create-url (project repo)
  "Create a URL that is pleasant to use given necessary information."
  (if (eq "stash" code-review-provider)
      (format "%s/rest/api/1.0/projects/%s/repos/%s/pull-requests" (code-review-mode/clean-url code-review-url) (upcase project) repo)
    (if (eq "github" code-review-provider)
	(format "https://api.github.com/repos/%s/%s/pulls" project repo)
      )))

(defun code-review-mode/provider-function (func-name)
  "Simply creates a string of the provider function to call."
  (format "%s-%s" code-review-provider func-name))

(defun code-review-mode/create-review ()
  "Creates a request to the provider for a code review and then returns the link (if possible)."
  (interactive)
  (request
   (code-review-util/call-func (code-review-mode/provider-function "get-url") "provider")
   :type (code-review-util/call-func (code-review-mode/provider-function "get-request-type") "provider")
   :parser (code-review-util/call-func (code-review-mode/provider-function "get-parser") "provider")
   :data (code-review-util/call-func (code-review-mode/provider-function "request-data") "provider")
   :headers `(code-reivew-util/call-func (code-review-mode/provider-function "get-request-headers") "provider")
   :success (function* (lambda &key data &allow-other-keys)
		       (code-review-util/call-func (code-review-mode/provider-function "process-success") "provider"))
   :error (function* (lambda &key error-thrown &allow-other-keys &rest _)
		     (code-review-util/call-func (code-review-mode/provider-function "process-error") "provider"))))

(provide 'code-review-mode)
