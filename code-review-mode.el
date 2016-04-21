(defvar code-review-mode-hook nil "A hook for code-review-mode that doesn't do anything at the moment.")
(defvar code-review-provider "stash" "Who to use? Possible values: github/stash")
(defvar code-review-username nil "Username to log into the provider.")
(defvar code-review-password nil "Password for the username.")
(defvar code-review-url nil "The API URL base (not used for github).")

(defvar code-review-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c p") 'code-review/create-review)
    map)
  "Keymap for code review mode")

(add-to-list 'auto-mode-alist '("*\\.*\\'" . code-review-mode))

(define-minor-mode code-review-mode
  "Minor mode for creating code reviews"
  :lighter " CR"
  :keymap code-review-mode-map)

(require 'subr-x)
(require 'json)
(require 'request)

(defun code-review-mode/exec (cmd)
  (string-trim-right (shell-command-to-string cmd)))

(defun code-review-mode/git-exec (cmd)
  (code-review-mode/exec (format "git %s" cmd)))

(defun code-review-mode/match-regex (regex text)
  (string-match regex text))

;; Parse a Git URL to get the project and repository
(defun code-review-mode/parse-git-url (url)
  ;; In case SSL is used here lets be a little friendly here.  This assumes match 1 is the project and match 2 is the repository
  (if (or (code-review-mode/match-regex "http.://.*/\\([^/].*\\)/\\([^\.].*\\).git" url) (code-review-mode/match-regex ".*:[^/].*/\\([^/].*\\)/\\(.*\\).git" url))
      (list (match-string 1 url) (match-string 2 url))))

(defun code-review-mode/git-current-branch ()
  (code-review-mode/git-exec "rev-parse --abbrev-ref HEAD"))

(defun code-review-mode/clean-url (url)
  "Remove trailing slashes from the URL"
  (replace-regexp-in-string "/+$" "" url))

(defun code-review-mode/create-url (project repo)
  "Create a URL that is pleasant to use given necessary information."
  (if (eq "stash" code-review-provider)
      (format "%s/rest/api/1.0/projects/%s/repos/%s/pull-requests" (code-review-mode/clean-url code-review-url) (upcase project) repo)
    (if (eq "github" code-review-provider)
	(format "https://api.github.com/repos/%s/%s/pulls" project repo)
      )))

(provide 'code-review-mode)
