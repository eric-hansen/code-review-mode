(eval-when-compile (require 'cl-lib))

(require 'subr-x)

(defun code-review-util/exec (cmd)
  "Trim the whitespace from output when calling a command"
  (string-trim-right (shell-command-to-string cmd)))

(defun code-review-util/git-exec (cmd)
  "Run a git command (i.e.: pass in \"commit -a\" to commit all files"
  (code-review-util/exec (format "git %s" cmd)))

(defun code-review-util/match-regex (regex text)
  "I did this a lot so figured I would just make it it's own method, though it really isn't necessary."
  (string-match regex text))

;; Parse a Git URL to get the project and repository
(defun code-review-util/parse-git-url (url)
  ;; In case SSL is used here lets be a little friendly here.  This assumes match 1 is the project and match 2 is the repository
  (if (or (code-review-util/match-regex "http.://.*/\\([^/].*\\)/\\([^\.].*\\).git" url) (code-review-util/match-regex ".*:[^/].*/\\([^/].*\\)/\\(.*\\).git" url))
      (list (match-string 1 url) (match-string 2 url))))

(defun code-review-util/git-current-branch ()
  "Use git to retrieve the current branch; alternate solution is to call git branch and do some cli-fu but this is more universal."
  (code-review-util/git-exec "rev-parse --abbrev-ref HEAD"))

(defun code-review-util/clean-url (url)
  "Remove trailing slashes from the URL"
  (replace-regexp-in-string "/+$" "" url))

(cl-defun code-review-util/call-func (name &optional (suffix "mode") &rest params)
  "Dynamically calls a function for a provider (i.e.: stash-format-request)"
  (let ((function-name (format "code-review-%s/%s-%s" suffix code-review-provider name))
	(apply (intern function-name) params))))

(defun cod-review-util/basic-auth (&optional username password)
  "base64 encodes a proper Basic authentication header value"
  (unless (and username password)
    (setq username code-review-username)
    (setq password code-review-password))

  (format "Basic %s" (base64-encode-string (format "%s:%s" code-review-username code-review-password) t)))

(provide 'code-review-util)
