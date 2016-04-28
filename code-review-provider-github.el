(require 'code-review-util)

(defun code-review-provider/github-get-url (git-url)
   "Returns the Stash URL needed."
  (let ((gitparts (code-review-util/parse-git-url git-url)))
    (format "%s/repos/%s/%s/pulls" (cod-review-util/clean-url code-review-url) (upcase (nth 1 gitparts)) (nth 2 gitparts))))

(defun code-review-provider/github-request-type ()
  (format "POST"))

(defun code-review-provider/github-parser ()
  ('json-read))

(defun code-review-provider/github-get-data (from to title description &optional issue-id)
  (let ((request (format "{ \"head\": \"%s\", \"base\": \"%s\"" from to)))
    (if (issue-id)
	(setq request (format "%s, \"issue\": %S}" request issue-id))
      (setq request (format "%s, \"title\": \"%s\", \"body\": \"%s\"}" request title description)))))

(defun code-review-provider/github-get-request-headers (request-body)
  "Request headers that GitHub requires"
  `(("User-Agent" . code-review-username)
    ("Authorization" . ,(code-review-util/basic-auth))))

(defun code-review-provider/github-process-success (data)
  "Fetch the review link out of the response."
  (message "Issue link: %s" (assoc-default 'url data)))

(defun code-review-provider/github-process-error (data)
  "Error handle failed attempts at creating a pull request."
  (message "Some error happened.  No useful error handling in GitHub provider just yet."))

(provide 'code-review-provider-github)
