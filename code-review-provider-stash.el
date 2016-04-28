(require 'code-review-util)
(require 'json)

(defvar code-review-provider/stash-reviewers nil "A list of reviewers for a Stash PR")

(defun code-review-provider/stash-build-reviewers-list ()
  "Buidls a JSON array of reviewers that should be attached to a code review."
  (let (
	(reviewers (make-vector (length code-reviewer-provider/stash-reviewers) nil))
	(index 0))
    (dolist (reviewer code-reviewer-provider/stash-reviewers)
      (setq namehash (make-hash-table :test 'equal))
      (puthash "user" (make-hash-table :test 'equal) namehash)
      (puthash "name" reviewer (gethash "user" namehash))
      (aset reviewers index namehash)
      (setq index (+ 1 index)))
    (json-encode reviewers)))

(defun code-review-provider/stash-get-url (git-url &optional pr-id)
  "Returns the Stash URL needed."
  (let ((gitparts (code-review-util/parse-git-url git-url)))
    (code-review-util/clean-url (format "%s/rest/api/1.0/projects/%s/repos/%s/pull-requests/%s" (code-review-util/clean-url code-review-url) (upcase (nth 1 gitparts)) (nth 2 gitparts) pr-id))))

(defun code-review-provider/get-request-type ()
  "We like JSON.  JSON is nice.  JSON is friendly.  JSON plays better with POST."
  (format "POST"))

(defun code-review-provider/get-parser ()
  "Again, JSON!"
  (format "json-read"))

(defun code-review-provider/request-data (from to title description)
  (let ((gitparts (code-review-util/parse-git-url code-review-url 
	(jsonRequest (format "{
    \"title\": \"<title>\",
    \"description\": \"<description>\",
    \"state\": \"OPEN\",
    \"open\": true,
    \"closed\": false,
    \"fromRef\": {
        \"id\": \"refs/heads/<source>\",
        \"repository\": {
            \"slug\": \"<repository>\",
            \"name\": null,
            \"project\": {
                \"key\": \"<project>\"
            }
        }
    },
    \"toRef\": {
        \"id\": \"refs/heads/<destination>\",
        \"repository\": {
            \"slug\": \"<repository>\",
            \"name\": null,
            \"project\": {
                \"key\": \"<project>\"
            }
        }
    },
    \"locked\": false,
    \"reviewers\": <reviewers>
}")))
    (setq jsonRequest (replace-regexp-in-string "<title>" title jsonRequest))
    (setq jsonRequest (replace-regexp-in-string "<source>" from jsonRequest))
    (setq jsonRequest (replace-regexp-in-string "<destination>" to jsonRequest))
    (setq jsonRequest (replace-regexp-in-string "<project>" fff jsonRequest))))))

(defun code-review-provider/stash-get-request-headers (request-body)
  "Request headers that need to be sent.  This is the bare minimum"
  `(("Content-Type" . "application/json")
    ("Content-Length" . ,(length request-body))
    ("Authorization" . ,(format "Basic %s" (base64-encode-string (format "%s:%s" code-review-username code-review-password) t)))))

(defun code-review-provider/stash-process-success (data git-url)
  (message "PR link: %s" (code-review-provider/stash-get-url git-url (assoc-default 'id data))))

(defun code-review-provider/stash-process-error (data)
  (user-error "Something happened.  Error handling doesn't exist yet though."))

(provide 'code-review-provider-stash)
