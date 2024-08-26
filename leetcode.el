;;; leetcode.el --- An leetcode client  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019  Wang Kai
;;
;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "29.1") (dash "2.16.0") (graphql "0.1.1") (spinner "1.7.3") (log4e "0.3.3"))
;; Version: 0.1.27
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; leetcode.el is an unofficial LeetCode client.
;;
;; Now it implements several API:
;; - Check problems list
;; - Try testcase
;; - Submit code
;;
;; Since most HTTP requests works asynchronously, it won't block Emacs.
;;
;;; Code:
(eval-when-compile
  (require 'let-alist)
  (require 'dash))

(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 'dash)
(require 'graphql)                      ; Some requests of LeetCode use GraphQL
(require 'spinner)
(require 'log4e)

(defvar url-http-end-of-headers)

(log4e:deflogger "leetcode" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(setq log4e--log-buffer-leetcode "*leetcode-log*")

;;;###autoload
(defun leetcode-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (leetcode--log-debugging-p)
      (progn
        (leetcode--log-set-level 'info)
        (leetcode--log-disable-debugging)
        (message "leetcode disable debug"))
    (progn
      (leetcode--log-set-level 'debug)
      (leetcode--log-enable-debugging)
      (message "leetcode enable debug"))))

(defun leetcode--install-my-cookie ()
  "Install leetcode dependencies."
  (let ((async-shell-command-display-buffer t))
    (async-shell-command
     "pip3 install my_cookies"
     (get-buffer-create "*leetcode-install*"))))

(defun leetcode--check-deps ()
  "Check if all dependencies installed."
  (if (executable-find "my_cookies") t
    (leetcode--install-my-cookie)
    nil))

(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

(defcustom leetcode-prefer-tag-display t
  "Whether to display tags by default in the *leetcode* buffer."
  :type 'boolean)

(defcustom leetcode-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift."
  :type 'string)

(defcustom leetcode-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql."
  :type 'string)

(defcustom leetcode-directory "~/leetcode"
  "Directory to save solutions."
  :type 'string)

(defcustom leetcode-save-solutions nil
  "If it's t, save leetcode solutions to `leetcode-directory'."
  :type 'boolean)

(defcustom leetcode-focus t
  "When execute `leetcode', always delete other windows."
  :type 'boolean)

(defcustom leetcode-before-submit-hook nil
  "Hook run prior to submitting code with `leetcode-submit' or `leetcode-try'.
Hooks are called from code buffer being submitted.

For example, to reset window configuration when submitting,
`leetcode-restore-layout' can be added to this hook."
  :type 'hook)

(defcustom leetcode-after-set-language-hook nil
  "Hook run after setting language before creating a buffer for coding.
Hooks are run with the language as an argument.

This hook can be used, for example, to set `leetcode-directory' depending on the
language."
  :type 'hook)

(defcustom leetcode-filename-function 'leetcode--filename-default
  "Function to return filename for to use for a problem.
This function is called with the problem id, title slug, and a filename suffix
determined by `leetcode-prefer-language'. It should return a filename to use for
the problem.."
  :type 'function)

(defcustom leetcode-buffer-code-function 'leetcode--buffer-content-default
  "Function to return code from buffer for submission to leetcode api.
This function is called in the code buffer and should return a string."
  :type 'function)


(defcustom leetcode-display-configuration '((code   . 0.5)
                                            (detail . 0.4)
                                            (test   . 0.15)
                                            (result))
  "Configuration for leetcode detail, tests, and results buffers.
This is an alist of \\='(buffer . size) where BUFFER is one of \\='code,
\\='detail, \\='test, or \\='result. Size is ignored in the last entry, since
the last buffer uses the remaining vertical space.

The default layout looks roughly like:
+---------------+----------------+
|               |   1. Detail    |
|               |                |
|  0. Code      +----------------+
|               |   2. Test      |
|               +----------------+
|               |   3. Result    |
+---------------+----------------+

The order of entries in the alist determines the order of the buffers,
corresponding to the numbers in the above layout. The size of the first entry
determines the width for buffer 0. The remaining sizes are heights. Size can be
either a whole number for lines/columns, or a decimal representing a fraction of
the lines/columns in the frame."
  :type 'alist)

(defcustom leetcode-display-buffer-alist
  '((display-buffer-reuse-window)
    (window-height)
    (reusable-frames . visible)
    ;; (body-function . (lambda (w) (setq other-window-scroll-buffer (window-buffer w))))
    )
  "Display buffer action used when displaying results."
  :type 'alist)


(cl-defstruct leetcode-user
  "A LeetCode User.
The object with following attributes:
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number"
  username solved easy medium hard)

(cl-defstruct leetcode-problem
  "A single LeetCode problem.
:status     String
:id         Number
:backend-id Number
:title      String
:title-slug String
:acceptance String
:difficulty Number {1,2,3}
:paid-only  Boolean {t|nil}
:tags       List"
  status id backend-id title title-slug acceptance
  difficulty paid-only tags)

(cl-defstruct leetcode-problems
  "All LeetCode problems, the problems can filtered by tag.
:num      Number
:tag      String
:problems List[leetcode--problems]"
  num tag problems)

(defvar leetcode--user (make-leetcode-user)
  "A User object.")

(defvar leetcode--problems (make-leetcode-problems)
  "Problems object with a list of `leetcode-problem'.")

(defvar leetcode--all-tags nil
  "All problems tags.")

(defvar leetcode--display-tags leetcode-prefer-tag-display
  "(Internal) Whether tags are displayed the *leetcode* buffer.")

(defvar leetcode--display-paid nil
  "(Internal) Whether paid problems are displayed the *leetcode* buffer.")

(defvar leetcode--lang leetcode-prefer-language
  "LeetCode programming language or sql for current problem internally.
Default is programming language.")

(defconst leetcode--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("dart" . ".dart") ("elixir" . ".ex") ("erlang" . ".erl")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("kotlin" . ".kt") ("php" . ".php") ("python" . ".py") ("python3" . ".py")
    ("racket" . ".rkt") ("ruby" . ".rb") ("rust" . ".rs")
    ("scala" . ".scala") ("swift" . ".swift") ("typescript" . ".ts")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "LeetCode programming language suffixes.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

;;; FIXME(6/13/24): use alists?
(defvar leetcode--filter-regex nil "Filter rows by regex.")
(defvar leetcode--filter-tag nil "Filter rows by tag.")
(defvar leetcode--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")

(defconst leetcode--all-difficulties '("easy" "medium" "hard"))
(defconst leetcode--paid "•" "Paid mark.")
(defconst leetcode--checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name "*leetcode*")

(defconst leetcode--retry-times 10
  "Max retry attempts for `leetcode-try' or `leetcode-submit'.")

(defface leetcode-paid-face
  '((t (:foreground "gold")))
  "Face for `leetcode--paid'.")

(defface leetcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode--checkmark'.")

(defface leetcode-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problems.")

(defface leetcode-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problems.")

(defface leetcode-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problems.")

(defface leetcode-accepted-face
  '((t (:foreground "#228b22")))
  "Face for submission accepted.")

(defface leetcode-error-face
  '((t (:inherit font-lock-warning-face)))
  "Face for submission compile error, runtime error and TLE.")

;;; Login
(defconst leetcode--domain "leetcode.com")
(defconst leetcode--url-base "https://leetcode.com")
(defconst leetcode--url-login (concat leetcode--url-base "/accounts/login"))

;; Cookie key name
(defconst leetcode--cookie-csrftoken "csrftoken")
(defconst leetcode--cookie-session "LEETCODE_SESSION")

;; Header
(defconst leetcode--User-Agent
  '("User-Agent" .
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode--X-CSRFToken "X-CSRFToken")
(defconst leetcode--Content-Type '("Content-Type" . "application/json"))

;; API URL
(defconst leetcode--url-api (concat leetcode--url-base "/api"))
(defconst leetcode--url-graphql (concat leetcode--url-base "/graphql"))
(defconst leetcode--url-all-problems (concat leetcode--url-api "/problems/all/"))
(defconst leetcode--url-all-tags (concat leetcode--url-base "/problems/api/tags"))
(defconst leetcode--url-daily-challenge
  (concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))

;; Submit
(defconst leetcode--url-submit (concat leetcode--url-base "/problems/%s/submit/"))
(defconst leetcode--url-problems-submission
  (concat leetcode--url-base "/problems/%s/submissions/"))
(defconst leetcode--url-check-submission
  (concat leetcode--url-base "/submissions/detail/%s/check/"))

;; Try testcase
(defconst leetcode--url-try
  (concat leetcode--url-base "/problems/%s/interpret_solution/"))


;; -------------------------------------------------------------------
;;; Utils

(defsubst leetcode--to-list (vec)
  "Convert VEC to list."
  (append vec '()))

(defsubst leetcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode--maybe-csrf-token ()
  "Return csrf token if it exists, otherwise return nil."
  (if-let ((cookie (seq-find
                    (lambda (item)
                      (string= (aref item 1)
                               leetcode--cookie-csrftoken))
                    (url-cookie-retrieve leetcode--domain "/" t))))
      (aref cookie 2)))

(defun leetcode--csrf-token ()
  "Return csrf token."
  (unless (leetcode--maybe-csrf-token)
    (url-retrieve-synchronously leetcode--url-login))
  (leetcode--maybe-csrf-token))

(defun leetcode--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defsubst leetcode--problem-link (title-slug)
  "Generate problem link from TITLE-SLUG."
  (concat leetcode--url-base "/problems/" title-slug))

(defsubst leetcode--add-font-lock (str face)
  "Add font-locking FACE to STR."
  (propertize str 'font-lock-face face))

(defun leetcode--stringify-difficulty (difficulty)
  "Stringify DIFFICULTY level (number) to \"easy\", \"medium\" or \"hard\"."
  (pcase-exhaustive difficulty
    (1 (leetcode--add-font-lock "easy" 'leetcode-easy-face))
    (2 (leetcode--add-font-lock "medium" 'leetcode-medium-face))
    (3 (leetcode--add-font-lock "hard" 'leetcode-hard-face))))

(defsubst leetcode--maybe-focus ()
  "Delete other windows if `leetcode-focus'."
  (and leetcode-focus (delete-other-windows)))

(defun leetcode-read-problem (&optional prompt by-slot return-slot &rest args)
  "Return leetcode problem id or RETURN-SLOT using `completing-read'.
Completion is offered on problem titles or BY-SLOT.
BY-SLOT and RETURN-SLOT must be slots in struct `leetcode-problem'.
PROMPT and ARGS are passed to `completing-read'."
  (if (stringp return-slot)
      (setq return-slot (intern return-slot))
    (or return-slot (setq return-slot 'id)))
  (if (stringp by-slot)
      (setq by-slot (intern by-slot))
    (or by-slot (setq by-slot 'title)))
  (let* ((collection
          (mapcar
           (lambda (problem)
             (cons (cl-struct-slot-value 'leetcode-problem by-slot problem)
                   (cl-struct-slot-value 'leetcode-problem return-slot problem)))
           (leetcode-problems-problems leetcode--problems)))
         (choice (apply #'completing-read
                        (or prompt "Problem: ") collection args)))
    (assoc-default choice collection)))

(defun leetcode--problem-from-slug (title-slug)
  "Return problem from `leetcode--problems' for TITLE-SLUG."
  (seq-find (lambda (p)
              (equal title-slug (leetcode-problem-title-slug p)))
            (leetcode-problems-problems leetcode--problems)))

(defun leetcode--problem-from-id (id)
  "Return problem from `leetcode--problems' for ID."
  (seq-find (lambda (p)
              (equal id (leetcode-problem-id p)))
            (leetcode-problems-problems leetcode--problems)))

(defsubst leetcode--slug-from-id (problem-id)
  "Return title slug for PROBLEM-ID."
  (leetcode-problem-title-slug (leetcode--problem-from-id problem-id)))

(defsubst leetcode--id-from-slug (title-slug)
  "Return problem id for TITLE-SLUG."
  (leetcode-problem-id (leetcode--problem-from-slug title-slug)))

(defun leetcode--filename-default (problem-id title-slug suffix)
  "Default function to generate a filename for a problem.
PROBLEM-ID and TITLE-SLUG come from the leetcode api. SUFFIX is entry matching
`leetcode-prefer-language' in `leetcode--lang-suffixes'."
  (if leetcode-save-solutions
      (format "%04d_%s%s" problem-id title-slug suffix)
    (concat title-slug suffix)))


;;; Buffers

(cl-defstruct (leetcode-buffers (:constructor leetcode--make-buffers)
                                (:copier nil))
  "Buffers associated with a problem."
  code detail test result)

(defvar leetcode--active (make-hash-table)
  "Hash of problem-id to `leetcode-buffers' for active problems.")

(defvar-local leetcode--problem-id nil
  "Local variable in `leetcode-buffers'.")

(defconst leetcode--buffers '(code detail test result)
  "Convenience variable with problem buffer slots.")

(defsubst leetcode--make-buffer-name (problem-id buf-type)
  "Return buffer name of BUF-TYPE for PROBLEM-ID."
  (format "*leetcode-%s-%s*" buf-type problem-id))

(defun leetcode-make-buffers (problem-id)
  "Create buffers for PROBLEM-ID."
  (leetcode--ensure-problems)
  (let ((bufs (leetcode--make-buffers
               :code (leetcode-code-buffer problem-id t)
               :detail (leetcode-detail-buffer problem-id t)
               :test (leetcode-test-buffer problem-id t)
               :result (leetcode-result-buffer problem-id t))))
    (puthash problem-id bufs leetcode--active)))

(defun leetcode-get-or-create-buffers (problem-id &optional lang)
  "Return buffers for PROBLEM-ID, creating them if necessary.
When LANG is non-nil, reset code buffer when it doesnt match LANG."
  (let ((bufs (gethash problem-id leetcode--active nil)))
    (when-let ((code (and bufs lang (leetcode-buffers-code bufs))))
      (unless (and (buffer-live-p code)
                   (string-suffix-p
                    (buffer-name code)
                    (assoc-default lang leetcode--lang-suffixes)))
        (setf (leetcode-buffers-code bufs) nil)))
    (if (and bufs
             (--all? (buffer-live-p
                      (cl-struct-slot-value 'leetcode-buffers it bufs))
                     leetcode--buffers))
        bufs
      (leetcode-make-buffers problem-id))))

(defun leetcode--get-buffer (problem-id buf-type)
  "Return buffer of BUF-TYPE for PROBLEM-ID if live."
  (when-let* ((bufs (gethash problem-id leetcode--active nil))
              (buf (cl-struct-slot-value 'leetcode-buffers buf-type bufs)))
    (and (buffer-live-p buf) buf)))

(defun leetcode--get-or-create-buffer
    (problem-id buf-type mode &optional create)
  "Return buffer for PROBLEM-ID of BUF-TYPE.
If CREATE is non-nil, a new buffer is created with major mode MODE."
  (or (--when-let (leetcode--get-buffer problem-id buf-type)
        (and (buffer-live-p it) it))
      (let ((buf-name (leetcode--make-buffer-name
                       problem-id (symbol-name buf-type))))
        (or (get-buffer buf-name)
            (when create
              (with-current-buffer (get-buffer-create buf-name)
                (funcall mode)
                (setq-local leetcode--problem-id problem-id)
                (current-buffer)))))))

(defun leetcode-result-buffer (problem-id &optional create)
  "Return result buffer for PROBLEM-ID.
If CREATE is non-nil, and no buffer exists a new one is created."
  (leetcode--get-or-create-buffer
   problem-id 'result #'leetcode-result-mode create))

(defun leetcode-test-buffer (problem-id &optional create)
  "Return test buffer for PROBLEM-ID.
If CREATE is non-nil, and no buffer exists a new one is created."
  (leetcode--get-or-create-buffer
   problem-id 'test #'leetcode-test-mode create))

(defun leetcode-detail-buffer (problem-id &optional create)
  "Return detail buffer for PROBLEM-ID.
If CREATE is non-nil, and no buffer exists a new one is created."
  (leetcode--get-or-create-buffer
   problem-id 'detail #'leetcode-detail-mode create))

(defun leetcode--code-filename (problem-id)
  "Return filename for PROBLEM-ID."
  (let ((title-slug (leetcode--slug-from-id problem-id))
        (suffix (assoc-default leetcode--lang leetcode--lang-suffixes)))
    (funcall leetcode-filename-function problem-id title-slug suffix)))

(defun leetcode-code-buffer (problem-id &optional create)
  "Return code buffer for PROBLEM-ID.
If CREATE is non-nil, create new buffer when necessary."
  ;; Don't return cached buffer since `leetcode--lang' may have changed
  (let ((name (leetcode--code-filename problem-id)))
    (or (get-buffer name)
        (when create
          (with-current-buffer
              (if (not leetcode-save-solutions)
                  (get-buffer-create name)
                (unless (file-directory-p leetcode-directory)
                  (make-directory leetcode-directory))
                (find-file-noselect
                 (expand-file-name name leetcode-directory)))
            (setq-local leetcode--problem-id problem-id)
            (current-buffer))))))

(defvar leetcode-solution-minor-mode)

(defun leetcode--buffer-type (&optional buffer)
  "Return the type of leetcode BUFFER or nil."
  (with-current-buffer (or buffer (current-buffer))
    (cond (leetcode-solution-minor-mode 'code)
          ((pcase major-mode
             ('leetcode-detail-mode 'detail)
             ('leetcode-result-mode 'result)
             ('leetcode-test-mode 'test)
             (_ nil)))
          ((string-prefix-p "*leetcode-" (buffer-name))
           (intern (replace-regexp-in-string
                    "^[*]leetcode-\\([^-]+\\)-.*" "\\1" (buffer-name))))
          (t nil))))

(defsubst leetcode--problems-current-id ()
  "Get id of the current problem in `leetcode-problems-mode'."
  (string-to-number (aref (tabulated-list-get-entry) 1)))

(defun leetcode-current-problem-id (&optional buffer no-error)
  "Return the problem-id associated with BUFFER or nil.
If NO-ERROR is non-nil, dont error when no problem-id is found."
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (or leetcode--problem-id
        (if (derived-mode-p 'leetcode-problems-mode)
            (leetcode--problems-current-id)
          (when-let ((buf-type (leetcode--buffer-type buffer)))
            (catch 'done
              (maphash
               (lambda (id bufs)
                 (when (eq (cl-struct-slot-value 'leetcode-buffers buf-type bufs)
                           buffer)
                   (throw 'done id)))
               leetcode--active))))
        (unless no-error
          (user-error "No current problem id found for %S" buffer)))))

(defun leetcode-problem-buffers (&optional problem-id buffer no-error)
  "Return buffers for PROBLEM-ID, BUFFER, or the current buffer by default.
If NO-ERROR is non-nil, dont error when no problem-id is found."
  (or problem-id (setq problem-id (leetcode-current-problem-id buffer no-error)))
  (when problem-id
    (gethash problem-id leetcode--active nil)))


;; -------------------------------------------------------------------
;;; API

(defun leetcode--json-read (buffer)
  "Read JSON from response BUFFER."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (condition-case err
        (prog1 (json-read)
          (kill-buffer (current-buffer)))
      (error (leetcode--debug "json-read: %s" (error-message-string err))
             (kill-buffer (current-buffer))
             nil))))

(defmacro leetcode-with-results (submit-type status &rest body)
  "Anaphoric macro that binds `it' to request results.
If STATUS is an error, the error message is prefixed with SUBMIT-TYPE."
  (declare (indent 2))
  (and (symbolp submit-type)
       (setq submit-type (symbol-name submit-type)))
  `(if-let ((err (plist-get ,status :error)))
       (leetcode--debug (format (concat ,submit-type " error: %S") err))
     (let ((it (leetcode--json-read (current-buffer))))
       ,@body)))

(cl-defun leetcode--url-retrieve
    (url &key (referrer leetcode--url-login) (method "GET") data callback cbargs)
  "Retrieve URL synchronously or asynchronously when CALLBACK is non-nil.
METHOD and REFERRER are used to set url headers.
DATA is Json encoded and used as request data.
CALLBACK and CBARGS are passed to `url-retrieve'."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (delq nil (list leetcode--User-Agent
                          leetcode--X-Requested-With
                          (cons leetcode--X-CSRFToken (leetcode--csrf-token))
                          (and referrer (leetcode--referer referrer))
                          (and (equal method "POST") leetcode--Content-Type))))
         (url-request-data (and data (json-encode data))))
    (leetcode--debug "url-retrieve [%s]: %s %S %S"
                     (propertize url-request-method
                                 'font-lock-face 'font-lock-keyword-face)
                     url url-request-extra-headers url-request-data)
    (if (null callback)
        (url-retrieve-synchronously url)
      (leetcode--loading-mode t)
      (funcall #'url-retrieve url callback cbargs))))
(put 'leetcode--url-retrieve 'lisp-indent-function 1)


(defun leetcode--api-fetch-all-tags (&rest kwargs)
  "Fetch all problems' tags.
KWARGS args passed to `leetcode--url-retrieve'."
  (let ((res (apply #'leetcode--url-retrieve leetcode--url-all-tags kwargs)))
    (unless (plist-get kwargs :callback)
      (leetcode--json-read res))))

(defun leetcode--api-fetch-user-and-problems (&rest kwargs)
  "Fetch user and problems info.
KWARGS args passed to `leetcode--url-retrieve'."
  (let ((res (apply #'leetcode--url-retrieve leetcode--url-all-problems kwargs)))
    (unless (plist-get kwargs :callback)
      (leetcode--json-read res))))

(defun leetcode--problem-graphql-params (operation &optional vars)
  "Construct a GraphQL parameter.
OPERATION and VARS are LeetCode GraphQL parameters."
  (list (cons "operationName" operation)
        (cons "query"
              (graphql-query
               questionData
               (:arguments
                (($titleSlug . String!))
                (question
                 :arguments
                 ((titleSlug . ($ titleSlug)))
                 likes
                 dislikes
                 content
                 exampleTestcases
                 (topicTags slug)
                 (codeSnippets langSlug code)))))
        (if vars (cons "variables" vars))))

(defun leetcode--api-fetch-problem (slug)
  "Fetch single problem.
SLUG is a problem's title slug.
Return a object with following attributes:
:likes     Number
:dislikes  Number
:content   String
:topicTags String"
  (->> (leetcode--json-read
        (leetcode--url-retrieve leetcode--url-graphql
          :method "POST"
          :data (leetcode--problem-graphql-params
                 "questionData" (list (cons "titleSlug" slug)))))
       (alist-get 'data)
       (alist-get 'question)))


;;; Login

(defun leetcode-login ()
  "Steal LeetCode login session from local browser.
It also cleans LeetCode cookies in `url-cookie-file'."
  (interactive)
  (ignore-errors (url-cookie-delete-cookies leetcode--domain))
  (leetcode--csrf-token)    ; knock knock, whisper me the mysterious information
  (let* ((my-cookies (executable-find "my_cookies"))
         (my-cookies-output (shell-command-to-string my-cookies))
         (cookies-list (seq-filter
                        (lambda (s) (not (string-empty-p s)))
                        (split-string my-cookies-output "\n")))
         (cookies-pairs (seq-map
                         (lambda (s) (split-string s))
                         cookies-list))
         (leetcode-session (cadr (assoc leetcode--cookie-session cookies-pairs)))
         (leetcode-csrftoken (cadr (assoc "csrftoken" cookies-pairs))))
    (leetcode--debug "login session: %s" leetcode-session)
    (leetcode--debug "login csrftoken: %s" leetcode-csrftoken)
    (url-cookie-store leetcode--cookie-session leetcode-session nil leetcode--domain "/" t)
    (url-cookie-store "csrftoken" leetcode-csrftoken nil leetcode--domain "/" t)))

(defun leetcode--logged-in-p ()
  "Return non-nil if user is logged in."
  (when-let ((username (leetcode-user-username leetcode--user)))
    (unless (string-empty-p username)
      (seq-find (lambda (item)
                  (string= (aref item 1) leetcode--cookie-session))
                (url-cookie-retrieve leetcode--domain "/" t)))))

(defsubst leetcode--maybe-login ()
  "Ensure user is logged in."
  (or (leetcode--logged-in-p)
      (leetcode-login)))


;;; Fetch Problems / Tags

(defun leetcode--ensure-problems ()
  "Ensure `leetcode--problems' has been populated."
  (or (leetcode-problems-problems leetcode--problems)
      (-> (leetcode--api-fetch-user-and-problems)
          (leetcode--set-user-and-problems))))

(defun leetcode--set-user-and-problems (user-and-problems)
  "Set `leetcode--user' and `leetcode--problems'.
If user isn't logged in, only sets `leetcode--problems'.
USER-AND-PROBLEMS is an alist returned by `leetcode--url-all-problems'."
  (let-alist user-and-problems
    (setf (leetcode-user-username leetcode--user) .user_name
          (leetcode-user-solved leetcode--user) .num_solved
          (leetcode-user-easy leetcode--user) .ac_easy
          (leetcode-user-medium leetcode--user) .ac_medium
          (leetcode-user-hard leetcode--user) .ac_hard)
    (leetcode--debug "set user: %s, solved %s in %s problems"
                     .user_name .num_solved .num_total)
    ;; Problem list
    (setf (leetcode-problems-num leetcode--problems) .num_total
          (leetcode-problems-tag leetcode--problems) "all")
    (let (problems)
      (cl-loop for el across .stat_status_pairs
               do (let-alist el
                    ;; (leetcode--debug
                    ;;  "frontend_question_id: %s, question_id: %s, title: %s"
                    ;;  .stat.frontend_question_id .stat.question_id
                    ;;  .stat.question__title)
                    (push
                     (make-leetcode-problem
                      :status .status
                      :id .stat.frontend_question_id
                      :backend-id .stat.question_id
                      :title .stat.question__title
                      :title-slug .stat.question__title_slug
                      :acceptance (format "%.1f%%"
                                          (* 100
                                             (/ (float .stat.total_acs)
                                                .stat.total_submitted)))
                      :difficulty .difficulty.level
                      :paid-only (eq .paid_only t))
                     problems)))
      (setf (leetcode-problems-problems leetcode--problems) problems))))

(defun leetcode--set-tags (all-tags)
  "Set `leetcode--all-tags' and `leetcode--problems' with ALL-TAGS."
  (let ((tags-table (make-hash-table :size 2000)))
    (let-alist all-tags
      (leetcode--debug "fetched tags (%d topics)" (length .topic))
      (dolist (topic (leetcode--to-list .topics))
        (let-alist topic
          ;; Set leetcode--all-tags
          (unless (member .slug leetcode--all-tags)
            (push .slug leetcode--all-tags))
          ;; tags-table cache with backend-id
          (dolist (id (leetcode--to-list .questions))
            (let ((tags (gethash id tags-table)))
              (setf (gethash id tags-table) (cons .slug tags)))))))
    ;; Set problems tags with tags-table
    (dolist (problem (leetcode-problems-problems leetcode--problems))
      (let ((backend-id (leetcode-problem-backend-id problem)))
        (setf (leetcode-problem-tags problem) (gethash backend-id tags-table))))))


;;; Tabulated problem list

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (leetcode-problems-problems leetcode--problems))
        rows)
    (dolist (problem problems (reverse rows))
      (pcase-let (((cl-struct leetcode-problem
                              paid-only id status title acceptance tags difficulty)
                   problem))
        (when (or leetcode--display-paid (not paid-only))
          (push (vector
                 (if (equal status "ac")
                     (leetcode--add-font-lock leetcode--checkmark
                                              'leetcode-checkmark-face)
                   " ")
                 (number-to-string id)
                 (concat title " " (if (eq paid-only t)
                                       (leetcode--add-font-lock
                                        leetcode--paid 'leetcode-paid-face)
                                     " "))
                 acceptance
                 (leetcode--stringify-difficulty difficulty)
                 (and leetcode--display-tags (string-join tags ", ")))
                rows))))))

(defsubst leetcode--row-tags (row)
  "Get tags from ROW."
  (aref row 5))

(defsubst leetcode--row-difficulty (row)
  "Get difficulty from ROW."
  (aref row 4))

(defun leetcode--filter (rows)
  "Filter ROWS.
Rows are filtered by `leetcode--filter-regex', `leetcode--filter-tag' and
`leetcode--filter-difficulty'."
  (seq-filter
   (lambda (row)
     (and (if leetcode--filter-regex
              (let ((title (aref row 2)))
                (string-match-p leetcode--filter-regex title))
            t)
          (if leetcode--filter-tag
              (let ((tags (split-string (leetcode--row-tags row) ", ")))
                (member leetcode--filter-tag tags))
            t)
          (if leetcode--filter-difficulty
              (let ((difficulty (leetcode--row-difficulty row)))
                (string= difficulty leetcode--filter-difficulty))
            t)))
   rows))


;;; Problems List Commands

(defun leetcode-reset-filter ()
  "Reset filter."
  (interactive)
  (setq leetcode--filter-regex nil)
  (setq leetcode--filter-tag nil)
  (setq leetcode--filter-difficulty nil)
  (leetcode-refresh))

(defun leetcode-set-filter-regex (regex)
  "Set `leetcode--filter-regex' as REGEX and refresh."
  (interactive (list (read-string "Search: " nil 'leetcode-filter-history)))
  (setq leetcode--filter-regex regex)
  (leetcode-refresh))

(defun leetcode-set-filter-tag ()
  "Set `leetcode--filter-tag' from `leetcode--all-tags' and refresh."
  (interactive)
  (setq leetcode--filter-tag (completing-read "Tags: " leetcode--all-tags))
  (leetcode-refresh))

(defun leetcode-set-prefer-language ()
  "Set `leetcode-prefer-language' from `leetcode--lang-suffixes' and refresh."
  (interactive)
  (setq leetcode-prefer-language
        (completing-read "Language: " leetcode--lang-suffixes))
  (leetcode-refresh))

(defun leetcode-set-filter-difficulty ()
  "Set `leetcode--filter-difficulty' from `leetcode--all-difficulties' and refresh."
  (interactive)
  (setq leetcode--filter-difficulty
        (completing-read "Difficulty: " leetcode--all-difficulties))
  (leetcode-refresh))

(defun leetcode-toggle-tag-display ()
  "Toggle `leetcode--display-tags' and refresh."
  (interactive)
  (setq leetcode--display-tags (not leetcode--display-tags))
  (leetcode-refresh))

(defun leetcode-toggle-paid-display ()
  "Toggle `leetcode--display-paid' and refresh."
  (interactive)
  (setq leetcode--display-paid (not leetcode--display-paid))
  (leetcode-refresh))

(defun leetcode--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths (seq-reduce (lambda (acc row)
                              (cl-mapcar (lambda (a col)
                                           (max a (length col)))
                                         acc
                                         (append row '())))
                            rows
                            (seq-map #'length header-names))))
    (vconcat
     (cl-mapcar
      (lambda (col size) (list col size nil))
      header-names widths))))

(defun leetcode-refresh ()
  "Make `tabulated-list-entries'."
  (interactive)
  (let* ((header-names (append '(" " "#" "Problem" "Acceptance" "Difficulty")
                               (if leetcode--display-tags '("Tags"))))
         (rows (leetcode--filter (leetcode--problems-rows)))
         (headers (leetcode--make-tabulated-headers header-names rows)))
    (with-current-buffer (get-buffer-create leetcode--buffer-name)
      (leetcode-problems-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-entries
            (cl-mapcar (lambda (i x) (list i x))
                       (number-sequence 0 (1- (length rows)))
                       rows))
      (tabulated-list-init-header)
      (tabulated-list-print t))))

(defun leetcode-refresh-fetch-async ()
  "Same as `leetcode-refresh-fetch', but fetches asynchronously."
  (interactive)
  (let ((buf (current-buffer)))
    (cl-flet* ((finish-loading ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (leetcode--loading-mode -1))))
               (fetch-tags ()
                 (leetcode--api-fetch-all-tags
                  :callback
                  (lambda (status &rest _args)
                    (leetcode-with-results "tags" status
                      (leetcode--set-tags it)
                      (setq leetcode--display-tags leetcode-prefer-tag-display)
                      (leetcode-reset-filter)
                      (leetcode-refresh))
                    (finish-loading)))))
      (leetcode--api-fetch-user-and-problems
       :callback (lambda (status &rest _args)
                   (leetcode-with-results "user/problems" status
                     (leetcode--set-user-and-problems it)
                     (fetch-tags)))))))

(defun leetcode-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (let ((users-and-problems (leetcode--api-fetch-user-and-problems))
        (all-tags (leetcode--api-fetch-all-tags)))
    (if users-and-problems
        (leetcode--set-user-and-problems users-and-problems)
      (leetcode--warn "LeetCode parse user and problems failed"))
    (if all-tags
        (leetcode--set-tags all-tags)
      (leetcode--warn "LeetCode fetch all tags failed")))
  (setq leetcode--display-tags leetcode-prefer-tag-display)
  (leetcode-reset-filter)
  (leetcode-refresh))

(defun leetcode--show-problems ()
  "Show leetcode problems buffer."
  (if (get-buffer leetcode--buffer-name)
      (pop-to-buffer leetcode--buffer-name)
    (leetcode--maybe-login)
    (leetcode-refresh-fetch)
    (pop-to-buffer leetcode--buffer-name))
  (leetcode--maybe-focus))

;;;###autoload
(defun leetcode ()
  "Show leetcode problems."
  (interactive)
  (if (leetcode--check-deps)
      (leetcode--show-problems)
    (message "installing leetcode dependencies...")))

;;;###autoload
(defun leetcode-daily ()
  "Open the daily challenge."
  (interactive)
  (leetcode--maybe-login)
  (leetcode--ensure-problems)
  (let-alist (leetcode--json-read
              (leetcode--url-retrieve leetcode--url-graphql
                :method "POST"
                :data `((operationName . "questionOfToday")
                        (query         . ,leetcode--url-daily-challenge))))
    (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
      (leetcode-show-problem (string-to-number qid)))))


;;; Submissions

(defun leetcode--buffer-content-default ()
  "Function to return code from buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun leetcode--buffer-content (buffer &optional code)
  "Return BUFFER contents for submission.
If CODE is non-nil, call `leetcode-buffer-code-function'."
  (with-current-buffer buffer
    (if code
        (funcall leetcode-buffer-code-function)
      (leetcode--buffer-content-default))))

(defun leetcode--poll-submission
    (status attempt wait submission-id title-slug callback &optional cbargs)
  "Poll STATUS and apply CALLBACK to result and CBARGS on sucess.
ATTEMPT is the polling count.
WAIT is time to wait when not successful.
SUBMISSION-ID and TITLE-SLUG belong to the problem being submitted."
  (leetcode-with-results "poll" status
    (if (and it (equal (alist-get 'state it) "SUCCESS"))
        (apply callback it cbargs)
      (if (>= attempt leetcode--retry-times)
          (leetcode--warn "Polling timeout: %S %S" callback cbargs)
        (sit-for wait)
        (leetcode--api-check-submission
         submission-id title-slug callback cbargs (1+ attempt) (* 1.05 wait))))))

(defun leetcode--api-check-submission
    (submission-id title-slug callback &optional cbargs attempt wait)
  "Check api to for successful submission of problem SUBMISSION-ID.
On sucess, CALLBACK is called with result and CBARGS.
TITLE-SLUG, ATTEMPT, and WAIT are passed to `leetcode--poll-submission'.
After each submission (either try or submit), LeetCode returns a
SUBMISSION-ID. With the SUBMISSION-ID, client will poll for the submission
detail."
  (or attempt (setq attempt 0))
  (or wait (setq wait 1))
  (leetcode--url-retrieve (format leetcode--url-check-submission submission-id)
    :referrer (format leetcode--url-problems-submission title-slug)
    :callback #'leetcode--poll-submission
    :cbargs (list attempt wait submission-id title-slug callback cbargs)))

(defun leetcode--polling-callback (type title-slug callback &optional on-response)
  "Return a callback for submission TYPE that polls the api's check endpoint.
After the submission runs successfully, CALLBACK is called with the results. If
ON-RESPONSE is non-nil, it is called on the initial request response, not the
the final checked result.
TITLE-SLUG is passed to `leetcode--api-check-submission'."
  (macroexpand-all
   (lambda (status &rest args)
     (leetcode-with-results type status
       (let-alist it
         (and on-response (funcall on-response it))
         (leetcode--api-check-submission
          (if (equal type 'submit) .submission_id .interpret_id)
          title-slug callback args))))))

(cl-defun leetcode--api-submit-problem
    (submit-type endpoint callback &key data on-response)
  "Send api request to ENDPOINT with problem code to run.
SUBMIT-TYPE is either \\='submit or \\='try.
DATA is appended to shared data (lang, question_id, typed_code).
CALLBACK and ON-RESPONSE are passed to `leetcode--polling-callback'.
CALLBACK is called as (callback RESULT RESULT-BUF CODE-BUF)."
  (pcase-let* ((problem-id (leetcode-current-problem-id))
               ((cl-struct leetcode-problem id backend-id title-slug)
                (leetcode--problem-from-id problem-id))
               ((cl-struct leetcode-buffers code result)
                (leetcode-get-or-create-buffers problem-id)))
    (run-hooks 'leetcode-before-submit-hook)
    (leetcode--debug "%s: slug=%s, id=%s, buf=%S" submit-type title-slug id code)
    (leetcode--url-retrieve (format endpoint title-slug)
      :method "POST"
      :referrer (format leetcode--url-problems-submission title-slug)
      :data (append ; Data common to try/submit
             `((lang        . ,leetcode--lang)
               (question_id . ,backend-id)
               (typed_code  . ,(leetcode--buffer-content code t)))
             data)
      :callback (leetcode--polling-callback
                 submit-type title-slug callback on-response)
      :cbargs (list result code))))

(defun leetcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (leetcode--api-submit-problem
   'submit leetcode--url-submit #'leetcode--show-submission-result))

(defun leetcode-try ()
  "Asynchronously test the code using testcases from test buffer."
  (interactive)
  (pcase-let* ((problem-id (leetcode-current-problem-id))
               ((cl-struct leetcode-buffers result test)
                (leetcode-get-or-create-buffers problem-id)))
    (leetcode--api-submit-problem
     'try leetcode--url-try #'leetcode--show-try-result
     :data `((data_input  . ,(leetcode--buffer-content test))
             (judge_type  . "small"))
     :on-response (lambda (res)
                    (let-alist res
                      (with-current-buffer result
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert "Your input:\n" .test_case "\n\n")))
                      (display-buffer result leetcode-display-buffer-alist))))))

(defun leetcode--show-submission-result (result result-buf code-buf)
  "Format RESULT in RESULT-BUF based on status code.
CODE-BUF is the source buffer.
Error info comes from submission RESULT.

\\='status_code has following possible value:

- 10: Accepted
- 11: Wrong Anwser
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
  (with-current-buffer code-buf (leetcode--loading-mode -1))
  (cl-macrolet ((status-error (msg)
                  `(format "Status: %s"
                           (leetcode--add-font-lock ,msg 'leetcode-error-face)))
                (status-result (msg correct tests face)
                  `(format "Status: %s\n\n"
                           (leetcode--add-font-lock
                            (format "%s (%s/%s)" ,msg ,correct ,tests) ',face))))
    (let-alist result
      (when (>= .status_code 14)
        (setq next-error-last-buffer result-buf))
      (with-current-buffer result-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (pcase .status_code
            (10 (insert
                 (status-result .status_msg .total_correct .total_testcases
                                leetcode-accepted-face)
                 (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n"
                         .status_runtime .runtime_percentile .pretty_lang)
                 (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                         .status_memory .memory_percentile .pretty_lang)))
            (11 (insert (status-result .status_msg .total_correct .total_testcases
                                       leetcode-error-face)
                        (format "Test Case: \n%s\n\n" .input)
                        (format "Answer: %s\n\n" .code_output)
                        (format "Expected Answer: %s\n\n" .expected_output))
                (unless (string-empty-p .std_output)
                  (insert (format "Stdout: \n%s\n" .std_output))))
            (14 (insert (status-error .status_msg) "\n"))
            (15 (insert (status-error .status_msg) "\n\n"
                        (format .full_runtime_error)))
            (20 (insert (status-error .status_msg) "\n\n"
                        (format .full_compile_error)))))
        (display-buffer (current-buffer) leetcode-display-buffer-alist)))))

(defun leetcode--show-try-result (results result-buf code-buf)
  "Update RESULTS in RESULT-BUF for TITLE-SLUG after running test cases.
CODE-BUF is the source buffer."
  (with-current-buffer code-buf (leetcode--loading-mode -1))
  (let-alist results
    (when (>= .status_code 14)
      (setq next-error-last-buffer result-buf))
    (with-current-buffer result-buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (pcase .status_code
          (10 (insert "Output:\n")
              (seq-do (lambda (res) (insert res "\n")) .code_answer)
              (insert "\nExpected:\n")
              (seq-do (lambda (res) (insert res "\n")) .expected_code_answer)
              (insert "\n"))
          (14 (insert .status_msg))
          (15 (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face)
                      "\n\n" .full_runtime_error))
          (20 (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face)
                      "\n\n" .full_compile_error)))
        (when (> (length .code_output) 0)
          (insert "\n\n")
          (insert "Code output:\n")
          (seq-do (lambda (res) (insert res "\n")) .code_output))
        (insert "\n\n"))
      (display-buffer (current-buffer) leetcode-display-buffer-alist))))


;; -------------------------------------------------------------------
;;; Display

(defvar leetcode--windows '()
  "Holds references to leetcode windows.")

(defun leetcode--split-window (size &optional horizontal)
  "Split current window below, or HORIZONTAL if non-nil, using SIZE.
Return new window."
  (funcall (if horizontal #'split-window-horizontally #'split-window-below)
           (if (floatp size)
               (round (* size (if horizontal (frame-width) (frame-height))))
             size)))

(defun leetcode--solving-window-layout ()
  "Configure windows for problem solving to use BUFFERS.
See `leetcode-display-configuration' for more details."
  (delete-other-windows)
  (setq leetcode--windows nil)
  (let ((win (get-buffer-window (current-buffer)))
        (buffers (leetcode-problem-buffers))
        (i 0))
    (pcase-dolist (`(,buf . ,size) leetcode-display-configuration)
      (push (cons buf win) leetcode--windows)
      (when buffers
        (set-window-buffer
         win (cl-struct-slot-value 'leetcode-buffers buf buffers)))
      (and (< i 3) (setq win (leetcode--split-window size (= i 0))))
      (cl-incf i)
      (other-window 1))))

(defun leetcode-restore-layout ()
  "Restore the window layout based on current buffer's associated problem id."
  (interactive)
  (let ((problem-id (leetcode-current-problem-id)))
    (unless (buffer-live-p (leetcode-detail-buffer problem-id))
      (leetcode-show-problem problem-id))
    (leetcode-get-or-create-buffers problem-id)
    (leetcode--solving-window-layout)))


;; -------------------------------------------------------------------
;;; Problem Description

(defun leetcode--render-detail-buffer (problem problem-info)
  "Return description buffer for PROBLEM rendered with `shr-render-buffer'.
PROBLEM-INFO is problem's metadata."
  (pcase-let* ((problem-id (leetcode-problem-id problem-info))
               ((cl-struct leetcode-problem title title-slug difficulty)
                problem-info)
               (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (let-alist problem
      (with-temp-buffer
        (insert "<h1>" (number-to-string problem-id) ". " title "</h1>"
                (capitalize (leetcode--stringify-difficulty difficulty))
                html-margin
                "likes: " (number-to-string .likes) html-margin
                "dislikes: " (number-to-string .dislikes)
                ;; Sometimes LeetCode doesn't have a '<p>' at the outermost...
                ;; Note: content might be nil for a paid problem
                "<p>" (or .content "<i>Description not available</i>") "</p>")
        (setq shr-current-font t)
        (leetcode--replace-in-buffer "" "")
        ;; NOTE: shr.el can't render "https://xxxx.png", so we use "http"
        (leetcode--replace-in-buffer "https" "http")
        (shr-render-buffer (current-buffer)))
      (with-current-buffer "*html*"
        (save-match-data
          (re-search-forward "dislikes: .*" nil t)
          (insert (make-string 4 ?\s))
          (insert-text-button
           "Solve it"
           'action (lambda (_btn)
                     (leetcode--start-coding problem problem-info))
           'help-echo "Solve the problem.")
          (insert (make-string 4 ?\s))
          (insert-text-button
           "Link"
           'action (lambda (_btn)
                     (browse-url (leetcode--problem-link title-slug)))
           'help-echo "Open the problem in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button
           "Solution"
           'action (lambda (_btn)
                     (browse-url
                      (concat (leetcode--problem-link title-slug) "/solution")))
           'help-echo "Open the problem solution page in browser."))
        (rename-buffer (leetcode--make-buffer-name problem-id "detail"))
        (leetcode-detail-mode)
        (goto-char (point-min))
        (and (search-forward "Solve it" nil t 1)
             (goto-char (match-beginning 0)))
        (current-buffer)))))

(defun leetcode--switch-to-detail-buffer (problem problem-info)
  "Switch to description buffer for PROBLEM, creating it when necessary.
PROBLEM-INFO is metadata for PROBLEM."
  (let* ((problem-id (leetcode-problem-id problem-info))
         (detail-buf (or (leetcode-detail-buffer problem-id)
                         (leetcode--render-detail-buffer problem problem-info))))
    (leetcode--maybe-focus)
    (switch-to-buffer detail-buf)))

(defun leetcode--read-problem-or-current (&rest args)
  "Return problem id with completing read or `leetcode-current-problem-id'.
ARGS are passed to `leetcode-read-problem'."
  (or (apply #'leetcode-read-problem args)
      (leetcode-current-problem-id)))

(defun leetcode-show-problem (problem-id)
  "Jump to description for problem with id PROBLEM-ID."
  (interactive (list (leetcode--read-problem-or-current "Show problem: ")))
  (if-let ((buf (leetcode-detail-buffer problem-id)))
      (pop-to-buffer buf)
    (leetcode--ensure-problems)
    (let* ((problem-info (leetcode--problem-from-id problem-id))
           (problem (leetcode--api-fetch-problem
                     (leetcode-problem-title-slug problem-info))))
      (leetcode--switch-to-detail-buffer problem problem-info))))

(defun leetcode-show-problem-by-slug (title-slug)
  "Jump to description for problem with TITLE-SLUG.
For example: in an org-link elisp:(leetcode-show-problem-by-slug \"3sum\")."
  (interactive (list (leetcode--read-problem-or-current
                      "Show problem: " 'title-slug 'title-slug)))
  (leetcode-show-problem
   (leetcode-problem-id (leetcode--problem-from-slug title-slug))))

(defun leetcode-show-current-problem ()
  "Jump to description for current problem."
  (interactive)
  (leetcode-show-problem (leetcode-current-problem-id)))

(defun leetcode-view-problem (problem-id)
  "Display PROBLEM-ID's problem description in another window."
  (interactive (list (leetcode--read-problem-or-current "View problem: ")))
  (leetcode-show-problem problem-id))

(defun leetcode-view-current-problem ()
  "Display current problem's description in another window."
  (interactive)
  (leetcode-view-problem (leetcode-current-problem-id)))

(defun leetcode-show-problem-in-browser (problem-id)
  "Open the problem with id PROBLEM-ID in browser."
  (interactive (list (leetcode--read-problem-or-current "Browse problem: ")))
  (let ((link (leetcode--problem-link
               (leetcode-problem-title-slug
                (leetcode--problem-from-id problem-id)))))
    (leetcode--debug "open in browser: %s" link)
    (browse-url link)))

(defun leetcode-show-current-problem-in-browser ()
  "Open the current problem in browser.
Call `leetcode-show-problem-in-browser' on the current problem id."
  (interactive)
  (leetcode-show-problem-in-browser (leetcode-current-problem-id)))

(defun leetcode-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (leetcode--read-problem-or-current "Solve problem: ")))
  (let* ((problem-info (leetcode--problem-from-id problem-id))
         (problem (leetcode--api-fetch-problem
                   (leetcode-problem-title-slug problem-info))))
    (leetcode--switch-to-detail-buffer problem problem-info)
    (leetcode--start-coding problem problem-info)))

(defun leetcode-solve-current-problem ()
  "Start coding the current problem.
Call `leetcode-solve-problem' on the current problem id."
  (interactive)
  (leetcode-solve-problem (leetcode-current-problem-id)))


;; -------------------------------------------------------------------
;;; Cleanup

(defun leetcode--kill-buff-and-delete-window (buf)
  "Kill BUF and delete its window."
  (when (buffer-live-p buf)
    (delete-windows-on buf t)
    (kill-buffer buf)))

(defun leetcode-quit ()
  "Close and delete leetcode related buffers and windows."
  (interactive)
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--buffer-name))
  (maphash (lambda (_id bufs)
             (dolist (buf leetcode--buffers)
               (leetcode--kill-buff-and-delete-window
                (cl-struct-slot-value 'leetcode-buffers buf bufs))))
           leetcode--active)
  (clrhash leetcode--active))


;; -------------------------------------------------------------------
;;; Coding

(defun leetcode--set-lang (snippets)
  "Set `leetcode--lang' based on langSlug in SNIPPETS for PROBLEM-ID."
  (setq leetcode--lang
        ;; If there is a mysql snippet, we use mysql as our prefer language.
        (if (seq-find (lambda (s)
                        (equal (alist-get 'langSlug s) leetcode-prefer-sql))
                      snippets)
            leetcode-prefer-sql
          leetcode-prefer-language))
  (run-hook-with-args 'leetcode-after-set-language-hook leetcode--lang))

(defun leetcode--setup-code-buffer (code-buf snippets)
  "Setup source buffer, CODE-BUF, from SNIPPETS."
  (let ((suffix (assoc-default leetcode--lang leetcode--lang-suffixes)))
    (with-current-buffer code-buf
      (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
      (leetcode-solution-minor-mode t)
      (let* ((snippet (seq-find (lambda (s)
                                  (equal (alist-get 'langSlug s)
                                         leetcode--lang))
                                snippets))
             (template-code (alist-get 'code snippet)))
        (unless template-code
          (user-error "No template code for %s!" leetcode--lang))
        (unless (save-mark-and-excursion
                  (goto-char (point-min))
                  (search-forward (string-trim template-code) nil t))
          (insert template-code))
        (leetcode--replace-in-buffer "" "")))))

(defun leetcode--start-coding (problem problem-info)
  "Create a buffer for coding PROBLEM with meta-data PROBLEM-INFO.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'."
  (let-alist problem
    (let ((problem-id (leetcode-problem-id problem-info))
          (snippets (leetcode--to-list .codeSnippets)))
      ;; Set language before getting buffers in case lang changed
      (leetcode--set-lang snippets)
      (pcase-let (((cl-struct leetcode-buffers code test)
                   (leetcode-get-or-create-buffers problem-id leetcode--lang)))
        ;; Setup code buffer
        (if (zerop (buffer-size code))
            (leetcode--setup-code-buffer code snippets)
          (with-current-buffer code
            (or leetcode-solution-minor-mode
                (leetcode-solution-minor-mode t))))
        ;; Setup test buffer
        (with-current-buffer test
          (erase-buffer)
          (insert .exampleTestcases))
        (leetcode--solving-window-layout)
        (set-buffer code)))))


;; -------------------------------------------------------------------
;;; Problems Mode

(defvar-keymap leetcode-problems-mode-map
  :doc "Keymap active in `leetcode-problems-mode'."
  :suppress t
  "RET" #'leetcode-show-current-problem
  "TAB" #'leetcode-view-current-problem
  "C-o" #'leetcode-view-current-problem
  "o" #'leetcode-show-current-problem
  "O" #'leetcode-show-problem
  "v" #'leetcode-view-current-problem
  "V" #'leetcode-view-problem
  "b" #'leetcode-show-current-problem-in-browser
  "B" #'leetcode-show-problem-in-browser
  "c" #'leetcode-solve-current-problem
  "C" #'leetcode-solve-problem
  "s" #'leetcode-set-filter-regex
  "L" #'leetcode-set-prefer-language
  "t" #'leetcode-set-filter-tag
  "T" #'leetcode-toggle-tag-display
  "P" #'leetcode-toggle-paid-display
  "d" #'leetcode-set-filter-difficulty
  "g" #'leetcode-refresh
  "G" #'leetcode-refresh-fetch-async
  "r" #'leetcode-reset-filter
  "q" #'quit-window)

(define-derived-mode leetcode-problems-mode tabulated-list-mode "Leetcode"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-refresh nil t))

(add-hook 'leetcode-problems-mode-hook #'hl-line-mode)

(defvar evil-normal-state-local-map)
(defun leetcode--set-evil-local-map (map)
  "Set `evil-normal-state-local-map' to MAP.
Should be added to mode hooks to enable evil bindings."
  (when (featurep 'evil)
    (define-key map "h" nil)
    (define-key map "v" nil)
    (define-key map "V" nil)
    (define-key map "b" nil)
    (define-key map "B" nil)
    (define-key map "g" nil)
    (define-key map "G" nil)
    (define-key map "z" #'leetcode-refresh)
    (define-key map "Z" #'leetcode-refresh-fetch)
    (setq evil-normal-state-local-map map)))


;;; Loading Mode

;;; Use spinner.el to show progress indicator
(defvar leetcode--spinner (spinner-create 'progress-bar-filled)
  "Progress indicator to show request progress.")

(defconst leetcode--loading-lighter
  '(" [LeetCode" (:eval (spinner-print leetcode--spinner)) "]"))

(define-minor-mode leetcode--loading-mode
  "Minor mode to showing leetcode loading status."
  :require 'leetcode
  :lighter leetcode--loading-lighter
  (if leetcode--loading-mode
      (spinner-start leetcode--spinner)
    (spinner-stop leetcode--spinner)))


;;; Leetcode Solving Modes

(defvar-keymap leetcode-base-mode-map
  :doc "Keymap shared in detail, test, result, and code buffers."
  "C-c C-t" #'leetcode-try
  "C-c C-s" #'leetcode-submit
  "C-c C-r" #'leetcode-restore-layout)

(define-derived-mode leetcode-base-mode fundamental-mode "Leetcode"
  "Major mode that leetcode buffers derive from."
  :abbrev-table nil)

;;; Solution Minor Mode

(defvar-keymap leetcode-solution-minor-mode-map
  :doc "Keymap for `leetcode-solution-minor-mode'."
  :parent leetcode-base-mode-map)

(define-minor-mode leetcode-solution-minor-mode
  "Minor mode to provide shortcut and hooks."
  :lighter " LcSolution"
  :require 'leetcode)

;;; Detail Mode

(defvar-keymap leetcode-detail-mode-map
  :doc "Keymap active in `leetcode-detail-mode'."
  :parent (make-composed-keymap leetcode-base-mode-map special-mode-map)
  "TAB"       #'forward-button
  "<backtab>" #'backward-button
  "l"         #'leetcode-set-prefer-language
  "q"         #'quit-window)

(define-derived-mode leetcode-detail-mode special-mode "LcDetail"
  "Major mode for displaying leetcode problem details."
  :abbrev-table nil)

(when (fboundp 'derived-mode-add-parents)
  (derived-mode-add-parents 'leetcode-detail-mode '(leetcode-base-mode)))

;;; Test Mode

(define-derived-mode leetcode-test-mode leetcode-base-mode "LcTest"
  "Major mode for displaying leetcode test cases."
  :abbrev-table nil)

;;; Results Mode

(defun leetcode--result-source (&rest _)
  "Return source buffer for compilation error."
  (when-let ((bufs (leetcode-problem-buffers nil nil t)))
    (list (buffer-file-name (leetcode-buffers-code bufs)))))

(defvar leetcode-compilation-error-regexp-alist-alist
  '((leetcode
     "Line \\([0-9]+\\): Char \\([0-9]+\\):\\(?: \\([^:]+\\):\\)?"
     (leetcode--result-source) 1 2 (1) 0 (3 compilation-error-face))
    (racket
     "solution.rkt:\\([0-9]+\\):\\([0-9]+\\)\\(?:: \\([^\n ]+\\)\\)?"
     (leetcode--result-source) 1 2 (2) nil (3 font-lock-function-name-face))
    (go
     "solution.go, line \\([0-9]+\\)"
     (leetcode--result-source) 1))
  "Compilation error regexps to links errors in `leetcode-result-mode'.
The source buffer is used instead of the filename component which can be nil.")

(defvar leetcode-result-mode-font-defaults
  `((,(rx bol (group (or "Your input" "Status" "Runtime" "Memory Usage"
                         "Output" "Expected" "Code output"))
          ":")
     (1 '(:inherit outline-1 :weight bold) t))
    (,(rx bol (group (or "panic")) ":")
     (1 font-lock-warning-face)))
  "Additional font-locking in `leetcode-result-mode'.")


(define-derived-mode leetcode-result-mode leetcode-base-mode "LcResults"
  "Major mode in leetcode results buffers."
  :abbrev-table nil
  (setq-local font-lock-defaults '(leetcode-result-mode-font-defaults nil nil))
  (setq-local compilation-error-regexp-alist-alist
              leetcode-compilation-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              (mapcar #'car leetcode-compilation-error-regexp-alist-alist))
  (compilation-minor-mode t))

(provide 'leetcode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; leetcode.el ends here
