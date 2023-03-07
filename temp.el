;; -*- lexical-binding: t; -*-

(require 'url)
(require 'cl-lib)

;; inclusion of cl-lib seems ok because it is used by the
;; url package.

;; <package-name>-*<function-name> (mypackage-*my-function) a version of the function called by a "cleaner" version.
;; for example: the seed of a recursive function, replacing *'s nil output with a default or the passing of a variable the "normal" function takes from the state.

;;(grank--get-url-handler "http://www.wikipedia.com")
;;(grank--get-url-handler "http://www.wikipedia.co")

;; set the default in / out from marked, stared or dashed
;; variant, allow the user to override but return to
;; originals at any time.
(defvar grank-url-dispatch-table nil)
(defvar grank-mode-dispatch-table nil)

(defun grank--*get-url-handler (handlers url)
  "Find the handler for URL."
  (cl-loop for pair in handlers
           when (string-match (car pair) url)
           return (cdr pair)))

(defun grank--get-url-handler (url)
  (grutils-non-nil-x-or-y
   (grank--*get-url-handler grank-url-dispatch-table url)
   #'grank--default-url-handler))

(cl-defstruct
    (grank-link-info
     (:constructor nil)
     (:constructor
      init-grank-link-info
      (&key (url)
       &aux (parsed-url (url-generic-parse-url url))
            (title url)))
     (:constructor
      make-grank-link-info
      (&key (url nil)
            (title nil)
            (synopsis nil))
      (&aux (parsed-url (url-generic-parse-url url)))))
  url parsed-url title synopsis)

;; with the addition of title = url from grank-link-info
;; init constructor, we no longer need much of this. It
;; could be the identity function.
(defun grank--default-url-handler (link-info)
  "Create basic link-info from link-info."
  (let ((link-info-url (grank-link-info-url link-info)))
    (init-grank-link-info
     :url link-info-url)))

(defun grank--*default-mode-handler (mode link-info &optional style)
  "Create basic string from LINK-INFO depending on MODE.
The STYLE parameter changes the output. Unused as of now."
  (let ((url (grank-link-info-url link-info))
        (title (grank-link-info-title link-info))
        (synopsis (grank-link-info-synopsis link-info)))
    (pcase mode
      ('org-mode
       (pcase style
         (_ (format "[[%s][%s]]" url title))))
      ('html-mode
       (pcase style
         (_ (format "<a href=\"%s\">%s</a>" url title))))
      ('markdown-mode
       (pcase style
         (_ (format "[%s](%s)" url title))))
      (_ url))))

(defun grank--default-mode-handler (link-info &optional style)
  "Create basic string from LINK-INFO.
The STYLE parameter changes the output. Unused as of now."
  (grank--*default-mode-handler major-mode link-info style))

(defun grank--obtain-info (url)
  "Calls the appropriate url-handler on URL."
  (let* ((link-info (init-grank-link-info :url url))
         (parsed-url (grank-link-info-parsed-url link-info))
         (host (url-host parsed-url))
         (handler
          (grank--get-url-handler url)))
    (funcall handler link-info)))

(defun grank--process-info (link-info &optional style)
  "Format LINK-INFO struct for the current major mode.
If the major mode has en entry in the alist
`grunk-mode-dispatch-table', the formating is done by the
associated procedure falling back on
`grank--default-mode-handler' otherwise".
  (let ((handler
         (alist-get
          major-mode
          grunk-mode-dispatch-table
          #'grank--default-mode-handler)))
    (funcall handler link-info)))

(defun grank-url-to-link (url)
  "Create a mode aware link for URL.
Will use the current major mode"
  (grank--process-info
   (grank--obtain-info url)))

(defun grank ()
  (interactive)
  (let ((url (current-kill 0 t)))
    (insert (grank-url-to-link url))))

;; not in the final el file, in org as a suggestion.
;; (global-set-key (kbd "C-c y") 'grank)

;; (grank-url-to-link "http://www.example.com")
;; (grank--process-info (grank--obtain-info "http://www.example.com"))

(defun grank--wk-url-handler (link-info)
  "Create basic link-info from link-info."
  (let* ((url (grank-link-info-url link-info))
         (parsed-url (grank-link-info-parsed-url link-info))
         (filename (url-filename parsed-url)))
    (make-grank-link-info
     :url url
     :title filename)))

(defun racpam (x &rest functions)
  "One data, X, many FUNCTIONS.
(racpam x f g h) → (list (f x) (g x) (h x))"
  (mapcar (lambda (f) (funcall f x)) functions))

;; (shell-command "ls ~/bin" (current-buffer) (current-buffer))

(defun grank--get-page-response (url)
  "Get the response to get URL as a string.
This string includes the headers and the body of the
response."
  (let ((res nil)
        (res-buffer
         (url-retrieve-synchronously url)))
    (with-current-buffer res-buffer
      (setq res (buffer-string)))
    (kill-buffer res-buffer)
    res))

(defun grank--get-page-body (url)
  "Get the body of the response to get URL as a string.
This only includes the body or /payload/ of the response.
Nil is return if an error is encountered at any point. The
cause of the error will be explained in the *Messages*
buffer."
  (condition-case e
      (let* ((response (grank--get-page-response url))
             (start-at (string-match "\n\n" response)))
        (if start-at
            (substring response start-at)
          nil))
    (error
     (progn
       (message "%s" e)
       nil))))

(defun grank--f-on-resource-as-file (f url)
  "Apply F to saved resource at URL as file.
- retrive the resource pointed to by URL
- save it as a temporary file
- execute F with the temporary file as argument
- delete buffer and file
- return (F temp-file) result"
  (let ((res nil)
        (errors nil)
        (temp-file (make-temp-file "grank"))
        (res-buffer (url-retrieve-synchronously url)))
    (with-current-buffer res-buffer
      (write-file temp-file))
    (kill-buffer res-buffer)
    (let ((f-res (funcall f temp-file)))
      (delete-file temp-file)
      f-res)))

;; We need the title and host. The host is necessary because
;; querying the API with the wrong subdomain will not
;; work. For example, querying for the title حمص بطحينة
;; (hummus) with the =en= subdomain will only return the
;; normalised title.

(defun grank--wk-url-to-host-and-title (url)
  "Simple URL handler for Wikipedia"
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (path (url-filename parsed-url))
         (title (save-match-data
                  (and (string-match "/wiki/\\(.*\\)/?" path)
                       (match-string 1 path)))))
    (if (and host title)
        (cons host title)
      nil)))

(defun grutils-sym-nature (sym)
  "Nature of thing denoted by SYM.
The answer is a symbol (package, variable, function) or
nil if SYM does not exist."
  (cond ((featurep sym) 'package)
        ((boundp sym) 'variable)
        ((fboundp sym) 'function)
        (t nil)))


;; (add-to-list LIST-VAR ELEMENT &optional APPEND COMPARE-FN)

;;(grank--wk-url-to-host-and-title "http://www.example.com/wiki/title")

(defvar grank--wk-query-template
  "https://%s/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=%s")

(defun grank--wk-query-with-title (host-and-title)
  (pcase-let ((`(,host . ,title) host-and-title))
    (format grank--wk-query-template host title)))

;; investigate url-file function

;; https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=Stack%20Overflow
;; https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=Category:Chickpea_dishes
;; https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=%D8%AD%D9%85%D8%B5_%D8%A8%D8%B7%D8%AD%D9%8A%D9%86%D8%A9

;; youtube xpath
;; video page
;; name/title
;; "string(//meta[@itemprop='name']/@content)"
;; channel id
;; "string(//meta[@itemprop='channelId']/@content)"

(defun maybe-doodoodoo (maybe-thing)
  (pcase maybe-thing
    ('nil "really not")
    (`(t . ,thing) (format "definitely a thing: %s" thing))))

(maybe-doodoodoo '(t . 3))
(maybe-doodoodoo nil)

(defun grank--xmllint-message-error (code)
  (let ((msgs '((0 . "No error")
                (1 . "Unclassified")
                (2 . "Error in DTD")
                (3 . "Validation error")
                (4 . "CtxtReadFile error")
                (5 . "Schema compilation")
                (6 . "Error writing output")
                (7 . "Error in schema pattern")
                (8 . "Error in Reader registration")
                (9 . "Out of memory error")
                (10 . "XPath evaluation error"))))
    (message
     "xmllint gave error: %s"
     (alist-get code msgs "unknown error"))))

(defun grank--xpath-on-file (expression)
  "Evaluate xpath EXPRESSION against a file.
Requires xmllint installed and on $PATH."
  (lambda (filename)
    "Evaluates xpath EXPRESSION against FILENAME."
    (with-temp-buffer
      (call-process
       "xmllint"
       filename
       (list (current-buffer) nil)
       nil
       "--html" "--xpath" expression "2>/dev/null" "-")
      (buffer-string))))

(defun grank--xpath-on-file (expression)
  "Evaluate xpath EXPRESSION against a file.
Requires xmllint installed and on $PATH."
  (lambda (filename)
    "Evaluates xpath EXPRESSION against FILENAME."
    (with-temp-buffer
      (call-process
       "xmllint"
       filename
       (list (current-buffer) nil)
       nil
       "--html" "--xpath" expression "2>/dev/null" "-")
      (buffer-string))))


;;(string-chop-newline
;; (grank--f-on-resource-as-file
;;  (grank--xpath-on-file "string(//title)")
;;  "http://www.example.com/"))


;; option requires (t . response), including (t . nil)
;; because nil is a reasonable denotation for "none", as in
;; "no number in the list are odd" for example. This can
;; indeed be used by another list consuming function.

;; from youtube video page, the meta channelId can be used
;; to obtain the channel page url with the pattern
;; https://www.youtube.com/channel/<channelId>
;;
;; on that page, the meta named "description" contains then
;; channel's description.

;; The program’s input comes from file INFILE (nil means ‘null-device’).
;; If INFILE is a relative path, it will be looked for relative to the
;; directory where the process is run (see below).  If you want to make the
;; input come from an Emacs buffer, use ‘call-process-region’ instead.
;; (call-process
;;  "xmllint"
;;  "/home/gregory/Desktop/fakeif/data/single-page/index.html"
;;  (list (current-buffer) nil)
;;  nil
;;  "--html" "--xpath" "//title" "2>/dev/null" "-")

;;(cl-defstruct teststructa "yes" foo bar baz)
;;(defvar teststructx (make-teststructa :foo "too" :bar "titi" :baz "tata"))
;;(type-of teststructx)
;;(mapcar #'car (cdr (cl-struct-slot-info 'teststructa)))

;;(grank-f-on-temp-file "https://en.wikipedia.org/wiki/Persimmon")
;; (call-process "ls" nil nil nil "/home/gregory/bin")
;; (call-process "ls" nil nil nil "/home/gregory/bin")

;; (defvar cp-testfile "/home/gregory/Desktop/fakeif/data/single-page/index.html")
;; (call-process
;;  "xmllint"
;;  nil
;;  (list (current-buffer) nil)
;;  nil
;;  "--html" "--xpath" "//title" "2>/dev/null" cp-testfile)

;; this works
;;(call-process
;; "ls" nil (list (current-buffer) nil) nil "-la" "/home/gregory/Desktop/fakeif")

;; for yt and others, a temp file can allow the use of xmllint

;;(grank--wk-url-handler
;; (init-grank-link-info :url "https://en.wikipedia.org/wiki/Persimmon"))

;; there is not `grutils-none', nil is adequate and possibly
;; more Lisp-y

(defun grutils-none (why) (cons 'none why))
(defun grutils-some (res) (cons 'some res))

(defun grutils-none-p (opt)
  (and (consp opt)
       (eq (car opt) 'none)))

(defun grutils-some-p (opt)
  (and (consp opt)
       (eq (car opt) 'some)))

(defun grutils-pipe-break-on-nil (functions x)
  (if (null functions)
      x
    (let ((f (car functions)))
      (if (functionp f)
          (let ((result (funcall f x)))
            (if (grutils-some-p result)
                (grutils-pipe-break-on-nil (cdr functions) (cdr result))
              result))
        (error "grutils: not function %s %s"
               (type-of f)
               f)))))

;;(defun k-some-one (x) (grutils-some 1))
;;(defun k-some-two (x) (grutils-some 2))
;;(grutils-pipe-break-on-nil '() 3)
;;(grutils-pipe-break-on-nil (list #'grutils-none) 3)
;;(grutils-pipe-break-on-nil (list #'k-some-one) 3)
;;(grutils-pipe-break-on-nil (list #'k-some-one #'grutils-none) 3)
;;(grutils-pipe-break-on-nil (list #'grutils-none #'k-some-one) 3)
;;(grutils-pipe-break-on-nil (list #'k-some-two #'k-some-one) 3)
;;(grutils-pipe-break-on-nil (list #'k-some-one #'k-some-two) 3)
;; (if (require 'grutils nil t) "yes" "no")
;; (featurep 'grank)

(provide 'grank)
