;;; grank.el --- Mode aware URL yanking -*- lexical-binding: t; -*-

;;; Commentary:
;;; Please see src/README.org for details

;;; Code:

(require 'url)
(require 'cl-lib)

(defvar grank-url-dispatch-table nil)
(defvar grank-mode-dispatch-table nil)

(defun grank--*get-url-handler (handlers url)
  "Find the handler for URL in HANDLERS.
Return nil if none found."
  (cl-loop for pair in handlers
           when (string-match (car pair) url)
           return (cdr pair)))

(defun grank--get-url-handler (url)
  "Find the handler for URL or default if none found."
  (or
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

(defun grank--default-url-handler (link-info)
  "Create basic LINK-INFO."
  (let ((link-info-url (grank-link-info-url link-info)))
    (init-grank-link-info
     :url link-info-url)))

(defun grank--*default-mode-handler (mode link-info &optional style)
  "Create basic string from LINK-INFO depending on MODE.
The STYLE parameter changes the output.  Unused as of now."
  (let ((url (grank-link-info-url link-info))
        (title (grank-link-info-title link-info)))
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
The STYLE parameter changes the output.  Unused as of now."
  (grank--*default-mode-handler major-mode link-info style))

(defun grank--obtain-info (url)
  "Call the appropriate url-handler on URL."
  (let* ((link-info (init-grank-link-info :url url))
         (handler
          (grank--get-url-handler url)))
    (or
     (funcall handler link-info)
     (grank--default-url-handler link-info))))

(defun grank--process-info (link-info &optional style)
  "Format LINK-INFO according to major mode and STYLE.
If the major mode has en entry in the alist
`grank-mode-dispatch-table', the formating is done by the
associated procedure falling back on
`grank--default-mode-handler' otherwise."
  (let ((handler
         (alist-get
          major-mode
          grank-mode-dispatch-table
          #'grank--default-mode-handler)))
    (or
     (funcall handler link-info)
     (grank--default-mode-handler link-info))))

(defun grank-url-to-link (url)
  "Create a mode aware link for URL.
Will use the current major mode"
  (grank--process-info
   (grank--obtain-info url)))

(defun grank ()
  "Pretty-yank the url in clipboard."
  (interactive)
  (let ((url (current-kill 0 t)))
    (insert (grank-url-to-link url))))


(provide 'grank)
;;; grank.el ends here