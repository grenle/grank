;; -*- lexical-binding: t; -*-

(require 'grank)
(require 'ert)

(ert-deftest init-grank-link-info-test ()
  ;; init a link-info struct
  (should
   (init-grank-link-info :url "http://www.example.com/"))
  ;; make a link-info struct
  (should
   (make-grank-link-info
    :url "http://www.example.com/"
    :title "disentitle"))
  ;; with init, the url should be stored
  (should
   (equal
    (grank-link-info-url
     (init-grank-link-info :url "http://www.example.com/"))
    "http://www.example.com/"))
  ;; with init, the title should be the url
  (should
   (equal
    (grank-link-info-title
     (init-grank-link-info :url "http://www.example.com/"))
    "http://www.example.com/"))
  ;; with init, the parsed url should be created
  (should
   (equal
    (url-host
     (grank-link-info-parsed-url
      (init-grank-link-info :url "http://www.example.com/")))
    "www.example.com")))

(ert-deftest grank--*default-mode-handle-test ()
  (should
   (equal
    (grank--*default-mode-handler
     'org-mode
     (make-grank-link-info
      :url "http://www.example.com/"
      :title "just an example"))
    "[[http://www.example.com/][just an example]]"))
    (should
   (equal
    (grank--*default-mode-handler
     'html-mode
     (make-grank-link-info
      :url "http://www.example.com/"
      :title "another title"))
    "<a href=\"http://www.example.com/\">another title</a>")))

(ert-deftest grank--default-mode-handle-test ()
  (should
   (equal
    (with-temp-buffer
      (org-mode)
      (insert
       (grank--default-mode-handler
        (make-grank-link-info
         :url "http://www.example.com/"
         :title "just an example")))
      (buffer-string))
    "[[http://www.example.com/][just an example]]")))

(ert-deftest grank--obtain-info-test ()
  (let* ((example-url "http://www.example.com/")
         (example-res (grank--obtain-info example-url)))
    (should
     (equal
      (grank-link-info-title example-res)
      "http://www.example.com/"))))
    
