(defmodule homepage
  (export all))

(include-lib "_build/default/lib/exemplar/include/html-macros.lfe")

(defun info (data parameters headers instancename)
  (html
   (list
    (head
     (title "purestyle."))
    (body
     (main
      (div '(class "dynamic content")
           (list
            (h1 "Hello there! Welcome to my LFE and Exemplar powered homepage!")
            (h2 (++ "Yes " "hello"))
            (div (p (++ "<a href='https://play.purestyle.se'>"
                        "purestyle. play</a>"))))))))))
