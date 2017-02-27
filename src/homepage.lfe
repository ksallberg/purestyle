(defmodule homepage
  (export all))

(include-lib "_build/default/lib/exemplar/include/html-macros.lfe")

(defun info (data parameters headers instancename)
  (html
   (list
    (head
     (list
       (title "purestyle.")
       (link '(rel "stylesheet" href "http://surf.suckless.org/pub/style.css"))))
    (body
     (main
      (list
        (img '(src "pstyle.png"))
        (div '(class "dynamic content")
             (list
              (h1 "Hello there! Welcome to my LFE and Exemplar powered homepage!")
              (h2 (++ "Yes " "hello"))
              (div (p (++ "<a href='https://play.purestyle.se'>"
                          "purestyle. play</a>")))))))))))
