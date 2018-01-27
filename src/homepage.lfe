(defmodule homepage
  (export all))

(include-lib "_build/default/lib/exemplar/include/html-macros.lfe")

(defun href (url txt)
  (++ "<a href='" (++ (++ url "'>") (++ txt "</a>"))))

(defun linkedin ()
  (href "https://www.linkedin.com/in/kristian-s%C3%A4llberg-22a7ba18"
        "linkedin"))

(defun pplay ()
  (list (href "https://play.purestyle.se" "Musiklistan")
        " is a web app that lets you collect "
        (href "https://www.youtube.com" "youtube")
        " & "
        (href "https://www.soundcloud.com" "soundcloud")
        " links, and listen through these links. "
        ))

(defun subheader ()
  (list "Welcome to my "
        (href "https://www.erlang.org" "Erlang/OTP")
        ", "
        (href "https://github.com/ksallberg/brunhilde" "brunhilde")
        ", "
        (href "http://lfe.io" "LFE")
        " & "
        (href "https://github.com/lfex/exemplar" "exemplar")
        " powered homepage!"))

(defun bread ()
  (list "My name is Kristian, I like building stuff. I used to be a "
        "web designer, then a flash developer. Then a student of "
        "computer science. Now I am employed as a Software Engineer at "
        (href "https://www.cisco.com" "Cisco Systems, Inc.") " "
        "Usually I find myself using "
        (href "http://www.erlang.org" "Erlang/OTP")
        " or "
        (href "https://www.python.org" "python") ". "
        "In my spare time I also like exploring "
        (href "https://www.haskell.org" "Haskell") ". "
        "If you are interested, please browse my "
        (href "https://github.com/ksallberg" "github")
        " or my "
        (linkedin) "."
        ))

(defun info (data parameters headers instancename)
  (html
   (list
    (head
     (list
       (title "pure style.")
       (script '(src "waves.js"))
       (link '(rel "stylesheet" href "/style.css"))
       (link
        '(rel "stylesheet"
              href "https://fonts.googleapis.com/css?family=Droid+Sans:700"))
       (link '(rel "stylesheet"
                   href "https://fonts.googleapis.com/css?family=Open+Sans"))
       (link '(rel "stylesheet"
                   href "https://fonts.googleapis.com/css?family=Droid+Serif"))
       ))
    (body
     (main
      (list
       "<canvas id='waver' width='200' height='400'></canvas>"
       (div '(id "allt")
         (list
           (img '(src "pstyle.png"))
           (div '(class "dynamic content")
              (list
               (div '(class "title") "hej")
               (div '(class "mini") '("Here is the list of the things I like,"
                                      " this list is the list of the things "
                                      "I like:"))
               (div '(class "sub-title") (subheader))
               (div '(class "bread") (bread))
               (p (pplay))

              )
         )))
       )))
    )))
