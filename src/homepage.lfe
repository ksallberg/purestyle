(defmodule homepage
  (export all))

(include-lib "_build/default/lib/exemplar/include/html-macros.lfe")

(defun href (url txt)
  (++ "<a href='" (++ (++ url "'>") (++ txt "</a>"))))

(defun info (data parameters headers instancename)
  (html
   (list
    (head
     (list
       (title "pure style.")
       (link '(rel "stylesheet" href "/style.css"))
       (link '(rel "stylesheet" href "https://fonts.googleapis.com/css?family=Droid+Sans:700"))
       (link '(rel "stylesheet" href "https://fonts.googleapis.com/css?family=Open+Sans"))
       (link '(rel "stylesheet" href "https://fonts.googleapis.com/css?family=Droid+Serif"))
       )
     )
    (body
     (main
      (list
       (img '(src "pstyle.png"))
       (div '(class "dynamic content")
            (list
             (div '(class "section-title-fonts") "Hello there!")
             (div '(class "sub-title") "Welcome to my LFE and Exemplar powered homepage!")
             (div '(class "bread") "Apparently we had reached a great height in the atmosphere, for the sky was a dead black, and the stars had ceased to twinkle. By the same illusion which lifts the horizon of the sea to the level of the spectator on a hillside, the sable cloud beneath was dished out, and the car seemed to float in the middle of an immense dark sphere, whose upper half was strewn with silver. Looking down into the dark gulf below, I could see a ruddy light streaming through a rift in the clouds.")
             (p (href "https://play.purestyle.se" "purestyle. play"))
             (p (href "http://www.erlang.org" "Erlang/OTP"))
             (p (href "https://www.haskell.org" "Haskell"))
             (p (href "https://github.com/ksallberg/brunhilde" "brunhilde"))
             (p (href "https://www.linkedin.com/in/kristian-s%C3%A4llberg-22a7ba18" "linkedin"))
             (p (href "https://www.cisco.com" "Cisco Systems, Inc."))
            )
          )
       )
      )
     )
    )
   )
  )
