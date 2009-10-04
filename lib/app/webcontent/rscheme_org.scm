(store-template
 "*common"
 "sxml/html"
 '(title content trailer)
 '(html
   (head
    (link (@ (rel "stylesheet")
             (type "text/css")
             (href "/style.css")))
    (title ,title))
   (body
    (div (@ (id "nav"))
         (img (@ (src "/rscheme.png")
                 (border "0")))
         (h1 ,title))
    (div (@ (id "main")) ,@content)
    ,trailer)))

(store-template
 "*trailer"
 "sxml/node-list"
 '(node)
 '(div (@ (id "trailer"))
       "Last modified "
       (i ,(time->string (timestamp (modification-audit node))))
       " by "
       (i ,(name (principal (modification-audit node))))))

(import-text-file "style.css" "text/css" "style.css")

(store-template
 "*faq"
 "sxml/webnode"
 '(question answer)
 '(webnode
   (@ (id "*faq"))
   (content
    (title "FAQ: " ,(xpath:node-set->string question))
    (content
     (h2 "Q: " ,@question)
     (div (@ (class "answer"))
          ,@answer)))))

(store-template
 "welcome"
 "sxml/webnode"
 '()
 '(webnode
   (@ (id "welcome"))
   (content
    (title "RScheme: An Object-Oriented Reflective Scheme System")
    (content
     (div
      (@ (class "notice"))
      (h2 "Note")
      (p
       "As of September 29, 2004, this web site\n"
       "is undergoing a significant revamping.  Some content\n"
       "that was previously available here is not (yet) available.  If you\n"
       "are looking for anything in particular, please "
       (a (@ (href "/site-request")) "let the webmaster know")
       "\nand we will prioritize getting it back online.\n"))
     (h2 "Introduction")
     (p
      "RScheme is an object-oriented, extended version of the\n"
      "Scheme dialect of Lisp.  RScheme is freely\n"
      "redistributable, and offers reasonable performance\n"
      "despite being quite portable.  For performance-critical applications,\n"
      "RScheme code can be\n"
      "compiled to C, and the C can then compiled with a normal\n"
      "C compiler to generate native machine code.  By default,\n"
      "however, RScheme compiles to bytecodes which are\n"
      "interpreted by a (runtime) virtual machine. This ensures\n"
      "that compilation is fast and keeps code size down.\n")

     (p
      "Additionally, RScheme comes with many libraries that are useful\n"
      "for developing full-scale real-world applications.\n")

     (p "The RScheme Development Group (" (code "rscheme.org") ")\n"
        "is dedicated to\n"
        "the development, dissemination, and coordination of efforts\n"
        "related to the RScheme language and implementation.\n")
     
     (p "This is the official RScheme web site. Notes and documents on\n"
        "RScheme are published here, as well as information about the\n"
        "current status of the project, such as it is.\n")
     
     (h2 "Documentation")
     (p "The documentation for RScheme uses the DocBook DTD\n"
        "["
        (a (@ (href "http://www.oasis-open.org/docbook/")) "Davenport Group")
        "]"
        ", and various tools are used to produce an online\n"
        "version as well as print versions.")
     ;;
     (h2 "Community")
     (p 
      "The RScheme community is maintained by means of a mailing list,\n"
      (code "list@rscheme.org")
      ".  An archive of this mailing list is available\n"
      (a (@ (href "/mlist/index")) "here")
      ", and submissions are restricted to members.  To sign up, fill\n"
      "out "
      (a (@ (href "/mlist/signup")) "this form")
      ".")
     ;;
     (h2 "FAQs")
     (p "Here are a few of the most recently added FAQs:")
     (table
      (tr
       (th  "Date")
       (th  "Author")
       (th  "Question"))
      ,@(recent-faqs))
     ;;
     (h2 "Releases")
     (p "Current releases are summarized in the following table:\n")
     ;;
     (table
      (tr
       (th "Major")
       (th "Minor")
       (th "Date")
       (th "Download")
       (th "Notes"))
      ,@(current-releases))
     (p "For historical purposes, a listing of all past releases\n"
        "is available "
        (a (@ (href "all")) "here")
        ".")
     ;;
     (h2 "Online Software")
     (p "This server is running an instance of the "
        (code "app.webcontent")
        " module.\n"
        "Furthermore, the source code and defect list for RScheme\n"
        "is maintained\n"
        "by " (b "Axis") ", an application developed principally in RScheme\n"
        "by "
        (a (@ (href "http://www.xynthesis.com/")) "Xynthesis LLC")
        ".\n"
        "Anyone may browse the online source code and defect database\n"
        (a (@ (href "http://axis1.westgate.xynthesis.com:7980/"))
           "here")
        ".\n"
        "To report or comment on a defect, you must have an account on\n"
        "the RScheme source repository.  Fill out a request for an\n"
        "account "
        (a (@ (href "account-request")) "here")
        ".\n")
     ))))

(store
 '(release
   (@ (id "rs-0.7.3.3-b20"))
   (date "2004-09-22")
   (file "rs-0.7.3.3-b20.tar.gz")
   (src "/u/rscheme/product/dist/rs-0.7.3.3-b20.tar.gz")
   (size "2410931")
   (build "b20")
   (minor "3.3")
   (major "0.7")))

(store
 '(release
   (@ (id "rs-0.7.3.2"))
   (date "2004-09-22")
   (file "rs-0.7.3.2.tar.gz")
   (src "/u/rscheme/product/dist/rs-0.7.3.2.tar.gz")
   (size "1907741")
   (build "final")
   (minor "3.2")
   (major "0.7")))

(store
 '(directory
   (@ (id "current-releases"))
   (list
    (item "rs-0.7.3.3-b20")
    (item "rs-0.7.3.2"))))
    
(store
 '(relnote
   (@ (id "rs-0.7.3.3-b20"))
   (content
    (p "This is a fairly stable release, although it has\n"
       "some build problems on AIX.\n"))))

(store
 '(directory
   (@ (id "recent-faqs"))
   (list
    (item "compat.std.r4rs"))))
   
(store
 '(faq
   (@ (id "compat.std.r4rs"))
   (content
    (question
     "Is RScheme compliant with " (span "R" (sup "4") "RS") "?")
    (answer
     (p
      "RScheme is nearly 100% compatible with "
      (span "R" (sup "4") "RS")
      ".  Here are all the known exceptions:")
     (ul
      (li (b "Symbol case sensitivity.  ")
          "Although RScheme " (i "can") " be built with case-insensitive\n"
          "symbols, this is not the default, and is not a generally\n"
          "maintained feature, since about version 0.6.")
      (li (b "Default environment.  ")
          "RScheme's default environment includes many bindings that\n"
          "are not part of " (span "R" (sup "4") "RS") ", and some bindings\n"
          "that " (i "are") " part of the standard are defined differently.\n"
          "For example, " (code "car") " refers to an immutable primop\n"
          "binding instead of a plain top-level variable with a procedure\n"
          "value.  If the user wants a strict environment, RScheme\n"
          "should be started with the "
          (code "--in r4rs")
          " flag.")
      (li (b (code "transcript-on") "/" (code "transcript-off") ".  ")
          "The transcript features defined in "
          (span "R" (sup "4") "RS")
          " are not available in RScheme."))))))

(import-image "rscheme.png"
              "image/png"
              "images/rscheme.png")

(store
 '(webnode
   (@ (id "request.p"))
   (content
    (title "Thanks!")
    (content
     (h2 "Thanks")))))

(store
 '(webnode
   (@ (id "site-request"))
   (content
    (title "RScheme: Web Site Request")
    (content
     (h2 "Form to Request Web Site Maintenance")
     (p
      "Use this form to request maintenance of the RScheme web site,\n"
      "including reporting web site problems, content errors,\n"
      "factual omissions, " (i "etc.") ".\n")
     (form
      (@ (method "POST")
         (action "webreq.p"))
      ;;
      (table
       (@ (class "form"))
       (tr
        (th "Your Full Name:")
        (td (input (@ (name "fullname") (type "text") (size "24")))))
       (tr
        (th "Your Email Address:")
        (td (input (@ (name "email") (type "text") (size "24")))))
       (tr
        (th "Request Subject:")
        (td (input (@ (name "subject") (type "text") (size "50")))))
       (tr
        (th "Request Description:")
        (td (textarea (@ (name "comment") (cols "40") (rows "10"))
                      "")))
       (tr
        (th "")
        (td (input (@ (type "submit")
                      (value "Submit Request")))))))))))
     

(store
 '(webnode
   (@ (id "account-request"))
   (content
    (title "RScheme: Source Code Repository: Account Request")
    (content
     (h2 "Form to Request an RScheme Source Code Repository Account")
     (p
      "Use this form to request a new account for update access to the\n"
      "RScheme source code repository.\n")
     (form
      (@ (method "POST")
         (action "request.p"))
      ;;
      (table
       (@ (class "form"))
       (tr
        (th "User Name Preference:")
        (td (input (@ (name "login") (type "text") (size "20")))))
       (tr
        (th "Given (First) Name:")
        (td (input (@ (name "firstname") (type "text") (size "20")))))
       (tr
        (th "Family Name (Surname):")
        (td (input (@ (name "lastname") (type "text") (size "20")))))
       (tr
        (th "Email Address:")
        (td (input (@ (name "email") (type "text") (size "40")))))
       (tr)
       (tr
        (th "Locality:")
        (td "City:"
            (input (@ (name "city") (type "text") (size "20")))
            "State: "
            (input (@ (name "state") (type "text") (size "10")))))
       (tr
        (th "Country:")
        (td (input (@ (name "country") 
                      (type "text") (size "20") (value "USA")))))
       (tr
        (th "Requested Access Level:")
        (td (select
             (@ (name "access"))
             (option (@ (value "3")) "3: Component Lead")
             (option (@ (value "2")) "2: Source Code Modification")
             (option (@ (value "1")) "1: Defect Creation/Modification"))))
       (tr
        (td (@ (colspan "2"))
            (input (@ (type "submit")
                      (value "Submit Request")))))
       ))
     (p
      "After submitting this form, a confirmation email will\n"
      "be sent to the email address you provide containing an\n"
      "activation link that should be used to actually create your\n"
      "account.\n")))))
