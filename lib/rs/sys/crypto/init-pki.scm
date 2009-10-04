;;;
;;;   Root certificate for test realm
;;;

(cert-db-insert!
 (dsa-sign-xml
  *testkey*
  `(Certificate
    ;;
    (Subject
     (@ (Id "47abf2de-d3b3-46eb-81c3-91df1c4af2d0"))
     (Name
      (CommonName "Bob the Builder")
      (Organization "RScheme Development Group"))
     ,(to-sxml (public *testkey*)))
    ;;
    (Issuer
     (@ (Id "47abf2de-d3b3-46eb-81c3-91df1c4af2d0"))
     (Name
      (CommonName "Bob the Builder")
      (Organization "RScheme Development Group"))
     ;;
     (InlineSignature)))))

;;;
;;;   A certificate assigned to Donovan
;;;

(define *dmkkey* (make-dsa-key-pair name: "d6e88338-0c8e-47ba-8ffe-e6ca345efb8e"))

(cert-db-insert!
 (dsa-sign-xml
  *testkey*
  `(Certificate
    ;;
    (Subject
     (@ (Id "3ac6fb7c-a84d-4a57-954f-882be73579e1"))
     (Name
      (CommonName "Donovan Kolbly")
      (Organization "RScheme Development Group"))
     ,(to-sxml (public *dmkkey*)))
    ;;
    (Issuer
     (@ (Id "47abf2de-d3b3-46eb-81c3-91df1c4af2d0"))
     (Name
      (CommonName "Bob the Builder")
      (Organization "RScheme Development Group"))
     ;;
     (InlineSignature)))))

;;;
;;;   A certificate for servers that Donovan manages
;;;

(define *srvkey* (make-dsa-key-pair name: "033f88ab-e464-43d5-8227-fc27f9b6bd8e"))

(cert-db-insert!
 (dsa-sign-xml
  *dmkkey*
  `(Certificate
    ;;
    (Subject
     (@ (Id "b426cc10-1426-47df-8f30-0c04663684d7"))
     (Name
      (CommonName "star.westgate.xynthesis.com"))
     ,(to-sxml (public *srvkey*)))
     ;;
    (Issuer
     (@ (Id "3ac6fb7c-a84d-4a57-954f-882be73579e1"))
     (Name
      (CommonName "Donovan Kolbly")
      (Organization "RScheme Development Group"))
     ;;
     (InlineSignature)))))
