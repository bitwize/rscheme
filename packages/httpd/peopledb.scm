
(define *people*
  (list (make <person>
	      full-name: "Donovan Kolbly"
	      first-name: "Donovan"
	      user-id: "donovan"
	      home-page-proc: (lambda ((who <person>) path query data)
				(home-page-handler who
						   path
						   query
						   data
						   "/u/donovan/www"
						   "Exp.html"))
	      gender: 'male)
	(make <person>
	      full-name: "Lane Kolbly"
	      first-name: "Lane"
	      user-id: "lane"
	      home-page-proc: no-home-page-handler
	      gender: 'male)))
