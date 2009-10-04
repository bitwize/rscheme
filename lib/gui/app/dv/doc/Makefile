
CAT=proc/catalog
DSL=proc/modularwrap.dsl
DECL=proc/decl.sgml

ID=dv

valid::
	onsgmls -sv -c ${CAT} ${ID}.sgml

tex::
	openjade -c ${CAT} -t tex -iprint -d ${DSL}#print ${DECL} ${ID}.sgml

dvi::
	jadetex ${ID}.tex

ps::
	dvips -o${ID}.ps ${ID}.dvi

clean::
	rm -f ${ID}.fot ${ID}.tex ${ID}.log ${ID}.ps ${ID}.aux ${ID}.dvi
