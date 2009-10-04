INSTALLDIR=`rs --install`
PRODUCT=html
SCMFILES=html.scm htmlport.scm

${PRODUCT}.mif ${PRODUCT}.mx: ${SCMFILES}
	${INSTALLDIR}/bin/rsc -p ${PRODUCT}.mcf

install::
	cp -p ${PRODUCT}.mif ${PRODUCT}.mx ${INSTALLDIR}/resource/modules

clean::
	rm -f html.mif html.mx html.doc

