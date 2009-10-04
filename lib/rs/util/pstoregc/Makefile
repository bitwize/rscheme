
MAKEX=../../../makex
FSHELL=fshell
RSC=rsc
INSTALL_DIR := $(shell ${FSHELL} --install)

SCM_FILES=scanmgr.scm simple.scm
MODULE_FILE=module.scm
GC_SCM_FILES=gcmain.scm state.scm

MIF_PRODUCT=rs_util_pstoregc.mif
GC_INST=${INSTALL_DIR}/bin/gcmain
MIF_INST=${INSTALL_DIR}/resource/modules/rs_util_pstoregc.mif

all: ${MIF_INST} ${GC_INST}

${MIF_INST}: ${MIF_PRODUCT}
	echo "<${INSTALL_DIR}>"
	cp -p ${MIF_PRODUCT} ${MIF_INST}
	cp -p ${MIF_PRODUCT:.mif=.mx} ${MIF_INST:.mif=.mx}

${MIF_PRODUCT}: ${SCM_FILES} ${MODULE_FILE}
	${RSC} -p ${MODULE_FILE}

$(GC_INST): gcmain
	cp -p gcmain $(GC_INST)

gcmain: ${GC_SCM_FILES}
	${FSHELL} gcmain.scm -c $@
	${MAKEX} $@ `${FSHELL} --install`/bin/${FSHELL}
	chmod +x $@

clean::
	rm -f ${MIF_PRODUCT} ${MIF_PRODUCT:.mif=.mx} ${MIF_PRODUCT:.mif=.doc}
