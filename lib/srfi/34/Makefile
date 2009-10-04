MAKEX=../../makex
FSHELL=fshell
RSC=rsc
INSTALL_DIR := $(shell ${FSHELL} --install)

SCM_FILES=srfi-34.scm
MODULE_FILE=module.scm

MIF_PRODUCT=srfi_34.mif
MIF_INST=${INSTALL_DIR}/resource/modules/${MIF_PRODUCT}

all: ${MIF_INST} ${GC_PRODUCT}

${MIF_INST}: ${MIF_PRODUCT}
	echo "<${INSTALL_DIR}>"
	cp -p ${MIF_PRODUCT} ${MIF_INST}
	cp -p ${MIF_PRODUCT:.mif=.mx} ${MIF_INST:.mif=.mx}

${MIF_PRODUCT}: ${SCM_FILES} ${MODULE_FILE}
	${RSC} -p ${MODULE_FILE}

clean::
	rm -f ${MIF_PRODUCT} ${MIF_PRODUCT:.mif=.mx} ${MIF_PRODUCT:.mif=.doc}
