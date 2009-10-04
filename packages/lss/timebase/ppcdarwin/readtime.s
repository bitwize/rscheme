.text
	.align 2
.globl _read_timebase
_read_timebase:
        mftbu   r3
	mftb	r4
	mftbu	r5
	cmpw	0,r3,r5
	bne-	_read_timebase
	blr
