	.section	".text"
	.globl		read_timebase
	.set	r3,3
	.set	r4,4
	.set	r5,5
	.set	r6,6
read_timebase:
	mftbu	r3
	mftb	r4
	mftbu	r5
	cmpw	0,r3,r5
	bne-	read_timebase
	blr
