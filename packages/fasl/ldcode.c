#include "fasldef.h"

#if PLATFORM_ARCH_PPC

void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  UINT_32 *f = (UINT_32 *)stub;
  /* this isn't really code; on POWER (and derivatives), calling
   * through a function pointer really involves destructuring
   * a FUNCTION DESCRIPTOR
   */
#if defined(PLATFORM_LINUX) \
 || defined(PLATFORM_RHAPSODY) \
 || defined(PLATFORM_DARWIN)
  /* except on MkLinux and Darwin/MacOS X, where this really is 
     trampoline code (I'm assuming same is true for linuxppc)
   */
  /* So, let's build some code to jump to the stub
     entry point.  e.g., supposed ADDR=0x12345678

     3c 00 12 34    lis    r0,$HIGH16
     60 00 56 78    ori    r0,r0,$LOW16
     7c 09 03 a6    mtctr  r0
     4e 80 04 20    bctr
     */
  UINT_32 addr = (UINT_32)stub;
  hdr->stub_proc[0] = 0x3c000000 + ((addr >> 16) & 0xFFFF);
  hdr->stub_proc[1] = 0x60000000 + (addr & 0xFFFF);
  hdr->stub_proc[2] = 0x7c0903a6;
  hdr->stub_proc[3] = 0x4e800420;
#else
  hdr->stub_proc[0] = f[0];
  hdr->stub_proc[1] = f[1];
  hdr->stub_proc[2] = f[2];
#endif
}

#endif /* AIX, Rhapsody, etc */

/* linux, little-endian sunos boxes, FreeBSD, and Rhapsody for Intel
 * run on intel hardware
 */

#if PLATFORM_ARCH_X86_64
void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  static unsigned long stub_ptr;
  UINT_8 *d = (UINT_8 *)hdr->stub_proc;
  
  stub_ptr = (unsigned long)stub;

  /* load quad literal:                 movq $<literal>,%rax */

  *d++ = 0x48;
  *d++ = 0xb8;
  *(unsigned long *)d = stub_ptr;
  d += 8;

  /* jmp indirect through register (8-byte) */

  *d++ = 0xFF;
  *d++ = 0xE0;
}
#endif

#if PLATFORM_ARCH_I386

void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  static UINT_32 stub_ptr;
  UINT_8 *d = (UINT_8 *)hdr->stub_proc;
  UINT_32 addr;
  
  stub_ptr = (UINT_32)stub;
  addr = (UINT_32)&stub_ptr;

  *d++ = 0xff;  /* jmp absolute indirect (4-byte addr of fn ptr) */
  *d++ = 0x25;
  *d++ = addr & 0xFF;
  *d++ = (addr >> 8) & 0xFF;
  *d++ = (addr >> 16) & 0xFF;
  *d++ = (addr >> 24) & 0xFF;
}
#endif /* LINUX */

#if PLATFORM_ARCH_SPARC

/* SPARC 
            +-----+----------------------+
    call    | 0 1 |  29-bit displacement |
            +-----+----------------------+

stub gateway

    ==> save %sp, -112, %sp     0x9de3bf90
        call real_stub          <call>
        nop                     0x01000000
        ret                     0x81c7e008
        restore                 0x91e80008
*/

void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  static UINT_32 stub_ptr;
  UINT_32 *d = (UINT_32 *)hdr->stub_proc;
  UINT_32 addr = (UINT_32)stub;

  *d++ = 0x9de3bf90;
  *d = ((addr - (UINT_32)d) / 4) + 0x40000000;
  d++;
  *d++ = 0x01000000;
  *d++ = 0x81c7e008;
  *d++ = 0x91e80008;
}

#endif /* ARCH SPARC */

#if PLATFORM_ARCH_S390

/* S390

stub gateway

        bras    %r1,.LTN0_0
.LT0_0:
.LC0:
        .long   0xbabebec0
.LTN0_0:
        l       %r1,.LC0-.LT1_0(%r1)
        br      %r1
*/

void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  static UINT_32 stub_ptr;
  UINT_32 *d = (UINT_32 *)hdr->stub_proc;
  UINT_32 addr = (UINT_32)stub;

  *d++ = 0xA7150004;
  *d++ = addr;
  *d++ = 0x58101000;
  *((unsigned short *)d) = 0x07f1;
  d++;
}

#endif /* ARCH S390 */
 
#if PLATFORM_ARCH_MIPS
/* courtesy of Chris <chrislee@gs157.sp.cs.cmu.edu> */

void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  static UINT_32 stub_ptr, ptr_low, ptr_hi;
  unsigned base, offset;
  
  stub_ptr = (UINT_32)stub;
  ptr_low  = stub_ptr & 0xFFFF;
  ptr_hi   = (stub_ptr - ptr_low) >> 16;

  /* Load Upper Immediate
   * LUI(6)  0(5)  rt(5) imm(16)   (use rt = 25 = 11001 = "$jp" in gdb)
   * 001111  00000 11001 ptr_hi    (jp means "PIC jump register")
   * == 0011 1100 0001 1001 ptr_hi
   * == 0x3C19 ptr_hi
   *
   * Add Immediate Word Unsigned (note--this seems to do a _signed_ addition!)
   * ADDIU(6) rs(5) rt(5) imm(16)  (use rs = rt = 25 = 11001)
   * 001001   11001 11001 ptr_low
   * == 0010 0111 0011 1001 ptr_low
   * == 0x2739 ptr_low
   *
   * Jump register 
   * special(6) rs(5) 0(15) JR(6) (use rs = 25 = 11001)
   * 000000     11001 0(15) 001000
   * == 0000 0011 0010 0000 0000 0000 0000 1000
   * == 0x0320 0008
   *
   * NOP
   *  from gas source code it looks like it is
   * == 0x00000000.
   */

  if ( 0x8000 & ptr_low ) {
    /* _signed_ arithmetic of ADDIU will cause a _subtraction_ ! */
    ptr_hi += 0x1;   /* + 0x1000 because ptr_hi is shifted 16 bits */
    ptr_low = 0xFFFF & (unsigned)((int)ptr_low - 0x10000);
  }

  /* LW: GPR(24) <-- mem[addr]    */
  hdr->stub_proc[0] = 0x3C190000 | ptr_hi;
  hdr->stub_proc[1] = 0x27390000 | ptr_low;
  hdr->stub_proc[2] = 0x03200008;         /* JR : JR(GPR(25))  */
  hdr->stub_proc[3] = 0x00000000;         /* NOP */
}

#endif /* MIPS */

#if PLATFORM_ARCH_ALPHA

void init_stub_procs( struct FASL_Header *hdr, jump_addr stub )
{
  UINT_16 w0, w1, w2, w3;
  XUINT_32 *cp = hdr->stub_proc;

  w0 = 0xFFFF & (((unsigned long)stub) >> 48);
  w1 = 0xFFFF & (((unsigned long)stub) >> 32);
  w2 = 0xFFFF & (((unsigned long)stub) >> 16);
  w3 = 0xFFFF & ((unsigned long)stub);

  /******************************************************************
   *
   *  we give it .s:
   *
   *                      lda      $1,0x1231($31)
   *                      zapnot   $27,3,$27
   *                      ldah     $1,0x1230($1)
   *                      zapnot   $27,15,$27
   *                      sll      $1,32,$1
   *                      lda      $27,0x1233($31)
   *                      zapnot   $27,3,$27
   *                      ldah     $27,0x1232($27)
   *                      zapnot   $27,15,$27
   *                      addq     $27,$1,$27
   *                      jmp      $31,($27),0
   *
   *  and gdb x/11i says ~
   *
   *
   *          203f<w1>  lda        t0,<w1>(zero)
   *          4b60763b  zapnot     t12,0x3,t12
   *          2421<w0>  ldah       t0,4656(t0)
   *          4b61f63b  zapnot     t12,0xf,t12
   *          48241721  sll        t0,0x20,t0
   *          237f<w3>  lda        t12,<w3>(zero)
   *          4b60763b  zapnot     t12,0x3,t12
   *          277b<w2>  ldah       t12,4658(t12)
   *          4b61f63b  zapnot     t12,0xf,t12
   *          4361041b  addq       t12,t0,t12
   *          6bfb0000  jmp        zero,(t12),0x1200006ec
   *
   *   nb, `zapnot' clears the bytes corresponding to the bits
   *   in the middle argument...
   *
   *   I think $1 is a temporary register,
   *   $27 is supposed to be the recipient's address for
   *   cross-module calls (it will load `gp' from there)
   *   $31 is `zero'
   *
   ******************************************************************/

  *cp++ = 0x203f0000 + w1;
  *cp++ = 0x4b60763b;
  *cp++ = 0x24210000 + w0;
  *cp++ = 0x4b61f63b;
  *cp++ = 0x48241721;
  *cp++ = 0x237f0000 + w3;
  *cp++ = 0x4b60763b;
  *cp++ = 0x277b0000 + w2;
  *cp++ = 0x4b61f63b;
  *cp++ = 0x4361041b;
  *cp++ = 0x6bfb0000;
}
#endif
