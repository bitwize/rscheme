/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/hashfn.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.9
 * File mod date:    2003-10-22 17:56:16
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          hash function implementation
 *------------------------------------------------------------------------*/

#include <rscheme/runtime.h>
#include <ctype.h>
#include <rscheme/hashfn.h>

#if WORD_IS_32_BITS
#define SIGN_EXT_AS_32(x)  (x)
#else
#define SIGN_EXT_AS_32(x)  _sign_ext_as_32( x )

static _rs_inline obj _sign_ext_as_32( obj x )
{
  if (VAL(x) & 0x80000000UL)
    {
      /* plug in the sign bits by hand... */
      return OBJ( VAL(x) | 0xFFFFFFFF00000000UL );
    }
  else
    {
      return x;
    }
}
#endif


#define UINT_32_TO_HASH(u) SIGN_EXT_AS_32( OBJ( ((u) & ~PRIMARY_TAG_MASK) \
						+ FIXNUM_TAG ) )

/*
  This table is used in the (slow) rehashing
  of integers.  The algorithm is that 32 numbers
  are XORed together.  The numbers are chosen from
  32 pairs of random numbers, based on the values
  of the 32 bits of the input integer.  Hence,
  each output bit depends on all the input bits
  (except for those table entries that both have a 0
  in some bit position)

  It was generated using the Mathematica function:
  Table[ Random[Integer,{0,2^32-1}], {64} ]
*/

static UINT_32 hash2[256] = {
  3923816129U, 2317860316U, 3090765807U, 3396889516U, 
  3791021225U, 3385363528U, 1627584886U, 441766619U, 
  1443615973U, 2559228144U, 3745581467U, 2707891360U, 
  2453701053U, 4118783842U, 2272467972U, 3504575509U, 
  152894779U, 743797209U, 2031564275U, 1338548962U, 
  1247835526U, 3772155728U, 2210079934U, 1174138353U, 
  322644444U, 92762790U, 4176884183U, 10290758U, 3623950881U, 
  3857104797U, 586331503U, 3681236654U, 2781463670U, 
  510545103U, 3641742481U, 564741816U, 3371176501U, 
  4086285133U, 549310572U, 2847488162U, 2418002277U, 
  3258344369U, 2886857065U, 321923661U, 3203379610U, 
  1612047570U, 500458784U, 2735290498U, 726477745U, 
  430431592U, 1929539627U, 3593058507U, 2659427405U, 
  3396021212U, 1557943054U, 3082517365U, 2531003866U, 
  2513965555U, 475750036U, 85872719U, 1799540093U, 70759339U, 
  3173403528U, 88305710U,

  /* table extended to 256 elements... */

  1444479746U, 607884610U, 1784445714U, 1843424859U, 2623876040U, 1669714710U, 
  4002781276U, 1403122643U, 1286334834U, 1162288818U, 1740302133U, 94146012U, 
  366271236U, 1300009882U, 919067601U, 4168407875U, 611577376U, 2105024830U, 
  1445390840U, 1659881157U, 1175692197U, 2320598268U, 3347650902U, 1418174478U, 
  3210760040U, 3317344684U, 889390777U, 1709341951U, 3352231283U, 1963753097U, 
  894491546U, 317716232U, 2526763840U, 1380467083U, 1670066757U, 1816638427U, 
  2228726409U, 1757946544U, 2579770539U, 2388126853U, 1098222604U, 863468032U, 
  1148376992U, 270716497U, 3213520766U, 1549612341U, 1049103871U, 3426411528U, 
  169107586U, 4069447558U, 1664785257U, 3714000565U, 3480631914U, 4076041060U, 
  1042871132U, 750601309U, 2249175453U, 4027230141U, 1187796805U, 466162942U, 
  4220685281U, 3591918410U, 2933983774U, 4165882447U, 1744323151U, 2993255788U, 
  2637126380U, 891612339U, 2946494680U, 785460496U, 286802514U, 1316841025U, 
  2717568855U, 180172737U, 3208475318U, 3886550919U, 3217307844U, 3772859334U, 
  3873074902U, 553228400U, 4153956265U, 1463714956U, 3850002755U, 2507305100U, 
  1208505543U, 4026963132U, 2117775370U, 3918285883U, 374610566U, 1308608087U, 
  2501585490U, 3347981904U, 3415148181U, 2088247903U, 3161716008U, 4260847727U, 
  2991297839U, 4018864341U, 3325567207U, 2772620925U, 3217145624U, 2924587275U, 
  3672434761U, 3070296236U, 3397403662U, 3050566058U, 3593989256U, 1250765120U, 
  1385523700U, 2065954297U, 4257805525U, 2672576646U, 523882550U, 1971709959U, 
  992240257U, 2123249520U, 3963071897U, 524435548U, 3495946153U, 648501064U, 
  431401916U, 2353817923U, 1066524782U, 2205326615U, 2233710607U, 435631192U, 
  2464974783U, 4159422120U, 240454676U, 3015531966U, 4016166766U, 3558494390U, 
  1241743293U, 3466100486U, 183395232U, 35227054U, 1979727338U, 3822174333U, 
  3582400267U, 4254738782U, 1139782815U, 2419185982U, 1161940238U, 2450283001U, 
  3259558277U, 2267654027U, 3729983059U, 2979976419U, 2030150099U, 3602622211U, 
  777096615U, 2460786711U, 3814569335U, 3528939238U, 33158185U, 2033500291U, 
  4065565552U, 943398627U, 3038595889U, 3086427588U, 3658822287U, 748379468U, 
  4041260307U, 3973340988U, 1445014281U, 4019430926U, 2380223252U, 3724262095U, 
  31549722U, 680651677U, 2366025259U, 161515638U, 1690406879U, 4208595465U, 
  2218644420U, 4231047951U, 750509810U, 2351776407U, 1677173683U, 2786633452U, 
  884008263U, 1227814338U, 2854132135U, 3501607530U, 867703527U, 746529427U, 
  1348189009U, 3674740476U, 33411520U, 1597710469U, 2824702344U, 3709653569U
 };

/*  A nasty rehashing function
    Rather slow, but pretty gnarly
*/

/* every bit-decision in the input
   flips every bit in the output with Pr 0.5
   What could be better?
   It's slow though (approx 2778 68000 cycles on average 
   				-- incl. prologue & RTS
				   but not incl. `obj' conversions
   		     at 25MHz, that's about 111us)
		hand unrolled*5: 1826 68000 cycles on avg.
				 at 25MHz, about 73us
*/

obj rehash_fixnum( obj fixnum )
{
  UINT_32 n, h = 0;

  n = VAL(fixnum);
  h = hash2[ ((n >>  2) & 15) ]
    ^ hash2[ ((n >>  6) & 15) + 16 ]
    ^ hash2[ ((n >> 10) & 15) + 2 * 16 ]
    ^ hash2[ ((n >> 14) & 15) + 3 * 16 ]
    ^ hash2[ ((n >> 18) & 15) + 4 * 16 ]
    ^ hash2[ ((n >> 22) & 15) + 5 * 16 ]
    ^ hash2[ ((n >> 26) & 15) + 6 * 16 ]
    ^ hash2[ ((n >> 30) &  3) + 7 * 16 ];
  return UINT_32_TO_HASH( h );
}

/*   this somewhat more robust hash function is used for transient
 *   pointers and immobs
 */

obj obj_hash( obj v )
{
  UINT_32 x = VAL(v);
  
  x = hash2[x & 255]
    + hash2[(x >> 8) & 255]
    + hash2[(x >> 16) & 255]
    + hash2[(x >> 24) & 255];
  return UINT_32_TO_HASH( x );
}

obj obj_hash2( obj x0, obj y0 )
{
  UINT_32 h = 0x5DEFACE5;
  UINT_32 x = VAL( x0 );
  UINT_32 y = VAL( y0 );

  h += hash2[ x & 0xFF ];
  h += hash2[ ((x + h) >> 8) & 0xFF ];
  h += hash2[ ((x + h) >> 16) & 0xFF ];
  h += hash2[ ((x + h) >> 24) & 0xFF ];

  h += hash2[ (y + h) & 0xFF ];
  h += hash2[ ((y + h) >> 8) & 0xFF ];
  h += hash2[ ((y + h) >> 16) & 0xFF ];
  h += hash2[ ((y + h) >> 24) & 0xFF ];
  return UINT_32_TO_HASH( h );
}

obj hash_unicode_string( obj u_str )
{
  return UINT_32_TO_HASH( crc_hash_unicode( unicode_string_text(u_str),
                                            unicode_string_length(u_str) ) );
}

obj raw_bytes_hash( const void *bytes, unsigned length )
{
  return UINT_32_TO_HASH( crc_hash(bytes,length,0) );
}

obj raw_ci_bytes_hash( const void *bytes, unsigned length )
{
  return UINT_32_TO_HASH( crc_hash(bytes,length,1) );
}
