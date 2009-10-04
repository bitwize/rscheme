#include <rscheme.h>

int rscheme_xml_scanner( obj scanner_state, 
                         obj buffer,
                         unsigned *offset,
                         unsigned len,
                         obj class_vec,
                         int is_last,
                         obj *token,
                         int goal );


#define TC_MORE                         (0)
#define TC_EOF                          (1)
#define TC_EMPTY_ELEMENT                (2)
#define TC_START_ELEMENT                (3)
#define TC_END_ELEMENT                  (4)
#define TC_TEXT                         (5)
#define TC_PI                           (6)
#define TC_DOCTYPE_DECL                 (7)
#define TC_COMMENT                      (8)
#define TC_WHITESPACE                   (9)     /* TC_TEXT with only lws */
#define TC_XML                         (14)

/* These will only show up under certain goals... */

#define TC_NO_MATCH                    (10)
#define TC_NAME                        (11)
#define TC_ENTITY_VALUE                (12)
#define TC_EXTERNAL_ID                 (13)
