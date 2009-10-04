
(define-X-glue (event-accessor (event <X-event>)) :template
  literals: ((& <point>) 
	     (& <rect>)
	     '#(() (shift) (caps-lock) (shift caps-lock)
		   (control) (control shift) (control caps-lock) 
		   (control shift caps-lock) (command) (command shift)
		   (command caps-lock) (command shift caps-lock)
		   (command control) (command control shift) 
		   (command control caps-lock) 
		   (command control shift caps-lock)))

{
  USE_FUNCTION_ENVT();
  switch (fx2int(envt_reg))
    {
    case 0:
      REG0 = make2( TLREF(0), 
		    int2fx( event->xmotion.x ), 
		    int2fx( event->xmotion.y ) );
      break;
    case 1:
      REG0 = make2( TLREF(0), 
		    int2fx( event->xmotion.x_root ), 
		    int2fx( event->xmotion.y_root ) );
      break;
    case 2:
      {
	static char text[10];
	int n;
	KeySym key;

	n = XLookupString( &event->xkey, text, 10, &key, 0 );
	text[n] = 0;
	REG0 = make_string( text );
	REG1 = int2fx(key);
	RETURN(2);
      }
    case 3:
      REG0 = make4( TLREF(1),
		    int2fx( event->xconfigure.x ),
		    int2fx( event->xconfigure.y ),
		    int2fx( event->xconfigure.width ),
		    int2fx( event->xconfigure.height ) );
      break;
    case 4:
      REG0 = gvec_read( LITERAL(2), SLOT(event->xkey.state & 15) );
      break;
    }

  RETURN1();
})

(define motion-event->window-point
  (make <closure> 
	environment: 0 
	template: event-accessor))

(define motion-event->root-point
  (make <closure> 
	environment: 1
	template: event-accessor))

(define key-event->key 
  (make <closure> 
	environment: 2 
	template: event-accessor))

(define configure-event->frame
  (make <closure> 
	environment: 3
	template: event-accessor))

(define key-event->state
  (make <closure> 
	environment: 4
	template: event-accessor))

#|
(define-X-glue (key-event->key (event <X-event>))
{
static char text[10];
int n;
KeySym key;

  n = XLookupString( (XKeyEvent *)event, text, 10, &key, 0 );
  text[n] = 0;
  REG0 = make_string( text );
  REG1 = int2fx(key);
  RETURN(2);
})
|#

(define-X-glue (button-event->point (event <X-event>))
  literals: ((& <point>))
{
   REG0 = make2( TLREF(0), 
		 int2fx(event->xbutton.x),
		 int2fx(event->xbutton.y) );
   RETURN1();
})
