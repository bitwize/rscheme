(define-syntax (curses-extension) 81)

(define-primop (add-char <obj> <ascii-char>) => <raw-int>
    (bytecode (curses-extension) 0))

(define-primop (get-char <obj>) => <ascii-char>
    (bytecode (curses-extension) 1))

(define-primop (add-str <obj> <raw-string>) => <raw-int>
    (bytecode (curses-extension) 2))

(define-primop (get-str <obj> <raw-string>) => <raw-int>
    (bytecode (curses-extension) 3))

(define-primop (move <obj> <raw-int> <raw-int>) => <raw-int>
    (bytecode (curses-extension) 4))

(define-primop (clear <obj>) => <raw-int>
    (bytecode (curses-extension) 5))

(define-primop (erase <obj>) => <raw-int>
    (bytecode (curses-extension) 6))

(define-primop (clear-to-bottom <obj>) => <raw-int>
    (bytecode (curses-extension) 7))

(define-primop (clear-to-eol <obj>) => <raw-int>
    (bytecode (curses-extension) 8))

(define-primop (insert-line <obj>) => <raw-int>
    (bytecode (curses-extension) 9))

(define-primop (delete-line <obj>) => <raw-int>
    (bytecode (curses-extension) 10))

(define-primop (refresh <obj>) => <raw-int>
    (bytecode (curses-extension) 11))

(define-primop (input-char <obj>) => <ascii-char>
    (bytecode (curses-extension) 12))

(define-primop (insert-char <obj> <ascii-char>) => <raw-int>
    (bytecode (curses-extension) 13))

(define-primop (delete-char <obj>) => <raw-int>
    (bytecode (curses-extension) 14))

(define-primop (standout-begin <obj>) => <raw-int>
    (bytecode (curses-extension) 15))

(define-primop (standout-end <obj>) => <raw-int>
    (bytecode (curses-extension) 16))



(define-primop (mode-raw) => <raw-int> (bytecode (curses-extension) 30))
(define-primop (mode-no-raw) => <raw-int> (bytecode (curses-extension) 31))
(define-primop (mode-cbreak) => <raw-int> (bytecode (curses-extension) 32))
(define-primop (mode-no-cbreak) => <raw-int> (bytecode (curses-extension) 33))
(define-primop (mode-cr) => <raw-int> (bytecode (curses-extension) 34))
(define-primop (mode-no-cr) => <raw-int> (bytecode (curses-extension) 35))
(define-primop (mode-no-echo) => <raw-int> (bytecode (curses-extension) 36))
(define-primop (mode-nl) => <raw-int> (bytecode (curses-extension) 37))
(define-primop (mode-no-nl) => <raw-int> (bytecode (curses-extension) 38))
(define-primop (save-tty) => <raw-int> (bytecode (curses-extension) 39))
(define-primop (reset-tty) => <raw-int> (bytecode (curses-extension) 40))


(define-primop (new-window <raw-int> <raw-int> <raw-int> <raw-int>) 
	=> <obj>
   (bytecode (curses-extension) 51))
   
(define-primop (sub-window <obj> <raw-int> <raw-int> <raw-int> <raw-int>) 
	=> <obj>
   (bytecode (curses-extension) 52))

(define-safe-glue (init-curses)
{
  WINDOW *screen;
  screen = initscr();
  if (!screen)
    scheme_error( "init-curses: could not be initialized", 0 );
  REG0 = RAW_PTR_TO_OBJ(screen);
  RETURN1();
})

(define-safe-glue (screen-size)
{
  REG0 = int2fx(LINES);
  REG1 = int2fx(COLS);
  RETURN(2);
})

(define-safe-glue (end-curses)
{
   endwin();
   RETURN0();
})
