p0 = { x0, y0 }
p1 = { x1, y1 }
p2 = { x2, y2 }
p3 = { x3, y3 }

a = p3 - 3 p2 + 3 p1 - p0
b = 3 p0 - 6 p1 + 3 p2
c = 3 p1 - 3 p0

bez[t_] := a t^3 + b t^2 + c t + p0

(* computing the length of a bezier curve segment... *)

s.b. Integrate[ Sqrt[ ... ] ] which doesn't reduce
(even Sqrt[ O(t^3) ] doesn't reduce, and above is O(t^4))

Simplify[ Integrate[ D[bez[t],t] . D[bez[t],t], {t,0,1} ]  ]

(* rendering Mathematica formulae in scheme notation *)

SchemeHead[ Times ] := "*"
SchemeHead[ Power ] := "expt"
SchemeHead[ Plus ] := "+"
SchemeHead[ Rational ] := "/"

SchemeSeq[ args_ ] := Apply[ 
   StringJoin, 
   Flatten[ Transpose[ { SchemeForm /@ args, 
			 Append[ (" " &) /@ Drop[args,1], "" ] } ] ] ]

SchemeForm[ h_[ args__ ] ] := Block[
   { sh = SchemeHead[h],
     ar = SchemeSeq[ List[args] ] },
   StringJoin[ "(", sh, " ", ar, ")" ] ]

SchemeForm[ a_Number ] := ToString[ a ]
SchemeForm[ a_Symbol ] := ToString[ a ]
SchemeForm[ a_Integer ] := ToString[ a ]

SchemeHead[ Rule ] := "let"

(*  Fit a bezier curve to 4 data points.  Note the choice of t1 and t2
    is essentially arbitrary.  Once reference suggested using

      t1 = |P1-P0| / (|P3-P2|+|P2-P1|+|P1-P0|)
      t2 = (|P2-P0| + |P1+P0|) / (|P3-P2|+|P2-P1|+|P1-P0|)

    but why?
*)

Solve[ { bez[0] == {X0,Y0}, 
         bez[1/3] == {X1,Y1}, 
         bez[2/3] == {X2,Y2},
         bez[1] == {X3,Y3} } , {x0,x1,x2,x3,y0,y1,y2,y3} ]

(* A bezier curve approximating 1/4 cycle of a sine wave *)

Solve[ { bez[0] == { 0,0 },
         p1 - p0 == {A,A},
         p3 - p2 == {B,0},
         bez[1] == { Pi/2, 1 },
         bez[Pi/4] == { Pi/4, Sin[Pi/4] }
          }, {A,B} ]
