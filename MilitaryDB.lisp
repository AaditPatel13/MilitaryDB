;; Print out values.
(defun show (&rest x)
    (showhelper x)
    'ignore
)

(defun showhelper (x)
    (if (not (null x))
	(progn
	    (format t "~a~&"
		(if (listp (car x))
		    (sort (car x) #'scmp)
		    (car x)))
	    (showhelper (cdr x)))))

(defun scmp (a b)
    (string< (string a) (string b)))

;; Print out truth values.
(defun truth (&rest x)
    (truthhelper x)
    'ignore
    )

(defun truthhelper (x)
    (if (not (null x))
	(progn
	    (format t "~a~&" (collapse (car x)))
	    (truthhelper (cdr x)))))

;; Collapse true values to simply t or nil.
(defun collapse (x)
    (if x t nil))


(defmacro defun.fexpr (funname varlist &rest expr)
  (let ((ffunname (concat-symbols funname '.fexpr)))
    `(progn
       (defun ,ffunname ,varlist ,@expr)
       (defmacro ,funname (&body args)
	 (list ',ffunname (list 'quote args))))))

(defun concat-symbols (sym1 sym2)
  (intern (concatenate 'string (string sym1) (string sym2))))


(defvar database nil)
(defvar officers nil)
(defvar general nil)
(defvar colonel nil)
(defvar major nil)
(defvar lieutenant nil)
(defvar private nil)
(defvar checkset nil)

(setq checkset '(general colonel major lieutenant private))

(defun clear ()
	(progn
		(setq database nil)
		(setq officers nil)
		(setq general nil)
		(setq colonel nil)
		(setq major nil)
		(setq lieutenant nil)
		(setq private nil)

	)
)

(defun setcurr ()
	(progn
		(setq currentof-officer officers)
		(setq currentof-general general)
		(setq currentof-colonel colonel)
		(setq currentof-major major)
		(setq currentof-lieutenant lieutenant)
		(setq currentof-private private)
	)
)


(defvar currentof-officer officers)
(defvar currentof-general general)
(defvar currentof-colonel colonel)
(defvar currentof-major major)
(defvar currentof-lieutenant lieutenant)
(defvar currentof-private private)


(defun.fexpr store (proplist)
	(cond
		((and (not (member (car proplist) database)) (not (member (car proplist) officers)) )
			(progn 
				(setq officers (append officers (list (car proplist))))
				(setq database (append database proplist))
				(setq currentof-officer officers)
				t				
			)
		)
		((and (not (member (car proplist) database)) (member (car proplist) officers)) 
			(progn
				(setq database (append database proplist)))
				(setq currentof-officer officers)
				t
			)

	)
)

(defun getSet (checkset name)
	(cond
		((equal (car checkset) name) (car checkset))
		(t (getSet (cdr checkset) name))
	)
)


(defun.fexpr insert (nameandset) 
	(cond
		((and (and (equal (cadr nameandset) 'general) 
			(member (car nameandset) database)) 
			(not (member (car nameandset) general)))
		(progn
			(setq general (append general (list (car nameandset))))
			(setq currentof-general general)
			t
		))

		((and (and (equal (cadr nameandset) 'colonel) 
			(member (car nameandset) database)) 
			(not (member (car nameandset) colonel)))
		(progn
			(setq colonel (append colonel (list (car nameandset))))
			(setq currentof-colonel colonel)
			t
		))

		((and (and (equal (cadr nameandset) 'major) 
			(member (car nameandset) database)) 
			(not (member (car nameandset) major)))
		(progn
			(setq major (append major (list (car nameandset))))
			(setq currentof-major major)
			t
		))

		((and (and (equal (cadr nameandset) 'lieutenant) 
			(member (car nameandset) database)) 
			(not (member (car nameandset) lieutenant)))
		(progn
			(setq lieutenant (append lieutenant (list (car nameandset))))
			(setq currentof-lieutenant lieutenant)
			t
		))

		((and (and (equal (cadr nameandset) 'private) 
			(member (car nameandset) database)) 
			(not (member (car nameandset) private)))
		(progn
			(setq private (append private (list (car nameandset))))
			(setq currentof-private private)
			t
		))

	)
)

(defun.fexpr removeofficer (nameandset) 
	(cond
		((and (equal (cadr nameandset) 'general) 
			(member (car nameandset) general))
		(progn
			(setq general (remove (car nameandset) general))
			(setq currentof-general general)
			t
		))

		((and (equal (cadr nameandset) 'colonel) 
			(member (car nameandset) colonel))
		(progn
			(setq colonel (remove (car nameandset) colonel))
			(setq currentof-colonel colonel)
			t
		))

		((and (equal (cadr nameandset) 'major) 
			(member (car nameandset) major))
		(progn
			(setq major (remove (car nameandset) major))
			(setq currentof-major major)
			t
		))

		((and (equal (cadr nameandset) 'lieutenant) 
			(member (car nameandset) lieutenant))
		(progn
			(setq lieutenant (remove (car nameandset) lieutenant))
			(setq currentof-lieutenant lieutenant)
			t
		))

		((and (equal (cadr nameandset) 'private) 
			(member (car nameandset) private))
		(progn
			(setq private (remove (car nameandset) private))
			(setq currentof-private private)
			t
		))


	)
)

(defun deletefromdb (name db)
	(cond
		((null db) nil)
		((equal name (car db)) (cddr db))
		(t (cons (list (car db)) (cons (cadr db) (deletefromdb name (cddr db)))))
	)
)


(defun deleteofflist (name)
	(cond
		((member name officers) (remove name officers))
	)
)

(defun deleteofficerhelp (name)
	(cond
		((member name general) (setq general (remove name general)))
		((member name colonel) (setq colonel (remove name colonel)))
		((member name major) (setq major (remove name major)))
		((member name lieutenant) (setq lieutenant (remove name lieutenant)))
		((member name private) (setq private (remove name private)))
	)
)

(defun.fexpr deleteofficer (name)
	(cond
		((member (car name) database)
		(progn
			(setq database (deletefromdb (car name) database))
			(setq officers (deleteofflist (car name)))
			(deleteofficerhelp (car name))
			t

		))
	)
)


(defun modifyhelp2 (name prop val db)
	(cond
		((equal prop (car (car db))) (cons (cons prop val) (cdr db)))
		(t (cons (car db) (modifyhelp2 name prop val (cdr db))))
	)
)	

(defun modifyhelp (name prop val db)
	(cond
		((null db) nil)
		((equal (car db) name) (modifyhelp2 name prop val (cadr db)))
		(t (modifyhelp name prop val (cddr db) ))
	)
)


(defun reconstruct (name props db)
	(cond
		((equal name (car db)) (cons name (cons props (cddr db))))
		(t (cons (car db) (cons (cadr db) (reconstruct name props (cddr db)))))
	)
)

(defun falsecheck2 (name prop val db)
	(cond
		((null db) nil)
		((equal prop (car (car db))) t)
		(t (falsecheck2 name prop val (cdr db)))
	)
)


(defun falsecheck (name prop val db)
	(cond
		((null db) nil)
		((equal (car db) name) (falsecheck2 name prop val (cadr db)))
		(t (falsecheck name prop val (cddr db) ))
	)
)


(defun.fexpr modify (val)
	(cond
		((not (falsecheck (car val) (cadr val) (caddr val) database)) nil)
		(t
		(progn
			(setq database (reconstruct (car val) (modifyhelp (car val) (cadr val) (caddr val) database) database))
			t
		))
	)
)



(defun flatten (lst)
	(cond
		((atom lst) (cons lst nil))
		(t (append (flatten (car lst)) (flatten (cdr lst))))
	)
)

(defun removenil (lst)
	(remove nil lst)
)

(defun ins (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

(defun finalprod (val)
	(removenil (flatten val))
)

(defun rewritehelp (val ct)
	(cond
		((equal (car val) 'or) (rewritehelp (cdr val) (+ 3 ct)))
		((equal (car val) 'and) (rewritehelp (cdr val) (+ 3 ct)))
		(t ct)
	)
)

(defun rewrite (val)
	(cond
		((and (equal (car val) 'or) (or (equal (cadr val) 'or) (equal (cadr val) 'and) ))
			(ins (cdr val) (rewritehelp (cdr val) 3) 'orr))

		((and (equal (car val) 'and) (or (equal (cadr val) 'or) (equal (cadr val) 'and) ))
			(ins (cdr val) (rewritehelp (cdr val) 3) 'andd))

		((and (equal (car val) 'or) (and (not (equal (cadr val) 'or)) (not (equal (cadr val) 'and)) ))
			(ins (cdr val) (rewritehelp (cdr val) 2) 'orr))

		((and (equal (car val) 'and) (and (not (equal (cadr val) 'or)) (not (equal (cadr val) 'and)) ))
			(ins (cdr val) (rewritehelp (cdr val) 2) 'andd))

	)
)

(defun bringtogether (val val2)
	(cond
		((null val) val2)
		((or (equal (car val) 'or) (equal (car val) 'and)) (bringtogether (rewrite val) val2))
		(t (bringtogether (cdr val) (append val2 (list (car val)) )))
	)
)

(defun searchdbeqp (condition val db db2 ret)
	(cond
		((and (null db2) (not (null db))) (searchdbeqp condition val (cddr db) (cadr (cddr db)) ret))
		((null db) ret)
		((and (equal (car (car db2)) condition) (equal (cdr (car db2)) val))
			(searchdbeqp condition val (cddr db) (cadr (cddr db)) (append ret (list (car db)))))
		(t (searchdbeqp condition val db (cdr db2) ret))
	)
)

(defun searchdbnep (condition val db db2 ret)
	(cond
		((and (null db2) (not (null db))) (searchdbnep condition val (cddr db) (cadr (cddr db)) ret))
		((null db) ret)
		((and (equal (car (car db2)) condition) (not (equal (cdr (car db2)) val)))
			(searchdbnep condition val (cddr db) (cadr (cddr db)) (append ret (list (car db)))))
		(t (searchdbnep condition val db (cdr db2) ret))
	)
)

(defun searchdblep (condition val db db2 ret)
	(cond
		((and (null db2) (not (null db))) (searchdblep condition val (cddr db) (cadr (cddr db)) ret))
		((null db) ret)
		((and (equal (car (car db2)) condition) (<= (cdr (car db2)) val))
			(searchdblep condition val (cddr db) (cadr (cddr db)) (append ret (list (car db)))))
		(t (searchdblep condition val db (cdr db2) ret))
	)
)

(defun searchdbgep (condition val db db2 ret)
	(cond
		((and (null db2) (not (null db))) (searchdbgep condition val (cddr db) (cadr (cddr db)) ret))
		((null db) ret)
		((and (equal (car (car db2)) condition) (>= (cdr (car db2)) val))
			(searchdbgep condition val (cddr db) (cadr (cddr db)) (append ret (list (car db)))))
		(t (searchdbgep condition val db (cdr db2) ret))
	)
)

(defun searchdbltp (condition val db db2 ret)
	(cond
		((and (null db2) (not (null db))) (searchdbltp condition val (cddr db) (cadr (cddr db)) ret))
		((null db) ret)
		((and (equal (car (car db2)) condition) (< (cdr (car db2)) val))
			(searchdbltp condition val (cddr db) (cadr (cddr db)) (append ret (list (car db)))))
		(t (searchdbltp condition val db (cdr db2) ret))
	)
)

(defun searchdbgtp (condition val db db2 ret)
	(cond
		((and (null db2) (not (null db))) (searchdbgtp condition val (cddr db) (cadr (cddr db)) ret))
		((null db) ret)
		((and (equal (car (car db2)) condition) (> (cdr (car db2)) val))
			(searchdbgtp condition val (cddr db) (cadr (cddr db)) (append ret (list (car db)))))
		(t (searchdbgtp condition val db (cdr db2) ret))
	)
)


(defun doeval (oper condition val db)
	(cond 
		((equal oper 'eqp) (searchdbeqp condition val db (cadr db) nil))
		((equal oper 'nep) (searchdbnep condition val db (cadr db) nil))
		((equal oper 'lep) (searchdblep condition val db (cadr db) nil))
		((equal oper 'gep) (searchdbgep condition val db (cadr db) nil))
		((equal oper 'ltp) (searchdbltp condition val db (cadr db) nil))
		((equal oper 'gtp) (searchdbgtp condition val db (cadr db) nil))
	)
)



(defun getevallist (val val2)
	(cond 
		((null val) val2)
		(t (getevallist (cddddr val) 
			(append val2 (doeval (car val) (cadr val) (caddr val) database) (list (cadddr val)))))
	)
)

(defun construct (val val2 val3)
	(cond
		((null val) (append val3 (list val2)))
		((or (equal (car val) 'andd) (equal (car val) 'orr)) 
			(construct (cdr val) nil (append val3 (list  val2) (list (car val))) ) )
		(t (construct (cdr val) (append val2 (list (car val))) val3))
	)
)




(defun finconstructand(a b fin)
	(cond
		((null a) fin)
		((member (car a) b) (finconstructand (cdr a) b (append fin (list (car a)))))
		(t (finconstructand (cdr a) b fin))
	)
)

(defun finconstruct (val val2)
	(cond
		((null val) val2)
		((null (cadr val)) (append val2 (list (car val))))
		((or (equal (car val) 'andd) (equal (car val) 'orr)) 
			(finconstruct (cdr val) (append val2 (list (car val)))))
		((equal (cadr val) 'andd) 
			(finconstruct (cdddr val) (append val2 (list  (finconstructand (car val) (caddr val) nil)))))
		((equal (cadr val) 'orr) 
			(finconstruct (cdddr val)
			 (append val2 (list (remove-duplicates (append (car val) (caddr val)))))))
	)
)



(defun.fexpr findall (val)
	(cond
		((equal (car val) 't)
		(progn
			(setq currentof-officer officers)
			officers
		))
		(t 
		(progn
			(setq currentof-officer officers)
			(finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod val) nil) nil)) nil nil) 
			nil) nil))
		))
	)

)

(defun findallhelp (setname condition ret)
	(cond
		((null setname) ret)
		((member (car condition) setname) 
			(findallhelp setname (cdr condition) (append ret (list (car condition)))))
		(t (findallhelp (cdr setname) condition ret))
	)
)


(defun.fexpr findallinset (val)
	(cond
		((equal (car val) 'general) 
		(progn
			(setq currentof-general general)
			(findallhelp general (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) nil)
			))
		((equal (car val) 'colonel) 
		(progn
			(setq currentof-colonel colonel)
			(findallhelp colonel (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) nil)
			))
		((equal (car val) 'major) 
		(progn
			(setq currentof-major major)
			(findallhelp major (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) nil)
			))
		((equal (car val) 'lieutenant) 
		(progn
			(setq currentof-lieutenant lieutenant)
			(findallhelp lieutenant (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) nil)
			))
		((equal (car val) 'private) 
		(progn
			(setq currentof-private private)
			(findallhelp private (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) nil)
			))

	)
)

(defun findanyhelp (val)
	(cond
		((member (car val) officers) (car val))
		(t (findanyhelp (cdr val)))
	)
)

(defun.fexpr findany (val)
	(findanyhelp (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (car val)) nil) nil)) nil nil) 
			nil) nil)))
)

(defun findanysethelp (setname condition)
	(cond
		((member (car condition) setname) (car condition))
		(t (findanysethelp (cdr setname) condition ))
	)
)


(defun.fexpr findanyinset (val)
	(cond
		((equal (car val) 'general) (findanysethelp general (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil))))
		((equal (car val) 'colonel) (findanysethelp colonel (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil))))
		((equal (car val) 'major) (findanysethelp major (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil))))
		((equal (car val) 'lieutenant) (findanysethelp lieutenant (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil))))
		((equal (car val) 'private) (findanysethelp private (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil))))

	)
)

(defun findnexthelp (val val2 set1 set2)
	(cond
		((and (equal (car val) (car set1)) (null (cdr set1)))
			(progn
				(setq currentof-officer officers)
				(car val)
			))

		((and (equal (car val) (car set1)) (not (null (cdr set1))))
			(progn
				(setq currentof-officer (cdr set1))
				(car val)
			))

		((and (not (equal (car val) (car set1))) (null (cdr set1)))
			(progn
				(findnexthelp (cdr val) val2 set2 set2)
			))

		((and (not (equal (car val) (car set1))) (not (null (cdr set1))))
			(progn
				(findnexthelp val val2 (cdr set1) set2)
			))			
	)
)


(defun.fexpr findnext (val)
	(findnexthelp (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (car val) ) nil) nil)) nil nil) 
			nil) nil)) (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (car val) ) nil) nil)) nil nil) 
			nil) nil)) currentof-officer currentof-officer)
)


(defun fniasgeneral (val val2 set1 set2)
	(cond
		((null set2) nil)
		((null (car val)) (fniasgeneral val2 val2 general general))
		((and (equal (car val) (car set1)) (null (cdr set1)))
			(progn
				(setq currentof-general general)
				(car val)
			))

		((and (equal (car val) (car set1)) (not (null (cdr set1))))
			(progn
				(setq currentof-general (cdr set1))
				(car val)
			))

		((and (not (equal (car val) (car set1))) (null (cdr set1)))
			(progn
				(fniasgeneral (cdr val) val2 set2 set2)
			))

		((and (not (equal (car val) (car set1))) (not (null (cdr set1))))
			(progn
				(fniasgeneral val val2 (cdr set1) set2)
			))			
	)
)

(defun fniascolonel (val val2 set1 set2)
	(cond
		((null set2) nil)
		((null (car val)) (fniascolonel val2 val2 colonel colonel))
		((and (equal (car val) (car set1)) (null (cdr set1)))
			(progn
				(setq currentof-colonel colonel)
				(car val)
			))

		((and (equal (car val) (car set1)) (not (null (cdr set1))))
			(progn
				(setq currentof-colonel (cdr set1))
				(car val)
			))

		((and (not (equal (car val) (car set1))) (null (cdr set1)))
			(progn
				(fniascolonel (cdr val) val2 set2 set2)
			))

		((and (not (equal (car val) (car set1))) (not (null (cdr set1))))
			(progn
				(fniascolonel val val2 (cdr set1) set2)
			))			
	)
)

(defun fniasmajor (val val2 set1 set2)
	(cond
		((null set2) nil)
		((null (car val)) (fniasmajor val2 val2 major major))
		((and (equal (car val) (car set1)) (null (cdr set1)))
			(progn
				(setq currentof-major major)
				(car val)
			))

		((and (equal (car val) (car set1)) (not (null (cdr set1))))
			(progn
				(setq currentof-major (cdr set1))
				(car val)
			))

		((and (not (equal (car val) (car set1))) (null (cdr set1)))
			(progn
				(fniasmajor (cdr val) val2 set2 set2)
			))

		((and (not (equal (car val) (car set1))) (not (null (cdr set1))))
			(progn
				(fniasmajor val val2 (cdr set1) set2)
			))			
	)
)

(defun fniaslieutenant (val val2 set1 set2)
	(cond
		((null set2) nil)
		((null (car val)) (fniaslieutenant val2 val2 lieutenant lieutenant))
		((and (equal (car val) (car set1)) (null (cdr set1)))
			(progn
				(setq currentof-lieutenant lieutenant)
				(car val)
			))

		((and (equal (car val) (car set1)) (not (null (cdr set1))))
			(progn
				(setq currentof-lieutenant (cdr set1))
				(car val)
			))

		((and (not (equal (car val) (car set1))) (null (cdr set1)))
			(progn
				(fniaslieutenant (cdr val) val2 set2 set2)
			))

		((and (not (equal (car val) (car set1))) (not (null (cdr set1))))
			(progn
				(fniaslieutenant val val2 (cdr set1) set2)
			))			
	)
)


(defun fniasprivate (val val2 set1 set2)
	(cond
		((null set2) nil)
		((null (car val)) (fniasprivate val2 val2 private private))
		((and (equal (car val) (car set1)) (null (cdr set1)))
			(progn
				(setq currentof-private private)
				(car val)
			))

		((and (equal (car val) (car set1)) (not (null (cdr set1))))
			(progn
				(setq currentof-private (cdr set1))
				(car val)
			))

		((and (not (equal (car val) (car set1))) (null (cdr set1)))
			(progn
				(fniasprivate (cdr val) val2 set2 set2)
			))

		((and (not (equal (car val) (car set1))) (not (null (cdr set1))))
			(progn
				(fniasprivate val val2 (cdr set1) set2)
			))			
	)
)

(defun.fexpr findnextinset (val)
	(cond
		((equal (car val) 'general) (fniasgeneral (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) currentof-general currentof-general))

		((equal (car val) 'colonel) (fniascolonel (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) currentof-colonel currentof-colonel))

		((equal (car val) 'major) (fniasmajor (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) currentof-major currentof-major))

		((equal (car val) 'lieutenant) (fniaslieutenant (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) currentof-lieutenant currentof-lieutenant))

		((equal (car val) 'private) (fniasprivate (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) (finalprod (finconstruct (finconstruct (construct 
			(removenil (getevallist (bringtogether (finalprod (cdr val) ) nil) nil)) nil nil) 
			nil) nil)) currentof-private currentof-private))
	)
)


