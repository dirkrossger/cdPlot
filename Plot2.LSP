(defun cd:subroutines()
  
  (defun steal:getprop ( object property )
    (if (vlax-property-available-p object property)
        (vlax-get-property object property)
      )
    )

;;-----------------------------------------------------------------------------------------------;;
  
  (defun steal:getdocumentobject ( filename / acdocs dbdoc vers )
    (vlax-map-collection (vla-get-documents (vlax-get-acad-object))
        (function
            (lambda ( doc )
                (setq acdocs (cons (cons (strcase (vla-get-fullname doc)) doc) acdocs))
            )
        )
    )
    (cond
        (   (null (setq filename (findfile filename)))
            nil
        )
        (   (cdr (assoc (strcase filename) acdocs))
        )
        (   (null
                (vl-catch-all-error-p
                    (vl-catch-all-apply 'vla-open
                        (list
                            (setq dbdoc
                                (vla-getinterfaceobject (vlax-get-acad-object)
                                    (if (< (setq vers (atoi (getvar 'acadver))) 16)
                                        "ObjectDBX.AxDbDocument"
                                        (strcat "ObjectDBX.AxDbDocument." (itoa vers))
                                    )
                                )
                            )
                            filename
                        )
                    )
                )
            )
            dbdoc
        )
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun steal:getitem ( collection item )
    (if
        (not
            (vl-catch-all-error-p
                (setq item
                    (vl-catch-all-apply 'vla-item (list collection item))
                )
            )
        )
        item
    )
)

;;-----------------------------------------------------------------------------------------------;;

(defun steal:getdata ( cln cl1 cl2 / exs itm key lst nme val )
  
    (cond
        (   (not (and cl1 cl2))
            nil
        )
        (   (= "Scales" cln)
            (vlax-for itm cl1
                (setq exs (cons (cdr (assoc 300 (entget (vlax-vla-object->ename itm)))) exs))
            )
            (vlax-for itm cl2
                (if (not (member (setq nme (cdr (assoc 300 (entget (vlax-vla-object->ename itm))))) exs))
                    (setq lst (cons (cons nme itm) lst))
                )
            )
            (if (setq lst (vl-sort lst '(lambda ( a b ) (< (strcase (car a)) (strcase (car b))))))
                (cons cln lst)
            )
        )
        (   (= "Drawing Properties" cln)
            (foreach prp '("Author" "Comments" "HyperlinkBase" "Keywords" "Subject" "Title")
                (if
                    (and
                        (vlax-property-available-p   cl2 prp)
                        (vlax-property-available-p   cl1 prp t)
                        (setq itm (vlax-get-property cl2 prp))
                        (/= "" itm)
                        (/= (vlax-get-property cl1 prp) itm)
                    )
                    (setq lst (cons (cons prp itm) lst))
                )
            )
            (if (setq lst (vl-sort lst '(lambda ( a b ) (< (strcase (car a)) (strcase (car b))))))
                (cons cln lst)
            )
        )
        (   (= "Custom Properties" cln)
            (repeat (setq idx (vla-numcustominfo cl2))
                (vla-getcustombyindex cl2 (setq idx (1- idx)) 'key 'itm)
                (if
                    (and itm
                        (/= "" itm)
                        (or (vl-catch-all-error-p (vl-catch-all-apply 'vla-getcustombykey (list cl1 key 'val)))
                            (/= val itm)
                        )
                    )
                    (setq lst (cons (cons key itm) lst))
                )
            )
            (if (setq lst (vl-sort lst '(lambda ( a b ) (< (strcase (car a)) (strcase (car b))))))
                (cons cln lst)
            )
        )
        (   (progn
                (vlax-for itm cl2
                    (if
                        (and
                            (setq nme
                                (cond
                                    (   (vlax-property-available-p itm 'name)
                                        (vla-get-name itm)
                                    )
                                    (   (cdr (assoc 2 (entget (vlax-vla-object->ename itm))))   )
                                )
                            )
                            (not
                                (or
                                    (= nme "")
                                    (wcmatch nme "`**,*|*,ACAD_*")
                                    (steal:getitem cl1 nme)
                                    (and
                                        (vlax-property-available-p itm 'isxref)
                                        (= :vlax-true (vla-get-isxref itm))
                                    )
                                    (and
                                        (vlax-property-available-p itm 'islayout)
                                        (= :vlax-true (vla-get-islayout itm))
                                    )
                                )
                            )
                        )
                        (setq lst (cons (cons nme itm) lst))
                    )
                )
                (setq lst (vl-sort lst '(lambda ( a b ) (< (strcase (car a)) (strcase (car b))))))
            )
            (cons cln lst)
        )              
    )
  )


;;-----------------------------------------------------------------------------------------------;;
  (defun SelectDrawing(dwg)
    (cond
;;;      ((null (if (setq dwg (getfiled "Select Drawing to Steal From"
;;;			       (cond
;;;				 ((and (setq dir (getenv "infratools\\scr"))
;;;				       (setq dir (strcat (vl-filename-directory dir) "\\"))
;;;				       (vl-file-directory-p dir)
;;;				       )
;;;				  dir
;;;				  )
;;;				 (   ""   )
;;;				 )
;;;			       "dwg;dwt;dws"
;;;			       16
;;;			       )
;;;		 )
;;;	     (setenv "infratools\\scr" (strcat (vl-filename-directory dwg) "\\"))
;;;	     )
;;;	   )
;;;     (princ "\nNo File select!")
;;;     )
    
    ((null (setq dbdoc (steal:getdocumentobject dwg)))
     (princ (strcat "\nUnable to Interface with drawing: " dwg))
     )
    ((setq acdoc (vla-get-activedocument (vlax-get-acad-object))
 	   acdic (vla-get-dictionaries acdoc)
	   dbdic (vla-get-dictionaries dbdoc)
	   acext (if (= :vlax-true (vla-get-hasextensiondictionary (vla-get-layers acdoc)))
   		   (vla-getextensiondictionary (vla-get-layers acdoc))
		   )
	   dbext (if (= :vlax-true (vla-get-hasextensiondictionary (vla-get-layers dbdoc)))
		   (vla-getextensiondictionary (vla-get-layers dbdoc))
		   )
	   acdata (list
                    (list "Layouts" (steal:getprop acdoc 'layouts) (steal:getprop dbdoc 'layouts))
                   )
	   dbdata (vl-remove 'nil (mapcar (function (lambda ( x ) (apply 'steal:getdata x))) acdata))
	   )
      )
     )
    (list (list "File" dwg) (list "Data" (list dbdata)))
    )
  )

  
(defun c:xx ( / lst resultFiles resultDv)
  
  (cd:subroutines)

  ; -----------------------------------------------------------------------------------
  ; Odcl functions                                                                    ;
  ; -----------------------------------------------------------------------------------
  (defun c:Form2/Dialog2#OnInitialize (/)
    (princ "\nFiles selected")
   )

  (defun c:Form2/Dialog1#OnInitialize (/ lst)
    (princ "\nLoaded...")
    )

  (defun c:Form2/Dialog1#OnClose (intUpperLeftX intUpperLeftY /)
    (dcl-Form-Close Form2/Dialog1)
    (princ "\n Dialog closed!")
    )



  (defun odcl:fillDataGrid(<Control> lst / i len count r c lyCount lyNames) ; <Control>... Form2/Dialog1/datagrid1

    ; Remove / clean all columns from datagrid before add 
    (repeat (setq count (dcl-grid-getcolumncount <Control>))
      (dcl-Grid-DeleteColumn <Control> (setq count (1- count))))

    ; Add rows
    ; Calculate largest list in variable lst to rowCount
    (setq i 0)
    (foreach x lst
      (setq len (length (mapcar 'car (cadr x))))
      (if (< i len)
	(setq i len)
	(setq len i)
	)
      )
       
			  
    (repeat i
      (dcl-Grid-InsertRow
	    <Control>; Form2/Dialog1/datagrid1
	    (setq i (1- i)) ; intRow [als Long] {intColumn0ImageIndex [als Integer]}
	    ""; strColumn0Label [als String] {strColumn1Label [als String] strColumn2Label [als String]} ...
	  )
      )

    ; Add new columns with name
    (dcl-Grid-InsertColumn <Control> 0 "" 0 20 ) ; insert first empty column
    (if (setq count (length lst) c 1) ; c...column r... row
      (repeat count
	(setq r 0)
	
	(dcl-Grid-InsertColumn <Control> c (car (nth c lst)) 0 100 )

	(setq lyCount (length (cadr (nth c lst)))
	      lyNames (reverse (mapcar 'car (cadr (nth c lst)))))

	(repeat lyCount
	  (dcl-Grid-SetCellStyle
	    <Control> ; Form2/Dialog1/datagrid1
	    r ; intRow [als Long]
	    c ; intColumn [als Long]
	    1 ; intStyle [als Integer] ... with checkbox
	    )

	  (dcl-Grid-SetCellText
	    <Control>; Form2/Dialog1/datagrid1
	    r ; intRow [als Long]
	    c ; intColumn [als Long]
	    (nth (setq lyCount (1- lyCount)) lyNames) ; strLabel [als String]
	    )
	  (setq r (1+ r))
	  )
        
        (setq c (1+ c)))
      )
    )

  (defun c:Form2/Dialog1/cmdDone#OnClicked (/)
    (dcl-Form-Close Form2/Dialog1)
    )

  
  (defun c:Form2/Dialog1/datagrid1#OnColumnClick (intColumn /)
   (dcl-MessageBox "column click!")
   )

 
  
  ; -----------------------------------------------------------------------------------
  ; Main functions                                                                    ;
  ; -----------------------------------------------------------------------------------


  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_OPENDCL")
  (setvar "CMDECHO" cmdecho)

  (dcl-project-load "H:\\Dropbox\\2014\\AF-Infrastructure-Tools\\Plot\\Form2.odcl")
  (if (/= 2 (setq resultFiles (dcl-Form-Show Form2/Dialog2))) ; Select Drawing files!
    (foreach x resultFiles
      (foreach y (cdr (assoc "Data" (SelectDrawing x)))
	(setq lst (append lst (list (list (vl-filename-base x) (cdaar y)))))
	)
      )
    )
    
  (cond
    ((/= lst nil)
     (setq resultDv (dcl-Form-Show Form2/Dialog1))
     (odcl:fillDataGrid Form2/Dialog1/datagrid1 lst)
     )
    
    )
    

    
  
  (princ)
  )
