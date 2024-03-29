;;; --------------------------------------------------------------------------
;;; File:   chmfilter.po - executable translated stuff
;;; Author: Jens Kallup  - paule32
;;;
;;; Rights: (c) 2024 by kallup non-profit software
;;;         all rights reserved
;;;
;;; only for education, and for non-profit usage !!!
;;; commercial use ist not allowed.
;;; --------------------------------------------------------------------------

(defpackage  :my-packet
    (:use    :common-lisp)
    (:export :search-word
             :writeln
    )
)
(in-package :my-packet)
;; ---------------------------------------------------------------------------
;; some global scoped constants, and variables ...
;; ---------------------------------------------------------------------------
(defconstant *EXIT-FAILURE* 1)
(defconstant *EXIT-SUCCESS* 0)

;; ---------------------------------------------------------------------------
;; WriteLn kann für die Ausgabe von Text am Bildschirm genutzt werden. Es wird
;; zusätzlich zum Text ein Zeilen-Einzug eingefügt.
;; ---------------------------------------------------------------------------
(defun writeln (&rest args)
    (apply #'format t "~{~a~^ ~}~%" args)
)
;; ---------------------------------------------------------------------------
;; einen kleinen aber feinen Copyright-Banner anzeigen - CODEOFCONDUCT :)
;; ---------------------------------------------------------------------------
(writeln (list "LISP DUDEN Deutsch v1.0"))
(writeln (list "(c) 2024 by Jens Kallup - non-profit software"))
(writeln (list "all rights reserved."))
(writeln (list ""))
(writeln (list "Lade Daten, bitte warten..."))

;; ---------------------------------------------------------------------------
;; pre-load requiered packages ...
;; ---------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute) (require :asdf))
(eval-when (:compile-toplevel :load-toplevel :execute) (require :uiop))
(eval-when (:compile-toplevel :load-toplevel :execute) (require :cffi))

;; ---------------------------------------------------------------------------
;; a more robust LISP quit function ...
;; ---------------------------------------------------------------------------
(defun my-quit ()
    (let ((quit-symbol (find-symbol "QUIT" "COMMON-LISP-USER"))
          (exit-symbol (find-symbol "EXIT" "COMMON-LISP-USER")))
    (when (and quit-symbol (fboundp quit-symbol))
          (funcall quit-symbol))
    (when (and exit-symbol (fboundp exit-symbol))
          (funcall exit-symbol))
    (warn "Don't know how to quit!")))

;; ---------------------------------------------------------------------------
;; es wird davon ausgegangen, das die Standard-Installation von QuickLisp sich
;; im Benutzer-Verzeichnis auf der default - Festplatte (c:) befindet ...
;; ---------------------------------------------------------------------------
(defvar *user-quicklisp* (merge-pathnames "quicklisp/setup.lisp" (
    user-homedir-pathname)))
    (if (= (length (namestring *user-quicklisp*)) 0)
        (progn
            (writeln (list "error: not found: Quicklist setup.lisp"))
            (writeln (list "aborted."))
            (my-quit)
        )
    )
(defvar *user-sqlite3* (namestring (probe-file "sqlite3.dll")))
    (if (= (length *user-sqlite3*) 0)
        (progn
            (writeln (list "error: not found: sqlite3.dll"))
            (writeln (list "aborted."))
            (my-quit)
        )
    )
    (writeln (list (probe-file (namestring *user-quicklisp*))))
    (load *user-quicklisp*)
;; ---------------------------------------------------------------------------
;; lade setup.lisp, und öffne Packete - Ausgabe wird an "null" weitergeleitet
;; so dass keine weiteren -Lase-Meldungen am Bildschirm erscheinen ...
;; ---------------------------------------------------------------------------
(uiop:with-null-output (*standard-output*)
    (progn
        (ql:quickload :cffi)        ; sqlite3.dll
        
        (cffi:define-foreign-library sqlite3 (t (:default "sqlite3")))
        (cffi:load-foreign-library 'sqlite3)

        (ql:quickload :cl-ppcre)    ; regular expression regex
        (ql:quickload :qt)          ; qt5
        (ql:quickload :qt-libs)     ; qt5 framework .dll files
        (ql:quickload :sqlite)      ; sqlite3.dll
    )
)
(writeln (list (probe-file "sqlite3.dll")))
;; ---------------------------------------------------------------------------
;; Link für Hörbeispiele auf wiki media ...
;; ---------------------------------------------------------------------------
(defvar *commons-link* "https://upload.wikimedia.org/wikipedia/commons")

;; ---------------------------------------------------------------------------
;; Duden-Nachschlage-Werk erstellen ...
;; ---------------------------------------------------------------------------
(defvar *duden-array* (
    make-array 0
        :adjustable   t
        :fill-pointer 0)
)
;; ---------------------------------------------------------------------------
;; Wörter für den Duden bereitstellen und anschließend sortieren.
;; Legende:
;; --------
;; wa = Wortart,
;; wt = Wort-Trennung,
;; wp = Positiv, wk = Komperativ, ws = Superlativ
;; hb = Hörbeispiel
;; ---------------------------------------------------------------------------
(defvar *duden-woerter* (append
        '("Apfel.s.m.s" )
        '("Sonne.s.w.s" )
        '("sonnen.s.w.p")
        '("manschaft"   )
        '("neutral.wt=neu-t-ral.wa=.wp=neutral.wk=neutraler.ws=neutralsten.hb='/4/48/De-neutral2.ogg'")
    )
)
(setq *duden-woerter* (sort *duden-woerter* #'string<))
;; ---------------------------------------------------------------------------
;; check, if W is in word ...
;; ---------------------------------------------------------------------------
(defun check-w-in-word (word)
    (let (( length-of-word ( length word )))
        (if (>= length-of-word 3)
            (char= (char word (1- length-of-word )) #\W )
            nil
        )
    )
)
;; ---------------------------------------------------------------------------
;; suchen eines Wortes im Duden-Wörterbuch ...
;; ---------------------------------------------------------------------------
(defun search-word (pattern)
    (let ((matches (remove-if-not (lambda (word)
         (cl-ppcre:scan pattern word))
         *duden-woerter*)))
    (if matches
        (format t "Übereinstimmungen gefunden: ~{~a~^, ~}~%}" matches)
        (format t "Keine Übereinstimmungen gefunden.~%"))
    )
)
;; ---------------------------------------------------------------------------
;; gesammelte Wörter in den Duden einfügen
;; ---------------------------------------------------------------------------
(loop for wort in *duden-woerter*
    do (
        vector-push-extend wort *duden-array*
    )
)
;; ---------------------------------------------------------------------------
;; GUI-Anwendung vorbereiten ...
;; ---------------------------------------------------------------------------
(defpackage :lispide
    (:use   :cl :qt)
    (:export #:main))
;; ---------------------------------------------------------------------------
(in-package :lispide)
(named-readtables:in-readtable :qt)

(defun button-click ()
    (my-packet:writeln (list "hhhhh")))

;; ---------------------------------------------------------------------------
;; start() ist unsere Haupt-Einstiegs-Routine ...
;; ---------------------------------------------------------------------------
(defun start()
    (let* ((app (make-qapplication))
            ;; --------------------------------------------------
            ;; Instanzen der Formular-Elemente erstellen ...
            ;; --------------------------------------------------
            (menubar     (#_new QMenuBar))
            (menu-file   (#_new QMenu "File"))
            (menu-help   (#_new QMenu "Edit"))
            
            ;; menu - new
            (menu-file-new         (#_new QWidgetAction menu-file))
            (menu-file-new-widget  (#_new QWidget))
            (menu-file-new-layout  (#_new QHBoxLayout menu-file-new-widget))
            ;;
            (menu-file-new-label   (#_new QLabel "New..."))
            
            
            ;; menu - exit
            (menu-file-exit        (#_new QWidgetAction menu-file))
            (menu-file-exit-widget (#_new QWidget))
            (menu-file-exit-layout (#_new QHBoxLayout menu-file-exit-widget))
            ;;
            (menu-file-exit-label  (#_new QLabel "Exit"))
            
            
            ;; --------------------------------------------------
            ;; controls on left side ...
            ;; --------------------------------------------------
            (font        (#_new QFont "Consolas"))
            (widget      (#_new QWidget))
            
            (label-out   (#_new QLabel "Processed output:"))
            (label-in    (#_new QLabel "Please type a word:"))
            
            (editfield   (#_new QLineEdit "hallo"))
            (textview    (#_new QTextEdit))
            
            (button      (#_new QPushButton "Eval"))
            
            
            ;; --------------------------------------------------
            ;; controls on right side ...
            ;; --------------------------------------------------
            (label-eval  (#_new QLabel "Evaluation Code:"))
            (tab-eval    (#_new QTabBar widget))
            ;;
            (tab-eval-content-1 (#_new QWidget tab-eval))
            (tab-eval-content-2 (#_new QWidget tab-eval))
            ;;
            (textview-eval-1  (#_new QTextEdit tab-eval-content-1))
            (textview-eval-2  (#_new QTextEdit tab-eval-content-2))
            ;;
            (button-eval      (#_new QPushButton "Eval"))
            
            (main-window (#_new QMainWindow))
        )
        
        ;; --------------------------------------------------
        ;; setze das Widget als Hauptwidget des Hauptfensters
        ;; --------------------------------------------------
        (#_setCentralWidget main-window  widget)
        
        ;; --------------------------------------------------
        ;; Schriftart setzen der Menu-Einträge ...
        ;; --------------------------------------------------
        (#_setFont menubar   font)
        (#_setFont menu-file font)
        (#_setFont menu-help font)
        
        ;; --------------------------------------------------
        ;; label - new
        ;; --------------------------------------------------
        (#_addWidget menu-file-new-layout  menu-file-new-label)
        (#_setLayout menu-file-new-widget  menu-file-new-layout)
        ;;
        (#_setDefaultWidget menu-file-new  menu-file-new-widget)
        
        ;; --------------------------------------------------
        ;; label - exit
        ;; --------------------------------------------------
        (#_addWidget menu-file-exit-layout menu-file-exit-label)
        (#_setLayout menu-file-exit-widget menu-file-exit-layout)
        ;;
        (#_setDefaultWidget menu-file-exit menu-file-exit-widget)
        
        
        ;; --------------------------------------------------
        ;; jedem Menü-Eintrag den gleichen Schriftzug geben:
        ;; --------------------------------------------------
        (#_setFont  menu-file-new-label  font)
        (#_setFont  menu-file-exit-label font)
        
        ;; --------------------------------------------------
        ;; Untermenüs an das "parent" Menü kleben ...
        ;; --------------------------------------------------
        (#_addAction menu-file menu-file-new)
        (#_addAction menu-file menu-file-exit)
        
        ;; --------------------------------------------------
        ;; "parent" Menüs an die menuBar kleben ...
        ;; --------------------------------------------------
        (#_addMenu menubar menu-file)
        (#_addMenu menubar menu-help)
        
        ;; --------------------------------------------------
        ;; Menü-Events zuweisen ...
        ;; --------------------------------------------------
        (connect menu-file-new  "triggered()" #'button-click)
        (connect menu-file-exit "triggered()" #'button-click)
        
        ;; --------------------------------------------------
        ;; last but not least: add menuBar to form ...
        ;; --------------------------------------------------
        (#_setMenuBar main-window menubar)
        
        ;; --------------------------------------------------
        ;; setze die Elemente als Kindwidget des Widget
        ;; --------------------------------------------------
        (#_setParent label-out     widget)
        (#_setParent label-in      widget)
        ;;
        (#_setParent editfield     widget)
        (#_setParent textview      widget)
        (#_setParent button        widget)
        
        
        (#_setParent label-eval    widget)
        (#_setParent button-eval   widget)
        ;;
        (let ((tab-1 (#_addTab tab-eval "Eval" )))
             (connect tab-eval "currentChanged()" #'button-click) ;;((lambda () (
                ;;(#_hide textview-eval-2)
                ;;(#_show textview-eval-1)
             ;;))))
        )
        (let ((tab-2 (#_addTab tab-eval "Debug")))
             ;;(connect tab-eval "currentChanged()" ((lambda () (
             ;;   (#_hide textview-eval-1)
             ;;   (#_show textview-eval-2)
             ;;))))
        )
        ;;
        
        
        ;; --------------------------------------------------
        ;; Position der Elemente werden statisch vergeben:
        ;; --------------------------------------------------
        (#_move label-out  5   5)
        (#_move label-in   5 235)
        
        (#_move textview   5  25)
        (#_move editfield  5 260)
        (#_move button     5 300)
        
        (#_move label-eval   400   5)
        (#_move tab-eval   400  25)
        (#_move button-eval  400 300)

        (#_setPointSize font 11)
        
        ;; --------------------------------------------------
        ;; einheitlicher Festbreiten-Font verwenden ...
        ;; --------------------------------------------------
        (#_setFont label-out font)
        (#_setFont label-in  font)
        
        (#_setFont textview  font)
        (#_setFont editfield font)
        (#_setFont button    font)
        
        (#_setFont label-eval  font)
        (#_setFont tab-eval    font)
        (#_setFont button-eval font)
        
        ;; --------------------------------------------------
        ;; Größenanpassungen vornehmen
        ;; --------------------------------------------------
        (#_resize main-window 800 400)
        (#_resize editfield   380  26)
        (#_resize textview    380 200)
        (#_resize button      100  32)
        ;;
        (#_resize tab-eval 400 260)
        (#_resize button-eval 100  32)
        
        ;; --------------------------------------------------
        ;; Button Event zuweisen ...
        ;; --------------------------------------------------
        (connect button "clicked()" #'button-click)
        
        ;; --------------------------------------------------
        ;; sanity: Objekte sichtbar machen/darstellen ...
        ;; --------------------------------------------------
        (#_show main-window)
        (#_show widget)
        (#_show label-out)
        (#_show label-in)
        (#_show editfield)
        (#_show textview)
        (#_show button)
        
        (#_show tab-eval)
        (#_show label-eval)
        ;;(#_show textview-eval)
        (#_show button-eval)
        
        ;; --------------------------------------------------
        ;; Anwendungs - Loop ...
        ;; --------------------------------------------------
        (unwind-protect (#_exec app)
        (#_hide widget))
        (my-packet:search-word "sonne")
    )
)
;; ---------------------------------------------------------------------------
;; Start-Routine aufrufen ...
;; ---------------------------------------------------------------------------
(start)

;;(writeln *duden-woerter*)
