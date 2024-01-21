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
    (:export :search-word)
)
(in-package :my-packet)
;; ---------------------------------------------------------------------------
;; WriteLn kann für die Ausgabe von Text am Bildschirm genutzt werden. Es wird
;; zusätzlich zum Text ein Zeilen-Einzug eingefügt.
;; ---------------------------------------------------------------------------
(defun writeln (&rest args)
    (apply #'format t "~{~a~^ ~}~%" args)
)

;; ---------------------------------------------------------------------------
;; einen kleinen aber feinen Copyright-Banner anzeigen - CODEOFCONDUCT :-D
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
  
;; ---------------------------------------------------------------------------
;; es wird davon ausgegangen, das die Standard-Installation von QuickLisp sich
;; im Benutzer-Verzeichnis auf der default - Festplatte (c:) befindet ...
;; ---------------------------------------------------------------------------
(defvar *user-quicklisp* (merge-pathnames "quicklisp/setup.lisp" (
    user-homedir-pathname)))
    (load *user-quicklisp*)
;; ---------------------------------------------------------------------------
;; lade setup.lisp, und öffne Packete - Ausgabe wird an "null" weitergeleitet
;; so dass keine weiteren -Lase-Meldungen am Bildschirm erscheinen ...
;; ---------------------------------------------------------------------------
(uiop:with-null-output (*standard-output*)
    (progn
        (ql:quickload :cl-ppcre)
        (ql:quickload :qt)
        (ql:quickload :qt-libs)
    )
)
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

;; ---------------------------------------------------------------------------
;; start() ist unsere Haupt-Einstiegs-Routine ...
;; ---------------------------------------------------------------------------
(defun start()
    (let* ((app (make-qapplication))
            ;; --------------------------------------------------
            ;; Instanzen der Formular-Elemente erstellen ...
            ;; --------------------------------------------------
            (font        (#_new QFont "Consolas"))
            (widget      (#_new QWidget))
            
            (label       (#_new QLabel "Please type a word:"))
            (editfield   (#_new QLineEdit "hallo"))
            (textview    (#_new QTextEdit))
            
            (button      (#_new QPushButton "Eval"))
            (main-window (#_new QMainWindow))
        )
        
        ;; --------------------------------------------------
        ;; setze das Widget als Hauptwidget des Hauptfensters
        ;; --------------------------------------------------
        (#_setCentralWidget main-window  widget)
        
        ;; --------------------------------------------------
        ;; setze die Elemente als Kindwidget des Widget
        ;; --------------------------------------------------
        (#_setParent label     widget)
        (#_setParent editfield widget)
        (#_setParent textview  widget)
        (#_setParent button    widget)
        
        ;; --------------------------------------------------
        ;; Position der Elemente werden statisch vergeben:
        ;; --------------------------------------------------
        (#_move label     5  5)
        (#_move editfield 5 25)
        (#_move textview  5 55)
        (#_move button   40  5

        (#_setPointSize font 11)
        
        ;; --------------------------------------------------
        ;; einheitlicher Festbreiten-Font verwenden ...
        ;; --------------------------------------------------
        (#_setFont label     font)
        (#_setFont editfield font)
        (#_setFont textview  font)
        (#_setFont button    font)
        
        ;; --------------------------------------------------
        ;; Größenanpassungen vornehmen
        ;; --------------------------------------------------
        (#_resize main-window 640 480)
        (#_resize textview    400 200)
        (#_resize button      100  25)
        
        ;; --------------------------------------------------
        ;; sanity: Objekte sichtbar machen/darstellen ...
        ;; --------------------------------------------------
        (#_show main-window)
        (#_show widget)
        (#_show label)
        (#_show editfield)
        (#_show textview)
        (#_show button)
        
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
