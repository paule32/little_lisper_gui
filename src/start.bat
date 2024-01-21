:: ---------------------------------------------------------------------------
:: Datei:  start.bat   - Windows MS-DOS batch file to execute start script.
:: Author: Jens Kallup - paule32
::
:: Rights: (c) 2024 by kallup non-profit software
::         all rights reserved
::
:: only for education, and for non-profit usage !!!
:: commercial use ist not allowed.
:: ---------------------------------------------------------------------------
@echo off

set BASEDIR=%cd%
set SBCL_PATH=E:\SBCL\sbcl.exe
set LISP_SCRIPT=%BASEDIR%\german.lisp

%SBCL_PATH% --script %LISP_SCRIPT%
