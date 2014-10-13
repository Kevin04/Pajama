@echo off
REM compila modelo y compilador
javac -cp .;lib;js.jar;%CLASSPATH%  -d lib src/js/*.java src/compiler/*.java 
