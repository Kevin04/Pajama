@echo off
REM Prueba una caso de prueba
java -cp .;lib;js.jar;%CLASSPATH% pajama.compile.Pajamac cases\%1
