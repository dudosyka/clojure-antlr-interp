antlr -o ./temp -package com.grammar -listener -visitor -lib ./ Grammar.g4
javac -d ./classes ./temp/Grammar*.java -cp "./antlr.jar"
jar -cvfm Grammar.jar ./Manifest.mf -C classes .