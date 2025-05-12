rm -R classes
mkdir classes
clojure -M -e "(compile 'compiler)"
bb -cp $(clojure -Spath) uberjar Compiler.jar -m compiler