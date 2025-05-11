rm -R classes
mkdir classes
clojure -M -e "(compile 'interpret)"
bb -cp $(clojure -Spath) uberjar Compiler.jar -m interpret