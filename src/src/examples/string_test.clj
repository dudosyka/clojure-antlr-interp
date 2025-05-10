(let [a "abc"
      b "def"
      c 1]
  (str a b (if (= 1 c) "true" "false")))