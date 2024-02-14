Поиск n-ого числа Фибоначчи, где n -- значение в rcx
  $ ./demoInterpreter.exe <<EOF
  > k = 10
  > i = 1 
  > n = 1 
  > while i <= k do 
  >   n = n * i 
  >   i = i + 1 
  > end
  { vars =
    ["i": (Exp_number 11.),
     "k": (Exp_number 10.),
     "n": (Exp_number 3628800.),
     ];
    last_value = Exp_nil; is_loop = false; jump_stmt = Default }
