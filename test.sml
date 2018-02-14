fun switch s =
  substring(s, size s div 2, size s div 2) ^ substring(s, 0, size s div 2);

switch "tests";
