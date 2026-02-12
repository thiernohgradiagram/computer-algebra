(* A Procedure called DigitList that takes a non-negative integer as argument and return a list of the decimal digits in the number. 
What should we do when the number we pass to the procedure contains leading zeros ???? :)
*)
DigitList := proc(number::nonnegint)
  local numberToString := convert(number, string);
  local stringToAsciiCodeArray := [];
  
  with(StringTools):
  stringToAsciiCodeArray := map(Ord, Explode(numberToString));

  local element;
  local result := [];
  for element in stringToAsciiCodeArray do
    result := [op(result), irem(element, 48)];
  end do;

  return result;
end proc:

# --------------------

DigitList(000123456789000);
DigitList(0123456789000);
DigitList(333345345355123456789000);
DigitList(000123456789000099999999999999999991158);