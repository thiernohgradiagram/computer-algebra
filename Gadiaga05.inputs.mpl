DigitList := proc(number::nonnegint)
  local numberToString := convert(number, string);
  local stringToAsciiCodeArray := [];
  with(StringTools):
  stringToAsciiCodeArray := map(Ord, Explode(numberToString));
  local result := [];
  local element;
  for element in stringToAsciiCodeArray do
    result := [op(result), irem(element, 48)];
  end do;
  return result;
end proc:

DigitList(02099);

# --------------------

DigitListToInt := proc(array)
   local result := "";
   local element;
   for element in array do 
   result := cat(result, convert(element, 'string'));
   end do;
   return parse(result);
end proc:

DigitListToInt([0,0,1,2,3,4,5,6,7,8,9,0, 1, 1]);

# --------------------

isPowerOf2 := proc(n::integer)
   if (evalb(n = 0)) then 
     return false;
   end if;
 return evalb( ceil(log(n) / log(2)) = floor( log(n) / log(2)) );
end proc:

isPowerOf2(44);
isPowerOf2(16);

# --------------------

AddInteger := proc(numb1, numb2)
  local number1 := [op(numb1)];
  local number2 := [op(numb2)];
  local lengthOfNumber1 := nops(number1);
  local lengthOfNumber2 := nops(number2);
  local number1IsShorterThanNumber2 := evalb((lengthOfNumber1 < lengthOfNumber2));
  local number2IsShorterThanNumber1 := evalb((lengthOfNumber2 < lengthOfNumber1));
  
  if number1IsShorterThanNumber2 then
    local i;
    for i from 1 to (lengthOfNumber2 - lengthOfNumber1) do
      number1 := [0, op(number1)];
    end do;
  elif number2IsShorterThanNumber1 then
    local j;
    for j from 1 to (lengthOfNumber1 - lengthOfNumber2) do
      number2 := [0, op(number2)];
    end do; 
  end if;
  
  local k;
  local carry := 0;
  local result := [];
  for k from nops(number1) to 1 by -1 do
     local columnSumPlusCarry := (number1[k] + number2[k] + carry);
     local wk :=  columnSumPlusCarry mod 10;
     result := [wk, op(result)];
     if evalb(columnSumPlusCarry >= 10) then
       carry := 1;
     else 
       carry := 0;
     end if
  end do;

  if evalb((carry = 1)) then
    result := [carry, op(result)];
  end if;

  return result;
end proc:

AddInteger([2,0,0,0], [2,0,0,0]);

# --------------------

Kmult := proc(number1::list, number2::list)
  option remember;
  local u := [op(number1)];
  local v := [op(number2)];
  local lengthOfU := nops(u);
  local lengthOfV := nops(v);

  if(evalb(lengthOfU < lengthOfV)) then
    local i;
    for i from 1 to (lengthOfV - lengthOfU) do
      u := [0, op(u)];
    end do;
  elif (evalb(lengthOfV < lengthOfU)) then
    local j;
    for j from 1 to (lengthOfU - lengthOfV) do
      v := [0, op(v)];
    end do; 
  end if;

  while(not isPowerOf2(lengthOfU)) do
     u := [0, op(u)];
     v := [0, op(v)];
     lengthOfU := lengthOfU + 1;
     lengthOfV := lengthOfV + 1;
  end do;

  local n := lengthOfU;

  if(evalb(lengthOfU = 1) and evalb(lengthOfV = 1)) then
    return DigitList(op(1, u) * op(1, v));
  end if; 

  local halfWay := n / 2; 
  local halfWayPlus1 := halfWay + 1;
  local index;
  local u1 := [seq(u[index], index=1..halfWay)];
  local u0 := [seq(u[index], index=halfWayPlus1..n)];
  local v1 := [seq(v[index], index=1..halfWay)];
  local v0 := [seq(v[index], index=halfWayPlus1..n)];

  local p1 := Kmult(u1, v1);                                 # p1 =(u1 * v1) * 10^n
  local k;
  for k from 1 to n do
    p1 := [op(p1), 0];
  end do;
  
  local u1v0 := Kmult(u1, v0);                               # (u1 * v0)
  local u0v1 := Kmult(u0, v1);                               # (u0 * v1)
  local sum := AddInteger(u1v0, u0v1);                       # (u1 * v0) + (u0 * v1) * 10^(n/2)
  local s;
  for s from 1 to halfWay do
    sum := [op(sum), 0];
  end do;

  local p2 := Kmult(u0, v0);                                 # p2 =(u0 * v0)
  local p1PlusSum := AddInteger(p1, sum);
  local w := AddInteger(p1PlusSum, p2);
  return DigitList(DigitListToInt(w));                       # w = p1 + sum + p2
end proc:

DigitList(49000 * 240);
Kmult(DigitList(49000), DigitList(240));
DigitList(150 * 2);
Kmult(DigitList(150), DigitList(2));
DigitList(1500 * 200);
Kmult(DigitList(1500), DigitList(200));
DigitList(0 * 0);
Kmult(DigitList(0), DigitList(0));
DigitList(0 * 45);
Kmult(DigitList(0), DigitList(45));
DigitList(11 * 11);
Kmult(DigitList(11), DigitList(11));
DigitList(10! * 5!);
Kmult(DigitList(10!), DigitList(5!));