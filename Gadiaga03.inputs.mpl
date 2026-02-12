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

AddInteger(DigitList(2000), DigitList(900000));
AddInteger(DigitList(2000), DigitList(2000));
AddInteger(DigitList(100!), DigitList(300!));

# --------------------

SubInteger := proc(numb1, numb2)
  
  local result := [];
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

    if (evalb(CompInt(number1, number2) = 1)) then

       local k;
       local borrow := 0;
       for k from nops(number1) to 1 by -1 do
         local columnSubPlusBorrow := (number1[k] - number2[k] + borrow);
         local wk :=  columnSubPlusBorrow mod 10;
         result := [wk, op(result)];

         if evalb(columnSubPlusBorrow < 0) then
           borrow := -1;
         else 
           borrow := 0;
         end if;
       end do;

    else
      return [0];
    end if;

  return result;
end proc:

SubInteger(DigitList(9999), DigitList(1111));
SubInteger(DigitList(9999), DigitList(9999));
SubInteger(DigitList(9999), DigitList(999));
SubInteger(DigitList(1111), DigitList(9999));
SubInteger(DigitList(100!), DigitList(111!));
SubInteger(DigitList(23!), DigitList(22!));
SubInteger(DigitList(2), DigitList(22!));

# --------------------

MultInteger := proc(num1, num2)
    local num_1, num_2, k, w, len_diff, resulting_list, diff, i, j, n, multiplicant, total_results_length;

    num_1 := num1;
    num_2 := num2;
    k := 0;
    w := 0;
    len_diff := 0;
    resulting_list := [];
    diff := 0;
    i := 0;
    j := 0;
    total_results_length := 0;

    if parse(cat(op(num_1))) = 0 or parse(cat(op(num_2))) = 0 then
        return [0];
    end if;

    if numelems(num_2) <= numelems(num_1) then
        total_results_length := 2 * numelems(num_1);
    else
        total_results_length := 2 * numelems(num_2);
    end if;

    len_diff := numelems(num_1) - numelems(num_2);
    diff := abs(len_diff);

    if numelems(num_2) < numelems(num_1) then
        num_2 := pad(num_2, diff);
    elif numelems(num_1) < numelems(num_2) then
        num_1 := pad(num_1, diff);
    end if;

    num_1 := reverse(num_1);
    num_2 := reverse(num_2);

    for i to total_results_length do
        resulting_list := [0, op(resulting_list)];
    end do;

    resulting_list := Array(resulting_list);

    for i to numelems(num_1) do
        n := i;
        k := 0;

        for j to numelems(num_2) do
            multiplicant := num_1[i] * num_2[j] + resulting_list[n] + k;
            resulting_list[n] := multiplicant mod 10;
            k := floor(1 / 10 * multiplicant);
            n := n + 1;
        end do;

        resulting_list[n] := k;
    end do;

    return RemoveLeadingZeros(reverse(resulting_list));
end proc:

MultInteger(DigitList(10), DigitList(50));
MultInteger(DigitList(90!), DigitList(50!));

# --------------------

DivInteger := proc(u, v)
    local u_list, v_list, q, r, leading_divisor;
    local d, u_value, v_value, u_, v_, new_v_value, j;
    local temp, n, k, u_n, u_til_list, w, c_list, u_til;

    u_list := u;
    v_list := v;
    q := [];
    r := 0;
    leading_divisor := v_list[1];
    d := floor((leading_divisor + 1)/2);
    u_value := parse(cat(op(u_list)));
    v_value := parse(cat(op(v_list)));
    u_ := DigitList(u_value*d);
    v_ := DigitList(v_value*d);
    new_v_value := parse(cat(op(v_)));
    j := 0;
    temp := 0;
    n := nops(v_);
    k := 0;
    u_n := 0;
    u_til_list := [];
    w := 1;
    c_list := [];
    u_til := 0;

    if new_v_value = 0 then
        error "Divisor must be greater than 0";
    end if;

    if u_value < v_value then
        return [0, u_value];
    end if;

    for k to n do
        u_til_list := [op(u_til_list), u_[k]];
    end do;

    u_til := parse(cat(op(u_til_list)));
    u_n := u_til_list[1];

    for j from nops(u_) by -1 to 0 do
        if u_n = v_[1] then
            temp := 9;
        else
            temp := floor((10*u_n + u_[n])/v_list[1]);
        end if;

        w := temp*new_v_value;

        while u_til < w do
            temp := temp - 1;
            w := w - new_v_value;
        end do;

        q := [op(q), temp];
        r := u_til - w;

        if n < nops(u_) then
            n := n + 1;
            u_til := 10*r + u_[n];
            c_list := DigitList(u_til);
            u_n := c_list[1];
        else
            r := r/d;
            break;
        end if;
    end do;
    return [parse(cat(op(q))), r];
end proc:

DivInteger(DigitList(150), DigitList(2));
DivInteger(DigitList(150), DigitList(150));
DivInteger(DigitList(1000), DigitList(3000));
DivInteger(DigitList(1111), DigitList(2));

# --------------------

GcdInteger :=proc(number1::list, number2::list)
  local numb1:= DigitListToInt(number1);
  local numb2:= DigitListToInt(number2);
  local u := abs(numb1);
  local v := abs(numb2);
  if (evalb(u = 0) or evalb(v = 0)) then
    return max(u, v);
  end if; 
  local g := 1;
  while(evalb((u mod 2) = 0) and evalb((v mod 2) = 0)) do
    u := u / 2;
    v := v / 2;
    g := 2 * g;
  end do;
  while (evalb(u > 0) and evalb(not (v = 1))) do
     while(evalb((u mod 2) = 0)) do
       u := u / 2;
     end do;
     while(evalb((v mod 2) = 0)) do
       v := v / 2;
     end do;
     local t := abs(u - v);
     v := min(u, v);
     u := t;
  end do;
  return DigitList(g * v);
end proc:

GcdInteger(DigitList(7000), DigitList(4400));
GcdInteger(DigitList(4400), DigitList(7000));
GcdInteger(DigitList(10!), DigitList(16!));
DigitList(gcd(10!, 16!));

# --------------------

RemoveLeadingZeros := proc(num_list) 
 local number_list;
 number_list := [];
 number_list := num_list;
 while number_list[1] = 0 do
   number_list := subsop(1 = NULL, number_list);
 end do; 
 return number_list;
end proc:

# --------------------

pad := proc(num, pads)
 local i, new_nums; 
 i := 0;
 new_nums := num;
 for i to pads do 
    new_nums := [0, op(new_nums)];
 end do;
 return new_nums;
end proc:

# --------------------

reverse := proc(list)
 local reverseList, total, i; reverseList := []; 
 total := numelems(list);
 i := 1;
 while i <= total do 
    reverseList := [list[i], op(reverseList)];
    i := i + 1;
 end do;
 return reverseList;
end proc:

# --------------------

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

DigitList(2023);

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

CompInt := proc(number1, number2)
 
  local i;
  for i from 1 to nops(number1) do
    if(evalb(number1[i] < number2[i])) then
      return -1;
    elif(evalb(number1[i] > number2[i])) then
      return 1;
    end if;
  end do;
  return 0;
end proc:
CompInt(DigitList(2000), DigitList(2000));
CompInt(DigitList(3000), DigitList(2000));
CompInt(DigitList(2000), DigitList(3000));