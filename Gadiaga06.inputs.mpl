getLastCharacter := proc(base::integer)
  # this procedure finds the last character of a given base [2 - 36]
  if(evalb(base < 2) or evalb(base > 36)) then
    return Error("Bases should be between 2 and 36 inclusive");
  end if;
  local bases := [seq(0..9),A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z];
  return bases[base];
end proc:

getLastCharacter(0);
getLastCharacter(1);
getLastCharacter(2);
getLastCharacter(10);
getLastCharacter(11);
getLastCharacter(12);
getLastCharacter(13);
getLastCharacter(20);

# --------------------

getBaseFromLastCharacter := proc(character::string)
  # this procedure finds the base given the last character of the base
  local bases := [seq(0..9),A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z];
  local i;
  for i from 1 to nops(bases) do
     if ( evalb(character = String(bases[i])) ) then
       return i;
     end if;
  end do;
end proc:

getBaseFromLastCharacter("9");
getBaseFromLastCharacter("A");
getBaseFromLastCharacter("B");
getBaseFromLastCharacter("C");
getBaseFromLastCharacter("J");
getBaseFromLastCharacter("2");
getBaseFromLastCharacter("1");
getBaseFromLastCharacter("0");

# --------------------

toBaseXFromBase10 := proc(b10Number::nonnegint, baseX::integer)
  
  if(evalb(baseX < 2) or evalb(baseX > 36)) then
    return Error("Bases should be between 2 and 36 inclusive");
  end if;

  if(evalb(b10Number = 0)) then 
     return [0];
  end if;
  
  local base10Number := b10Number;
  local numberToBaseX := [];

  while( not (evalb(base10Number = 0)) ) do
     local remainder := base10Number mod baseX;    
     numberToBaseX := [remainder, op(numberToBaseX)];
     base10Number := floor(base10Number / baseX);
  end do;

  if(evalb(baseX > 10)) then
    local i;
    for i from 1 to nops(numberToBaseX) do
       if (evalb(numberToBaseX[i] > 9)) then
          numberToBaseX[i] := getLastCharacter(numberToBaseX[i] + 1);
       end if;
    end do; 
  end if; 

  return numberToBaseX;
end proc:

toBaseXFromBase10(127, 367);
toBaseXFromBase10(127, 0);
toBaseXFromBase10(127, 37);
toBaseXFromBase10(0, 47777);
toBaseXFromBase10(3, 1);
toBaseXFromBase10(127, 36);
toBaseXFromBase10(127, 35);
toBaseXFromBase10(1, 35);
toBaseXFromBase10(0, 35);
toBaseXFromBase10(99, 36);
toBaseXFromBase10(35, 36);
toBaseXFromBase10(36, 36);
toBaseXFromBase10(929, 10);
toBaseXFromBase10(929, 11);

# --------------------

toBase10FromBaseX := proc(numb::list, baseX::integer)
   
   if(evalb(baseX < 2) or evalb(baseX > 36)) then
    return Error("Bases should be between 2 and 36 inclusive");
   end if;
   
   local number := [op(numb)];
   local lengthOfNumber := nops(number);
   if(evalb(baseX > 10)) then
     local j;
     for j from 1 to lengthOfNumber do
       local baseCode := getBaseFromLastCharacter(String(number[j]));
       if( evalb(baseCode > 10) and evalb(baseCode < 37) ) then
         number[j] := baseCode - 1;
       end if;
     end do;  
   end if;

   local numberToBase10 := 0;
   local i;
   for i from lengthOfNumber to 1 by -1 do
     numberToBase10 := numberToBase10 + number[i] * (baseX)**( lengthOfNumber - i );
   end do;
   return numberToBase10;
end proc:

toBase10FromBaseX([0,1,1,2,0,1], 3);
toBase10FromBaseX([3, J], 36);
toBase10FromBaseX([0, 0], 34);
toBase10FromBaseX([0], 36);

# --------------------

ChangeBase := proc(number::list, base1::posint, base2::posint)

 if(evalb(base1 < 2) or evalb(base1 > 36) or evalb(base2 < 2) or evalb(base2 > 36)) then
    return Error("Bases should be between 2 and 36 inclusive");
 end if;
 
 # validation: an element of number cannot be greater than the base
 local charCodeOflastCharacterOfBase1 := Ord(String(getLastCharacter(base1))); 
 local element;
 for element in number do
   local charCodeOfCurrentElement := Ord(String(element));
   if (evalb( charCodeOfCurrentElement > charCodeOflastCharacterOfBase1)) then
     return Error("Base and number mismatch"); 
   end if;
 end do;

 if (evalb(base1 = base2)) then
   return number;
 else
   local numberToBase10 := toBase10FromBaseX(number, base1);
   return toBaseXFromBase10(numberToBase10, base2);
 end if
end proc:

ChangeBase([1,2,7], 10, 10);
ChangeBase([1,2,7], 10, 3);
ChangeBase([1,1,2,0,1], 3, 10);
ChangeBase([1,2,7], 10, 2);

ChangeBase([1, 1, 1, 1, 1, 1, 1], 2, 10);
ChangeBase([1, 1, 1, 1, 1, 1, 1], 2, 3);
ChangeBase([1,1,2,0,1], 3, 2);
ChangeBase([2,4,6], 10, 16);
ChangeBase([3,J], 20, 10);
ChangeBase([3,I], 20, 10);
ChangeBase([9,9], 2, 3);
ChangeBase([3,K], 20, 10);
ChangeBase([3], 10, 1);
ChangeBase([0, 0, 0], 1, 1);

ChangeBase([9,S], 34, 6);
ChangeBase([1, 3, 1, 4], 6, 34);
ChangeBase([0,0], 10, 2);
ChangeBase([0], 4, 5);