getDivisors := proc(value::integer)
  local divisors := NumberTheory[Divisors](value);
  local i;
  for i from 1 to nops(divisors) do
    divisors := [op(divisors), (-1) * divisors[i]];
  end do;
  return divisors;
end proc:
getDivisors(4);
getDivisors(5);

# --------------------

chooseMplus1Integers := proc(m::integer)
  if(evalb(m < 1)) then 
    return Error("Please give me an m >= 1");
  end if;

  local myIntegers := [0, 1];
  if(evalb(m > 1)) then
    local mplus1 := m + 1;
    while(evalb(nops(myIntegers) < mplus1)) do
      local lastValue := myIntegers[nops(myIntegers)];
      if(evalb(lastValue > 0)) then
         myIntegers := [op(myIntegers), (-1) * lastValue];
      elif(evalb(lastValue < 0)) then
         myIntegers := [op(myIntegers), abs(lastValue) + 1];
      end if;
    end do;
  end if;
  
  return myIntegers;

end proc:

for k from 0 to 20 do
   chooseMplus1Integers(k);
end do;

# --------------------

generate_permutations := proc(sets, current_permutation, index)
  local num_sets, current_set, current_element, all_permutations, current_result;

  num_sets := nops(sets);
  all_permutations := [];

  if index > num_sets then
    # Base case: all sets processed, add the permutation to the list
    return [current_permutation];
  else
    # Recursive case: iterate over the current set
    current_set := sets[index];
    for current_element in current_set do
      # Update the permutation and move to the next set
      current_result := generate_permutations(sets,[op(current_permutation),current_element], index + 1);
      all_permutations := [op(all_permutations), op(current_result)];
    end do;
  end if;

  return all_permutations;
end proc:

# Example usage with n sets
sets := [{-1, 1, -2, 2, -4, 4}, {-1, 1, -5, 5}, {-1, 1, -5, 5}];
sets[1]; sets[2]; sets[3];
# Call the function to generate permutations and get the result
permutations := generate_permutations(sets, [], 1);
nops(permutations);
6 * 4 * 4;

# --------------------

SKFactor := proc(polynomial, _var)
  options remember;
    
  local r := polynomial;                           # the unfactored portion of p
  local pf := 1;                                    # the factored portion of p
  local m := floor(degree(r) / 2);   
  local mplus1 := m + 1;
  local mplus1Integers := chooseMplus1Integers(m);        # choose m plus 1 integers 

  local Di := [];                                   # Divisors sets
  
    local i;
    for i from 1 to mplus1 do
        local ai := mplus1Integers[i];
        local p_ai := eval(r, x=ai);
       if(evalb(p_ai = 0)) then
         local p := x - ai;
         while (evalb(rem(r, p, x) = 0)) do
          pf := pf * p;
          r := quo(r, p, x);
         end do;
       else
        local p_ai_divisors := {op(getDivisors(p_ai))};
        Di := [op(Di), p_ai_divisors];   
      end if;
    
    end do;
  
   m := floor(degree(r) / 2);
   local d;
   for d from 1 to m do

     local dplus1 := d + 1;
     local setOfdPlus1Values := generate_permutations(Di, [], 1);

     local j;
     local polynomial_form := y = reduce(`+`, [seq(c[j]*x^j, j=0..dplus1)]);
     
     local k;
     for k from 1 to nops(setOfdPlus1Values) do

        local subsetOfdplus1Values := setOfdPlus1Values[k];
        
        local equations := {};
        local l;
        for l from 1 to nops(subsetOfdplus1Values) do
           local eq := subs({x=mplus1Integers[l],y=subsetOfdplus1Values[l]}, polynomial_form);
           equations := equations union {eq};
        end do;
        
        local soln := solve(equations);
        local qsoln := rhs(subs(soln, polynomial_form));

        while (evalb(rem(r, qsoln, x) = 0)) do
           pf := pf * qsoln;
           r := quo(r, qsoln, x);
           if(evalb(qsoln = 1)) then
             return normal(pf * r);
           end if;
           if(evalb(degree(r) < 2*(d+1))) then   # 2*(d+1) was working
              return normal(pf * r);
           else
              break;
           end if;
        end do;

        #print("===================================================");

     end do;

     if(evalb(degree(r) < 2*(d + 1))) then
        return normal(pf * SKFactor(r, _var)); 
     end if;  

  end do;

end proc:

#########################################################################################
f := x^4 + 4;                         # GOOD
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := x^5 + x + 1;                     # GOOD
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := x^6 + 2*x^4 + 4*x^3 + x^2 + 4*x + 4;        # GOOD   
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := x^8 -2*x^6 + 2*x^2 - 1;                     # GOOD
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := x^6 - 1;                                    # GOOD
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := x^5 - x + 1;                                # Good
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := 2*x^4 + 4*x^3 + 2*x + 8;                    # GOOD
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);

f := 25*x^6 - 16;                                ## ???
myAnswer := SKFactor(f, x);
mapleAnswer := factor(f);