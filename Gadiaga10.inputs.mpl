CoeffList := proc(u::anything)

  if( type(u, polynom(rational, x)) ) then

    if(evalb(u = 0)) then
        return [];
    end if;
    
    local u_degree := degree(u);
    local u_length := u_degree + 1;

    local i;
    local u_coeffListed := [seq(0, i=1..u_length)];
   
    if(evalb(nops([coeffs(u, x)]) = 1)) then
       local currentTerm_degree := degree(u);
       local currentTerm_coefficient := coeffs(u, x);
       u_coeffListed[currentTerm_degree + 1] := currentTerm_coefficient;
       return u_coeffListed;
    end if;

    local terms := [op(u)];
    local j;
    for j from 1 to nops(terms) do
       local currentTerm := terms[j];
       local currentTerm_degree := degree(currentTerm);
       local currentTerm_coefficient := coeffs(currentTerm, x);
       u_coeffListed[currentTerm_degree + 1] := currentTerm_coefficient;
    end do;
    
    return u_coeffListed;

  else
    Error("Please enter a univariate polynomial with rational coefficient, where x is the variable");
  end if;
end proc:

poly := x^4 + x^2;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

# --------------------

AddPolynomial :=proc(polynomial1::list, polynomial2::list)

 local result := [];
 local u := polynomial1;                                # degree of u >= degree of v 
 local v := polynomial2;                                # degree of v <= degree of u
 local lengthOfU := nops(u);
 local lengthOfV := nops(v);
 
 if(evalb(lengthOfU = lengthOfV)) then                  # u(X) and v(x) have the same degree
   if(evalb(lengthOfU = 0)) then
     return [];
   end if;
   
   local i;
   for i from 1 to lengthOfU do
     local _coeff := u[i] + v[i];
     result := [op(result), _coeff];
   end do;

   #Remove leading zeros
   local newLengthOfResult := nops(result);
   while(evalb( result[newLengthOfResult] = 0 )) do
     result := [op(subsop(newLengthOfResult=NULL, result))];
     newLengthOfResult := nops(result);
   end do;

   return result;
end if;
 
if(evalb(lengthOfU < lengthOfV)) then                   # degree of u(x) < degree of v(x)
   u := polynomial2;
   v := polynomial1;
   lengthOfU := nops(u);
   lengthOfV := nops(v);                               
end if;

# At this point, the degree of u(x) > degree of v(x)
local j;
for j from 1 to lengthOfV do
  result := [op(result), u[j] + v[j]];
end do;

local k;
for k from (lengthOfV + 1) to lengthOfU do
  result := [op(result), u[k]];
end do;
 
return result;

end proc:

u := x^5 - 15*x^3 + 27*x;
v := x^3 + x^2;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

# --------------------

SubPolynomial := proc(minuendPoly::list, subtrahendPoly::list)
  if(evalb(minuendPoly = subtrahendPoly)) then
    return [];
  end;

  local subtrahendPolyNegated := [];
  local i;
  for i from 1 to nops(subtrahendPoly) do
    subtrahendPolyNegated := [op(subtrahendPolyNegated), (-1) * subtrahendPoly[i]]; 
  end do;
  return AddPolynomial(minuendPoly, subtrahendPolyNegated);
end proc:

u := x^5 - 15*x^3 + 27*x;
v := x^3 + x^2;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

# --------------------

MultPolynomial := proc(polynomial1::list, polynomial2::list)
  local u := polynomial1;                                     # degree of u >= degree of v 
  local v := polynomial2;                                     # degree of v <= degree of u
  local lengthOfU := nops(u);
  local lengthOfV := nops(v);

  if(evalb(lengthOfU < lengthOfV)) then                       # degree of u(x) < degree of v(x)
    u := polynomial2;
    v := polynomial1;
    lengthOfU := nops(u);
    lengthOfV := nops(v);                        
  end if;

  # At this point degree of u(x) >= degree of v(x)
  local result := [];
  local i, j;
  for j from 1 to lengthOfV do

      local p_result := [];
      for i from 1 to lengthOfU do
         p_result := [op(p_result), v[j] * u[i]];
      end do;

      if (evalb(j > 1)) then
        local k;
        for k from 1 to (j - 1) do
          p_result := [0, op(p_result)];
        end do;
      end if;

      result := AddPolynomial(result, p_result);
  end do;

  return result;
end proc:

u := (x^5 - 15*x^3 + 27*x);
v := (x^3 + x^2);
result := expand(u * v);
result_in_coeffList := MultPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

# --------------------

DivPolynomial := proc(polynomial1::list, polynomial2::list)
   if(evalb(nops(polynomial2) = 0)) then
      return "You cannot divide a polynomial by zero";
   end if;

   if(evalb(polynomial1 = polynomial2)) then
      return [[1], []];
   end if;

   if(evalb(nops(polynomial2) = 1) and evalb(polynomial2[1] = 1)) then
     return [[op(polynomial1)], []];
   end if;

   local degreeOfPolynomial1 := nops(polynomial1) - 1;
   local degreeOfPolynomial2 := nops(polynomial2) - 1;
   if(evalb(degreeOfPolynomial1 < degreeOfPolynomial2)) then
     return [[], [op(polynomial1)]];
   else
     local u := [op(polynomial1)];        # dividend
     local v := [op(polynomial2)];        # divisor
     local _quotient := [];               # quotient
     local _remainder := [];              # remainder
     local degreeOfU := nops(u) - 1;      # degree of u
     local degreeOfV := nops(v) - 1;      # degree of v

     while(evalb(degreeOfU >= degreeOfV)) do
       local qiLeadingCoefficient := u[nops(u)] / v[nops(v)];
       local qiDegree := nops(u) - nops(v);
       local qiPoly := qiLeadingCoefficient * x ** (qiDegree);
       local qiPolyCoeffListed := CoeffList(qiPoly); 
       _quotient := [op(AddPolynomial(_quotient, qiPolyCoeffListed))];
       local productOfqiPolyAndDivisor := [op(MultPolynomial(qiPolyCoeffListed, v))]; 
       u := [op(SubPolynomial(u, productOfqiPolyAndDivisor))]; 
       degreeOfU := nops(u) - 1;
     end do;
     
     return [[op(_quotient)], [op(u)]];

   end if;

end proc:


u := x^5 + x^3 + 1;
v := x^2 + x;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := -8/9 + 14/9*x - x^2 + x^3;
v := 8/3 - 6*x + 7/3*x^2 + x^3;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

# --------------------

EEAPoly := proc(polynomial1::list, polynomial2::list)
  local u := [op(polynomial1)];
  local v := [op(polynomial2)];
  local u_lcoeff := u[nops(u)];        
  local v_lcoeff := v[nops(v)];
  
  local u_scaling_factor := 1 / u_lcoeff;                  
  local u_monic := MultPolynomial([u_scaling_factor], u); 
  u := [op(u_monic)];                                      
  print("u(x)_scaling_factor" = u_scaling_factor);
  print("u(x)_monic" = PolynomialTools[FromCoefficientList](u, x));
  
  local v_scaling_factor := 1 / v_lcoeff;                 
  local v_monic := MultPolynomial([v_scaling_factor], v); 
  v := [op(v_monic)];                                     
  print("v(x)_scaling_factor" = v_scaling_factor);
  print("v(x)_monic" = PolynomialTools[FromCoefficientList](v, x));
  

  local q := [];
  local r := DivPolynomial(u, v)[2];
  local r_scaling_factor;
  local r_scaled :[];

  local sm1 := 1 / u_lcoeff;
  local s := [];
  local s_all := [[sm1], []];

  local tm1 := [];
  local t := [[1 / v_lcoeff]];  
  local t_all := [[], [op(op(1, t))]];
  
  local counter := 1;
  while (evalb(nops(r) > 0)) do

    q := DivPolynomial(u, v)[1];
    r := DivPolynomial(u, v)[2];
    print("======================= ITERATION NUMBER ========================" = counter);
    print("u(x)_monic" = PolynomialTools[FromCoefficientList](u, x));  
    print("v(x)_monic" = PolynomialTools[FromCoefficientList](v, x));
    print("q(x)" = PolynomialTools[FromCoefficientList](q, x));  
    print("r(x)" = PolynomialTools[FromCoefficientList](r, x));
    
    u := v;

    if(evalb(nops(r) = 0)) then
      print("============================ WHILE LOOP ENDS ============================");
      break;
    end if;

    r_scaling_factor := 1 / r[nops(r)];
    r_scaled := MultPolynomial([r_scaling_factor], r);
    
    if(evalb(counter = 1)) then
       s := [r_scaling_factor * sm1];
       s_all := [op(s_all), [op(s)]]; 
       t := MultPolynomial([r_scaling_factor * (-1) * op(op(1, t))], q);
       t_all := [op(t_all), [op(t)]];
    else
       s := r_scaling_factor * SubPolynomial(s_all[counter], MultPolynomial(s_all[counter + 1], q)); 
       s_all := [op(s_all), [op(s)]];
       t := r_scaling_factor * SubPolynomial(t_all[counter], MultPolynomial(t_all[counter + 1], q)); 
       t_all := [op(t_all), [op(t)]];
    end if;
    
    print("r(x)_scaling_factor" = r_scaling_factor);
    print("r(x)_scaled" = PolynomialTools[FromCoefficientList](r_scaled, x));
    print("s(x)" = PolynomialTools[FromCoefficientList](s, x));
    print("t(x)" = PolynomialTools[FromCoefficientList](t, x));

    v := MultPolynomial([1 / r[nops(r)]], r);
    counter := counter + 1;
  end do;
  
  if(evalb(nops(r) = 0)) then
    return u;
  else
    return [1];
  end if;

end proc:

u := 54*x^3 - 54*x^2 + 84*x - 48;
v := -12*x^3 - 28*x^2 + 72*x - 32;
my_gcdCoeffListed := EEAPoly(CoeffList(u), CoeffList(v));
my_gcd := PolynomialTools[FromCoefficientList](my_gcdCoeffListed, x);
maple_gcd := gcd(u, v);

# --------------------

restart;