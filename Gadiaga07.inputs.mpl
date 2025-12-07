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

poly := x^40 + x^2;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := x^5 - 15*x^3 + 27*x;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := (1/6)*x^5 - (3/2)*x^3 + 4*x;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := (1/6)*x^5 - (3/2)*x^3 + 4*x;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := 0;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := 0*x^99 + 4*x^7;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := 123;
coeffList_from_poly := CoeffList(poly);
poly_from_coeffList := PolynomialTools[FromCoefficientList](coeffList_from_poly, x);

poly := 0*x;
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

u := x^3 + x^2;
v := x^5 - 15*x^3 + 27*x;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^3 + x^2;
v := x^2 - 15*x^2;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^3 + x^2;
v := x^2 - x^3;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^2;
v := x^4;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^25;
v := x^25;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := (1/4)*x;
v := x^2 + (3/4)*x^5;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := 0*x;
v := 0*x;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

v := x^2 - 3*x^4;
result := 0 + v;
result_in_coeffList := AddPolynomial([], CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^3;
v := -x^2;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := 0*x^5;
v := -x^4 + x^3 + 1;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^5 + x^3 + 1;
v := -x^5 - x^4;
result := u + v;
result_in_coeffList := AddPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := 0*x;
v := 7*x;
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

u := x^3 + x^2;
v := x^5 - 15*x^3 + 27*x;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^3 + x^2;
v := x^2 - 15*x^2;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^3 + x^2;
v := x^2 - x^3;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^2;
v := x^4;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^25;
v := x^25;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := (1/4)*x;
v := x^2 + (3/4)*x^5;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := 0*x;
v := 0*x;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := x^5 + x^3 + 1;
v := x^5 + x^4;
result := u - v;
result_in_coeffList := SubPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

#result_in_coeffList := SubPolynomial([1, 0, 0, 1, 0, 1], [0, 0, 0, 0, 1, 1]);

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

u := (x^3 + x^2);
v := (x^5 - 15*x^3 + 27*x);
result := expand(u * v);
result_in_coeffList := MultPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := (x^8);
v := (x^5);
result := expand(u * v);
result_in_coeffList := MultPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := (x^4);
v := (x^4);
result := expand(u * v);
result_in_coeffList := MultPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := 15;
v := 3;
result := expand(u * v);
result_in_coeffList := MultPolynomial(CoeffList(u), CoeffList(v));
result_in_polynomial := PolynomialTools[FromCoefficientList](result_in_coeffList, x);

u := 0;
v := 0;
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

u := x^5 + x^3 + 1;
v := x^2 + x + 2;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^2 + x;
v := x^5 + x^3 + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^2 + x + 2;
v := x^5 + x^3 + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 2;
v := x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 2;
v := -x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 2;
v := -x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v)); 

u := x^5 + x + 1;
v := -x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := 0;
v := x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := 0*x^23;
v := x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := 66;
v := x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 1;
v := 0;
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 1;
v := 0*x^4;
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 1;
v := 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := x^5 + x + 1;
v := x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := (1/3)*x^8 + x + 1;
v := x^5 + x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := (1/3)*x^8 + (4/3)*x + 1/7;
v := x^5 + (3/4)*x + 1;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := (7/3)*x^8 + (4/3)*x + 1/7;
v := x^8 + (3/4)*x;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

u := (7/3)*x^8 + (4/3)*x + 1/7;
v := (7/3)*x^8 + (4/3)*x + 1/7;
_quotient := quo(u, v, x);
_remainder := rem(u, v, x);
result_coeffListed := [ [op(CoeffList(_quotient))], [op(CoeffList(_remainder))] ];
my_result_coeffListed := DivPolynomial(CoeffList(u), CoeffList(v));

# --------------------

MakeItMonic := proc(euclidGcd::list)
  local bad_gcd := [op(euclidGcd)];
  local hasFraction := false;
  
  local element;
  for element in bad_gcd do
    if (not evalb(type(elementent, integer))) then
      hasFraction := true;
    end if;
  end do;

  if(evalb(hasFraction)) then
    local _lcoeff := bad_gcd[nops(bad_gcd)];
    bad_gcd := bad_gcd / _lcoeff;
  end if;

  return bad_gcd;

end proc:


GcdPolynomial := proc(polynomial1::list, polynomial2::list)
  options remember;
  local u;
  local v;
  
  if(evalb(nops(polynomial2) > nops(polynomial1))) then
    u := polynomial2;
    v := polynomial1;
  else
    u := polynomial1;
    v := polynomial2;
  end if;

  if(evalb(nops(v) = 0)) then 
    return MakeItMonic(u);
  end if;

  if(evalb(nops(v) = 1)) then
    return [1];
  end if;

  return GcdPolynomial(v, [op(DivPolynomial(u, v)[2])]);

end proc:


u := x^2 - 1;
v := 2*x^2 + 4*x + 2;
gcd(u, v);
my_gcd := PolynomialTools[FromCoefficientList](GcdPolynomial(CoeffList(u), CoeffList(v)), x);

u := x^4 - 1;
v := (5/4)*x^2 + x + 2;
maple_gcd := gcd(u, v);
my_gcd := PolynomialTools[FromCoefficientList](GcdPolynomial(CoeffList(u), CoeffList(v)), x);

u := 54*x^3 - 54*x^2 + 84*x - 48;
v := -12*x^3 - 28*x^2 + 72*x - 32;
maple_gcd := gcd(u, v);
my_gcd := PolynomialTools[FromCoefficientList](GcdPolynomial(CoeffList(u), CoeffList(v)), x);