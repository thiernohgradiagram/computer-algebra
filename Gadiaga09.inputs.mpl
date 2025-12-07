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

# --------------------

PseudoDiv := proc(polynomial1::list, polynomial2::list)
  if(evalb(nops(polynomial2) = 0)) then
      return Error("You cannot Pseudo-divide a polynomial by zero");
  end if;

  local u := [op(polynomial1)];                # dividend
  local v := [op(polynomial2)];                # divisor
  local lengthOfU := nops(u);                  # length of the dividend u
  local lengthOfV := nops(v);                  # length of the divisor v
  local v_n := v[lengthOfV];                   # the leading coefficient of the divisor v(x)
  local delta := lengthOfU - lengthOfV;        # degree(u(x)) minus degree(v(x))
  local scaling_factor := (v_n)^(delta + 1);   # the scaling factor

  # scaling the dividend u(x) by the scaling_factor
  local i;
  for i from 1 to lengthOfU do
    u[i] := u[i] * scaling_factor;
  end do;

  local poly_division_result := DivPolynomial(u, v);
  local psquo := poly_division_result[1];
  local psrem := poly_division_result[2];
  
  # computing the actual quotient
  local _quotient := [];
  local j;
  for j from 1 to nops(psquo) do
    _quotient := [op(_quotient), psquo[j] / scaling_factor];
  end do;

  # computing the actual remainder
  local _remainder := [];
  local k;
  for k from 1 to nops(psrem) do
    _remainder := [op(_remainder), psrem[k] / scaling_factor];
  end do;

  return [psquo, psrem, [scaling_factor]]; 
  
end proc:


u := 2*x^3 + x^2 + x + 3;
v := 3*x^2 -2*x + 1;
_quotient := CoeffList(quo(u, v, x));
_remainder := CoeffList(rem(u, v, x));
my_pseudo_div_result := PseudoDiv(CoeffList(u), CoeffList(v));

u := 2*x^3 + x^2 + x + 3;
v := 0;
#_quotient := CoeffList(quo(u, v, x));
#_remainder := CoeffList(rem(u, v, x));
my_pseudo_div_result := PseudoDiv(CoeffList(u), CoeffList(v));

u := 2*x^3 + x^2 + x + 3;
v := 2*x^3 + x^2 + x + 3;
_quotient := CoeffList(quo(u, v, x));
_remainder := CoeffList(rem(u, v, x));
my_pseudo_div_result := PseudoDiv(CoeffList(u), CoeffList(v));

u := 2*x^3 + x^2 + x + 3;
v := 1;
_quotient := CoeffList(quo(u, v, x));
_remainder := CoeffList(rem(u, v, x));
my_pseudo_div_result := PseudoDiv(CoeffList(u), CoeffList(v));

u := x^5 + x^3 + 1;
v := x^2 + x;
_quotient := CoeffList(quo(u, v, x));
_remainder := CoeffList(rem(u, v, x));
my_pseudo_div_result := PseudoDiv(CoeffList(u), CoeffList(v));

# --------------------

ContPoly := proc(polynomial::list) 
  local hasRationalCoeff := false;
  local element;
  for element in polynomial do
    if (not evalb(type(element, integer))) then
       hasRationalCoeff := true;
       break;
    end if;
  end do;

  if(evalb(hasRationalCoeff)) then
    local coeffListNumerator := [];
    local coeffListDenominator := [];
    local i;
    for i from 1 to nops(polynomial) do
      local currentCoeff := polynomial[i];
      if(not evalb(type(currentCoeff, integer))) then
         coeffListNumerator := [op(coeffListNumerator), op(1, currentCoeff)];
         coeffListDenominator := [op(coeffListDenominator), op(2, currentCoeff)];
      else
         coeffListNumerator := [op(coeffListNumerator), currentCoeff];
         coeffListDenominator := [op(coeffListDenominator), 1];
      end if;
    end do;

    return igcd(op(coeffListNumerator)) / ilcm(op(coeffListDenominator));
  end if;

  return igcd(op(polynomial));
end proc:

u := 54*x^3 - 54*x^2 + 84*x - 48;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := -12*x^3 - 28*x^2 + 72*x - 32;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := 0;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := 0*x^4;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := 11;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := -5/12*x^3 - 28*x^2 + 72*x - 32;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := 1/33;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := x^8 + x^6 -3*x^4 -3*x^3 + 8*x^2 + 2*x - 5;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

u := 3*x^6 + 5*x^4 -4*x^2 - 9*x + 21;
contentOfU := content(u);
my_contentOfU := ContPoly(CoeffList(u));

# --------------------

PrimPartPoly := proc(poly::list)
 if(evalb(nops(poly) = 0)) then
   return [];
 end if;

 local polynomial := [op(poly)];
 local lengthOfPolynomial := nops(polynomial);
 local contentOfPoly := ContPoly(polynomial);
 local leadingCoefficient := polynomial[lengthOfPolynomial];
 
 local i;
 for i from 1 to lengthOfPolynomial do
   polynomial[i] := sign(leadingCoefficient) * (polynomial[i]) / contentOfPoly;
 end do;

 return polynomial;

end proc:

u := 54*x^3 - 54*x^2 + 84*x - 48;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := -12*x^3 - 28*x^2 + 72*x - 32;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := 0;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := 0*x^45;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := x^12;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := (9/4)*x^12 + (8/7)*x;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := x^8 + x^6 -3*x^4 -3*x^3 + 8*x^2 + 2*x - 5;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := 3*x^6 + 5*x^4 -4*x^2 - 9*x + 21;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

u := 143193869;
ppu := primpart(u);
my_ppu := PolynomialTools[FromCoefficientList](PrimPartPoly(CoeffList(u)), x);

# --------------------

GcdPrimitive := proc(polynomial1::list, polynomial2::list)
  local contentOfU := ContPoly(polynomial1);
  local contentOfV := ContPoly(polynomial2);
  local c := igcd(contentOfU, contentOfV);
  local u := PrimPartPoly(polynomial1);         # primitive parts of u
  local v := PrimPartPoly(polynomial2);         # primitive parts of v
  local degreeOfV := nops(v) - 1;
  local r := [];

  while(evalb(degreeOfV > 0)) do
    r := op(2, PseudoDiv(u, v));                # the pseudo-remainder
    u := [op(v)];
    v := [op(PrimPartPoly(r))];
    degreeOfV := nops(v) - 1;
  end do;

  if(evalb(nops(v) = 0)) then
    return MultPolynomial([c], u);
  else
    return [c];
  end if;
 
end proc:

u := x^8 + x^6 -3*x^4 -3*x^3 + 8*x^2 + 2*x - 5;
v := 3*x^6 + 5*x^4 -4*x^2 - 9*x + 21;
_gcd := gcd(u, v);
my_gcdCoeffListed := GcdPrimitive(CoeffList(u), CoeffList(v));
my_gcd := PolynomialTools[FromCoefficientList](my_gcdCoeffListed, x);

u2 := 54*x^3 - 54*x^2 + 84*x - 48;
v2 := -12*x^3 - 28*x^2 + 72*x - 32;
_gcd := gcd(u2, v2);
my_gcdCoeffListed := GcdPrimitive(CoeffList(u2), CoeffList(v2));
my_gcd := PolynomialTools[FromCoefficientList](my_gcdCoeffListed, x);

u := x^2 - 1;
v := 2*x^2 + 4*x + 2;
_gcd := gcd(u, v);
my_gcdCoeffListed := GcdPrimitive(CoeffList(u), CoeffList(v));
my_gcd := PolynomialTools[FromCoefficientList](my_gcdCoeffListed, x);