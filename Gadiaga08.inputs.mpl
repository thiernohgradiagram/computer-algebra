SparseCoeffList := proc(u::anything)

   if type(u, polynom(rational, x)) then
     local sparseCoeffList := [];
     local coefficients := [coeffs(u, x)];

     if(evalb(u = 0)) then
        return [];
     elif(evalb(nops(coefficients) = 1)) then
       return [[degree(u),op(coefficients)]];
     end if;

     # At this point, the univariate polynomial has at 2 non-zero terms
     local terms := [op(u)];
     local i;
     for i from 1 to nops(terms) do
        local currentTerm := terms[i];
        local sparseTerm := [degree(currentTerm), coeffs(currentTerm, x)];
        sparseCoeffList := [op(sparseCoeffList), sparseTerm];
     end do;

     return sparseCoeffList;
  else
    Error("Please enter a univariate polynomial with rational coefficients, where x is the variable");
  end if;

end proc:

polynomial := 8*x^6 + 13*x^5;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := (2/3)*x^3 + 3*x;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := (2/3)*x^3 + 3*x + 3*x;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := x^13;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := (7/9)*x^13;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := x^1;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := x^0;
sparseCoeffList := SparseCoeffList(polynomial);

polynomial := 0*x^0 + 0*x^56;
sparseCoeffList := SparseCoeffList(polynomial);

# --------------------

MultSparsePoly := proc(polynomial1::list, polynomial2::list)
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

      local vj := [op(v[j])];                            # [degree, coefficient]
      for i from 1 to lengthOfU do
         local ui := [op(u[i])];                         # [degree, coefficient]
         local newDegree := vj[1] + ui[1];               # new degree
         local newCoefficient := vj[2] * ui[2];          # new coefficient
         local newTerm := [newDegree, newCoefficient];
         local newTermHasMatch := false;
          
         if(evalb(j = 1)) then
           result := [op(result), newTerm];
         elif (evalb(j > 1)) then
           local k;
           for k from 1 to nops(result) do

             local kTerm := [op(result[k])]; 
             local kDegree := kTerm[1];
             local kCoefficient := kTerm[2];
             
             if(evalb(newDegree = kDegree)) then
                newTermHasMatch := true;
                local updatedTerm := [kDegree, kCoefficient + newCoefficient];
                result[k]:= [op(updatedTerm)];
                break;
             end if;
           end do;

           if(not newTermHasMatch) then
             result := [op(result), newTerm];
           end if;
           
         end if;
      end do;
  end do;

  return ListTools[Reverse](sort(result));
end proc:

u := (x^5 - 15*x^3 + 27*x);
v := (x^3 + x);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := (x^5 - 15*x^3 + 27*x);
v := (x^3 + x^2);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := (x^3 + x^2);
v := (x^5 - 15*x^3 + 27*x);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := (x^3 + x);
v := (x^5 - 15*x^3 + 27*x);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := (x^8 + x^3);
v := (x^5 + x^7);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := (x^8 + x^3);
v := (x^5);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := (x^8);
v := (x^5);
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));

u := 0*x^8;
v := x^5;
result := expand(u * v);
sparsed_result := SparseCoeffList(result);
multSparsePoly := MultSparsePoly(SparseCoeffList(u), SparseCoeffList(v));