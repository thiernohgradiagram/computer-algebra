GcdInteger :=proc(numb1, numb2)
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

  return g * v;
end proc:

# --------------------

MultRational := proc(rational1::list, rational2::list)
  local a, b, c, d;
  a := op(1, rational1);
  b := op(2, rational1);
  c := op(1, rational2);
  d := op(2, rational2);
  if (evalb(b = 0) or evalb(d = 0)) then
    return Error("You cannot divide by Zero");
  end if;
  local reducer := (GcdInteger(a, d)) * (GcdInteger(b, c));
  return ((a * c) / reducer) / ((b * d) / reducer);
end proc:

# --------------------

4/9 * 15/14;
MultRational([4, 0], [15, 14]);
MultRational([4, 9], [15, 0]);
MultRational([4, 9], [15, 14]);
MultRational([-4, 9], [15, 14]);
MultRational([-2, 1], [3, 1]);
MultRational([-2, 1], [3, 1]);
MultRational([2, 2], [4, 2]);

# --------------------

DivRational := proc(rational1::list, rational2::list)
  local a, b, c, d;
  a := op(1, rational1);
  b := op(2, rational1);
  c := op(1, rational2);
  d := op(2, rational2);
  if (evalb(b = 0) or evalb(d = 0)) then
    return Error("You cannot divide by Zero");
  end if;
  return MultRational([a, b], [d, c]);
end proc:

# --------------------

(4/9) / (3/1);
DivRational([4, 9], [3, 1]);
DivRational([4, 0], [3, 1]);
DivRational([4, 9], [3, 0]);

# --------------------

AddRational := proc(rational1::list, rational2::list)
  local a, b, c, d, p, q, g, reducer;
  a := op(1, rational1);
  b := op(2, rational1);
  c := op(1, rational2);
  d := op(2, rational2);
  if (evalb(b = 0) or evalb(d = 0)) then
    return Error("You cannot divide by Zero");
  end if;

  g := GcdInteger(b, d);
  if (evalb(g = 1)) then
    p := a * d + c * b;
    q := d * b;
    return p / q;                          # not reducable 
  else
    p := a * (d / g) + c * (b / g);
    q := d * (b / g);
    reducer := GcdInteger(p, g);
    return (p / reducer) / (q / reducer);  # reducable
  end if;
end proc:

# --------------------

11/42 + 13/60;
AddRational([11, 42], [13, 60]);
AddRational([11, 0], [13, 60]);
AddRational([11, 42], [13, 0]);

# --------------------

SubRational := proc(rational1::list, rational2::list)
  local a, b, c, d, p, q, g, reducer;
  a := op(1, rational1);
  b := op(2, rational1);
  c := op(1, rational2);
  d := op(2, rational2);
  if (evalb(b = 0) or evalb(d = 0)) then
    return Error("You cannot divide by Zero");
  end if;

  g := GcdInteger(b, d);
  if (evalb(g = 1)) then
    p := a * d - c * b;
    q := d * b;
    return p / q;                          # not reducable 
  else
    p := a * (d / g) - c * (b / g);
    q := d * (b / g);
    reducer := GcdInteger(p, g);
    return (p / reducer) / (q / reducer);  # reducable
  end if;
end proc:

# --------------------

SubRational([3, 2], [1, 2]);
SubRational([1, 2], [3, 2]);
SubRational([3, 0], [1, 2]);
SubRational([1, 2], [3, 0]);