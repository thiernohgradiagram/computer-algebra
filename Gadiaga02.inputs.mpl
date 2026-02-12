(*A procedure called Maxima that takes an expression and returns a list of the [x,y] coordinates of the local maxima of the expression*)

Maxima := proc(expression::anything)

  local firstDerivative := diff(expression, x);
  local firstDerivativeEquatedToZero := firstDerivative = 0;
  local criticalPoints:= [solve(firstDerivativeEquatedToZero, x)];
  
  local element;
  local realCriticalPoints := [];
  for element in criticalPoints do
    local isComplexNumber := has(evalc(element), I);
    if not isComplexNumber then
       realCriticalPoints := [op(realCriticalPoints), element];
    end if
  end do;

  local secondDerivative := diff(firstDerivative, x);
  local maximaPoints := [];
  local el;

  for el in realCriticalPoints do
    local valueOf2ndDiffAtEl := subs(x = el, secondDerivative);
    local valueOf2ndDiffAtEl_float := evalf(valueOf2ndDiffAtEl);
    local valIsNegative := evalb(valueOf2ndDiffAtEl_float < 0);

    if valIsNegative then
      maximaPoints := [op(maximaPoints), [el, subs(x = el, expression)]];
    end if
  end do;
  
  return maximaPoints;
end proc:

(*A procedure called Minima that takes an expression and returns a list of the [x,y] coordinates of the local minima of the expression*)

Minima := proc(expression::anything)
  
  local firstDerivative := diff(expression, x);
  local firstDerivativeEquatedToZero := firstDerivative = 0;
  local criticalPoints := [solve(firstDerivativeEquatedToZero, x)];

  local element;
  local realCriticalPoints := [];
  for element in criticalPoints do
    local isComplexNumber := has(evalc(element),I);
    if not isComplexNumber then
      realCriticalPoints := [op(realCriticalPoints), element];
    end if
  end do;

  local secondDerivative := diff(firstDerivative, x);
  local minimaPoints := [];
  local el;

  for el in realCriticalPoints do
    local valueOf2ndDiffAtEl := subs(x = el, secondDerivative);
    local valueOf2ndDiffAtEl_float := evalf(valueOf2ndDiffAtEl);
    local valIsPositive := evalb(valueOf2ndDiffAtEl_float > 0);

    if valIsPositive then
      minimaPoints := [op(minimaPoints), [el, subs(x = el, expression)]];
    end if
  end do;
  
  return minimaPoints;
end proc:

(*A procedure called Inflect that takes an expression and returns a list of the [x,y] coordinates of the local minima of the expression*)

Inflect := proc(expression::anything)
  
  local firstDerivative := diff(expression, x);
  local secondDerivative := diff(firstDerivative, x);
  local secondDerivativeEquatedToZero := secondDerivative = 0;
  local inflectionPoints := [solve(secondDerivativeEquatedToZero, x)];

  local el;
  local realInflectionPoints := [];
  for el in inflectionPoints do
    local isComplexNumber := has(evalc(el),I);
    if not isComplexNumber then
      realInflectionPoints:=[op(realInflectionPoints), [el, subs(x=el, expression)]];
    end if
  end do;

  return realInflectionPoints;
  
end proc:

# --------------------

f:= x^4 - 2*x^2 + 1;
_maxima:= Maxima(f);
_minima:= Minima(f);
_inflection:= Inflect(f);
with(plots,display);
display(plot(f), plot(_maxima, style=POINT), plot(_minima, style=POINT), plot(_inflection, style=POINT));

# --------------------

f:= x^3 + x;
_maxima:= Maxima(f);
_minima:= Minima(f);
_inflection:= Inflect(f);
with(plots,display);
display(plot(f), plot(_maxima, style=POINT), plot(_minima, style=POINT), plot(_inflection, style=POINT));

# --------------------

f:= (1 / ((x^2)*(x - 1))) + 3;
_maxima:= Maxima(f);
_minima:= Minima(f);
_inflection:= Inflect(f);
with(plots,display);
display(plot(f), plot(_maxima, style=POINT), plot(_minima, style=POINT), plot(_inflection, style=POINT));

# --------------------

f:= exp(1)^(x^2) - exp(1)*x^2 + 1;
_maxima:= Maxima(f);
_minima:= Minima(f);
_inflection:= Inflect(f);
with(plots,display);
display(plot(f, x=-5..5, y=-5..5), plot(_maxima, style=POINT), plot(_minima, style=POINT), plot(_inflection, style=POINT));

# --------------------

f:= 1/3*x^6 - 3/5*x^5 + 19/2*x^4 - 19*x^3 - 150*x^2 + 450*x;
_maxima:= Maxima(f);
_minima:= Minima(f);
_inflection:= Inflect(f);
with(plots,display);
display(plot(f, x=-6..6, y=-20..20), plot(_maxima, style=POINT), plot(_minima, style=POINT), plot(_inflection, style=POINT));