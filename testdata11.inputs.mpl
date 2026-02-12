SKFactorization_11;  name;
kernelopts(bytesalloc,cputime);

# --------------------

Score:  xx / 30Comments:

# --------------------

p1 := x^4 + 4; factor(p1); SKFactor(p1,x);
kernelopts(bytesalloc,cputime);

# --------------------

p2 := x^5 + x + 1; factor(p2); SKFactor(p2,x);
kernelopts(bytesalloc,cputime);

# --------------------

p3 := x^5 - x + 1; factor(p3); SKFactor(p3,x);
kernelopts(bytesalloc,cputime);

# --------------------

p4 := y^6 - 1; factor(p4); SKFactor(p4,y);
kernelopts(bytesalloc,cputime);

# --------------------

p5 := 25*x^6 - 16; factor(p5); SKFactor(p5,x);
kernelopts(bytesalloc,cputime);

# --------------------

p6 := x^6 + 2*x^4 + 4*x^3 + x^2 + 4*x + 4; factor(p6); SKFactor(p6,x);
kernelopts(bytesalloc,cputime);

# --------------------

p7 := x^8 - 2*x^6 + 2*x^2 - 1; factor(p7); SKFactor(p7,x);
kernelopts(bytesalloc,cputime);

# --------------------

p8 := 2*x^4 + 4*x^3 + 2*x + 8; factor(p8); SKFactor(p8,x);
kernelopts(bytesalloc,cputime);

# --------------------

p9:= 35*x^4+x^3+41*x^2+45*x-66; factor(p9); SKFactor(p9,x);
kernelopts(bytesalloc,cputime);

# --------------------

p10 := x^12-3*x^10-3*x^8+11*x^6+6*x^4-12*x^2-8; factor(p10); SKFactor(p10,x);
kernelopts(bytesalloc,cputime);