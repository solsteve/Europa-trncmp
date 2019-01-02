def cubic_interpolate( xt, T0, T1, T2, T3 ):
    X = (T0[0],T1[0],T2[0],T3[0])
    Y = (T0[1],T1[1],T2[1],T3[1])
    S = 0.0e0
    for i in (0,1,2,3):
        P = 1.0e0
        for j in (0,1,2,3):
            if (i != j):
                P = P * (xt - X[j]) / (X[i] - X[j])
                S = S + P*Y[i]
    return S
    
def linear_interpolate( x, p0, p1 ):
    x0 = p0[0]
    y0 = p0[1]
    x1 = p1[0]
    y1 = p1[1]
    dif = x1 - x0
    m = (y1 - y0) / dif
    b = (x1*y0 - x0*y1)/dif
    return x * m + b
