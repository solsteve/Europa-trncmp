#!/usr/bin/env python3

import sys

nr=3
nc=3

for i in range(1,nc+1):
    for j in range(1,nc+1):
        i2=j
        j2=i
        X = 'B(%d,%d) = A(%d,1)*A(1,%d)' % (i,j,i2,j2,)
        for k in range(2,nr+1):
            X = '%s + A(%d,%d)*A(%d,%d)' % (X,i,k,k,j2,)
        print(X)
            
