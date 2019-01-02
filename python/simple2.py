#!/usr/bin/python3

import numpy as np
import csv
import sys, time

def act(x):
    return 1.0/(1.0+np.exp(-x))

def dAct(x):
    return x*(1-x)

def ReadLabled( fspc ):
    fp = open(fspc, 'r')
    X = []
    Y = []

    head = fp.readline().strip().split()
    n  = int(head[0])
    nx = int(head[1])
    ny = int(head[2])
    
    for j in range(n):
        line = fp.readline().strip().split()
        x_rec = []
        y_rec = []
        for i in range(nx):
            x_rec.append(float(line[i]))
        for i in range(ny):
            y_rec.append(float(line[nx+i]))
        X.append(x_rec)
        Y.append(y_rec)
    return ( np.array(X), np.array(Y) )

def DisplayError( g, E ):
    (r, c ) = E.shape
    mse = np.multiply(E,E).sum() / float( r*c )
    print( '%10d %13.6e' % (g,mse, ))


def WriteMat( fp, M ):
    ( nr, nc ) = M.shape
    for r in range(nr):
        for c in range(nc):
            fp.write( '%12.8f\n' %( M[r,c], ) )

def ReadMat( fp, nr, nc ):
    M = np.zeros(( nr, nc ))
    for r in range(nr):
        for c in range(nc):
            M[r,c] = float(fp.readline())
    return M

def WriteNet( fspc, W1, W2, W3, b1, b2, b3 ):
    fp = open( fspc, 'w' )
    WriteMat( fp, W1 )
    WriteMat( fp, W2 )
    WriteMat( fp, W3 )
    WriteMat( fp, b1 )
    WriteMat( fp, b2 )
    WriteMat( fp, b3 )
    fp.close()

def ReadNet( fspc, m0, m1, m2, m3 ):
    fp = open( fspc, 'r' )

    W1 = ReadMat( fp, m0, m1 )
    W2 = ReadMat( fp, m1, m2 )
    W3 = ReadMat( fp, m2, m3 )
    b1 = ReadMat( fp, 1,  m1 )
    b2 = ReadMat( fp, 1,  m2 )
    b3 = ReadMat( fp, 1,  m3 )

    fp.close()

    return ( W1, W2, W3, b1, b2, b3 )

def MakeNet( fspc, m0, m1, m2, m3 ):
    np.random.seed(1)
    
    W1 = 0.02*np.random.random((m0,m1)) - 0.01
    W2 = 0.02*np.random.random((m1,m2)) - 0.01
    W3 = 0.02*np.random.random((m2,m3)) - 0.01
    
    b1 = 0.02*np.random.random((1,m1)) - 0.01
    b2 = 0.02*np.random.random((1,m2)) - 0.01
    b3 = 0.02*np.random.random((1,m3)) - 0.01
    
    WriteNet( fspc, W1, W2, W3, b1, b2, b3 )

def displayMat( M ):
    nr = len(M)
    nc = len(M[0])

    fmt = '%15.8e'
    
    for r in range(nr):
        line = ( fmt % (M[r,0],) )
        for c in range(1,nc):
            line = line + ' ' + ( fmt % (M[r,c],) )
        if (0 == r):
            sys.stdout.write( '[[%s]\n' % (line, ) )
        else:
            if ((nr-1) == r):
                sys.stdout.write( ' [%s]]\n' % (line, ) )
            else:
                sys.stdout.write( ' [%s]\n' % (line, ) )
            
def TestNet():
    ( X, Y ) = ReadLabled( '/data/datasets/Iris/iris.onehot' )

    ( nx, m0 ) = X.shape
    m1 = 7
    m2 = 5
    ( ny, m3 ) = Y.shape

    MakeNet( '/tmp/net-init.cfg', m0, m1, m2, m3 )
    
    ( W1, W2, W3, b1, b2, b3 ) = ReadNet( '/tmp/net-init.cfg', m0, m1, m2, m3 )  
    
    WriteNet( '/tmp/net-check-python.cfg', W1, W2, W3, b1, b2, b3 )

    ones  = np.ones((nx,1))
    onesT = np.ones((1,nx))

    l_rate = 0.3 / nx

    print( "using alpha=", l_rate )

    T0 = time.time()
    maxgen = 600000
    count = 0
    for gen in range(maxgen):

        # ----- forward propagate ---------------
        z1 = (X @ W1) + (ones @ b1)
        a1 = act(z1)

        
        z2 = (a1 @ W2) + (ones @ b2)
        a2 = act(z2)
        z3 = (a2 @ W3) + (ones @ b3)
        a3 = act(z3)

        E3 = (Y - a3)
        d3 = E3 * dAct(a3)

        #if (gen % 10000) == 0:
        #    DisplayError(gen+1,E3)

        E2 = d3 @ W3.T
        d2 = E2 * dAct(a2)

        E1 = d2 @ W2.T
        d1 = E1 * dAct(a1)

        W3 = W3 + (a2.T @ d3)*l_rate
        W2 = W2 + (a1.T @ d2)*l_rate
        W1 = W1 + (X.T  @ d1)*l_rate

        b3 = b3 + ( onesT @ d3 )*l_rate
        b2 = b2 + ( onesT @ d2 )*l_rate
        b1 = b1 + ( onesT @ d1 )*l_rate

        count += 1

        if ( 0 == gen ):
            WriteNet( '/tmp/net-once-python.cfg', W1, W2, W3, b1, b2, b3 )

    T1 = time.time()

    DisplayError(count,E3)
    print( '1 thread(s), %8.3f seconds' % ( T1-T0, ) )
    
    WriteNet( '/tmp/net-last-python.cfg', W1, W2, W3, b1, b2, b3 )
TestNet()
