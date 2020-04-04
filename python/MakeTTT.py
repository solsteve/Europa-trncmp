#!/usr/bin/env python3

import sys
import numpy as np
import numpy.random as rnd

rnd.seed()


QO = {}
QX = {}


#/ =======================================================================================
def addO( state, ns ):
    #/ -----------------------------------------------------------------------------------
    try:
        next = QO[state]
        next.append( ns )
    except KeyError:
        next = [ns,]
    QO[state] = next

    
#/ =======================================================================================
def addX( state, ns ):
    #/ -----------------------------------------------------------------------------------
    try:
        next = QX[state]
        next.append( ns )
    except KeyError:
        next = [ns,]
    QX[state] = next

    
#/ =======================================================================================
def isTerminal( S ):
    #/ -----------------------------------------------------------------------------------
    if ( ( 'x'==S[0] ) and ( 'x'==S[1] ) and ( 'x'==S[2] ) ): return -1
    if ( ( 'x'==S[3] ) and ( 'x'==S[4] ) and ( 'x'==S[5] ) ): return -1
    if ( ( 'x'==S[6] ) and ( 'x'==S[7] ) and ( 'x'==S[8] ) ): return -1
    if ( ( 'x'==S[0] ) and ( 'x'==S[3] ) and ( 'x'==S[6] ) ): return -1
    if ( ( 'x'==S[1] ) and ( 'x'==S[4] ) and ( 'x'==S[7] ) ): return -1
    if ( ( 'x'==S[2] ) and ( 'x'==S[5] ) and ( 'x'==S[8] ) ): return -1
    if ( ( 'x'==S[0] ) and ( 'x'==S[4] ) and ( 'x'==S[8] ) ): return -1
    if ( ( 'x'==S[2] ) and ( 'x'==S[4] ) and ( 'x'==S[5] ) ): return -1

    if ( ( 'o'==S[0] ) and ( 'o'==S[1] ) and ( 'o'==S[2] ) ): return  1
    if ( ( 'o'==S[3] ) and ( 'o'==S[4] ) and ( 'o'==S[5] ) ): return  1
    if ( ( 'o'==S[6] ) and ( 'o'==S[7] ) and ( 'o'==S[8] ) ): return  1
    if ( ( 'o'==S[0] ) and ( 'o'==S[3] ) and ( 'o'==S[6] ) ): return  1
    if ( ( 'o'==S[1] ) and ( 'o'==S[4] ) and ( 'o'==S[7] ) ): return  1
    if ( ( 'o'==S[2] ) and ( 'o'==S[5] ) and ( 'o'==S[8] ) ): return  1
    if ( ( 'o'==S[0] ) and ( 'o'==S[4] ) and ( 'o'==S[8] ) ): return  1
    if ( ( 'o'==S[2] ) and ( 'o'==S[4] ) and ( 'o'==S[5] ) ): return  1

    return 0


#/ =======================================================================================
def randomState():
    #/ -----------------------------------------------------------------------------------
    test = ( '.', 'x', 'o', '.', 'x', 'o', '.', 'x', 'o', )
    n = len(test)

    s = test[ rnd.randint(n) ]
    for i in range(1,9):
        s = '%s%s' % ( s, test[ rnd.randint(n) ], )

    return s


#/ =======================================================================================
def nextState( state, idx, mark ):
    #/ -----------------------------------------------------------------------------------
    ns = state[0]
    if ( 0 == idx ): ns = mark
    for i in range(1,9):
        if ( i==idx ):
            ns = '%s%s' % ( ns, mark, )
        else:
            ns = '%s%s' % ( ns, state[i], )
    return ns


#/ =======================================================================================
def playO( state ):
    #/ -----------------------------------------------------------------------------------
    if ( 0 == isTerminal( state ) ):
        for i in range(9):
            if ( '.' == state[i] ):
                ns = nextState( state, i, 'o' )
                addO( state, i )
                playX( ns )

                
#/ =======================================================================================
def makeRecord( state, ns ):
    #/ -----------------------------------------------------------------------------------
    play = ( 'X', '/', 'O' )
    mask = 9*['-']

    for i in ns:
        mask[i] = '%d' % (i,)

    return '%s%s' % ( state, ''.join(mask), )

                
#/ =======================================================================================
def playX( state ):
    #/ -----------------------------------------------------------------------------------
    if ( 0 == isTerminal( state ) ):
        for i in range(9):
            if ( '.' == state[i] ):
                ns = nextState( state, i, 'x' )
                addX( state, i )
                playO( ns )

playO('.........')

print( 'Length of the O-table = %d' % ( len(QO), ) )
print( 'Length of the X-table = %d' % ( len(QX), ) )

fp = open( 'qtable.ttt', 'w' )

for key,rec in QO.items():
    fp.write( '%s\n' % ( makeRecord( key, rec ), ) )

fp.close()
    
