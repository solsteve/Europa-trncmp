#!/usr/bin/python3
#/ ====================================================================== BEGIN FILE =====
#/ **                             S L I D I N G W I N D O W                             **
#/ =======================================================================================

import numpy as np
import time, sys


#/ =======================================================================================
def convert( X ):
    #/ -----------------------------------------------------------------------------------
    md = np.median(X)
    n  = len(X)
    for i in range(n):
        if ( X[i] < md ):
            X[i] = 0.0
        else:
            X[i] = 1.0


#/ =======================================================================================
def ProcessFile( inp_filename, out_filename, use_col, num_inp, num_out, num_inc ):
    #/ -----------------------------------------------------------------------------------
    print( 'Reading from      ', inp_filename )
    print( '  at col = ', use_col )
    print( 'Writting to       ', out_filename )
    print( 'Number of inputs  ', num_inp )
    print( 'Number of outputs ', num_out )
    print( 'Slide             ', num_inc )

    #/ ----- read input file -------------------------------------------------------------

    fp = open( inp_filename, 'r' )

    dummy = fp.readline()

    X = []
    for raw in fp:
        parts = raw.split(',')
        X.append( float( parts[use_col].strip() ) )
    fp.close()

    convert( X )

    #/ ----- write output file -----------------------------------------------------------

    fp = open( out_filename, 'w' )

    pos = 0

    max_pos = len(X) - num_inp - num_out

    line = []
    for i in range(num_inp):
        line.append( 'in%d' % ( i, ) )
    for i in range(num_out):
        line.append( 'out%d' % ( i, ) )
    fp.write( '%s\n' % ( ','.join(line), ) )
            
    while( pos < max_pos ):
        line = []
        for i in range(num_inp):
            line.append( '%3.1f' % ( X[pos+i], ) )
        for i in range(num_out):
            line.append( '%3.1f' % ( X[pos+num_inp+i], ) )
        fp.write( '%s\n' % ( ','.join(line), ) )
        pos += num_inc

    fp.close()

    return 0


#/ =======================================================================================
def usage( pn, msg=None ):
    #/ -----------------------------------------------------------------------------------
    if ( None != msg ):
        sys.stderr.write( '\n%s\n' %(msg,) )
    sys.stderr.write( """
USAGE: %s input.csv col output.csv nin nout step
  input.csv  - multi column input file
  col        - input column to use
  output.csv - output file
  nin        - number of inputs
  nout       - number of outputs
  step       - window increment

Example: %s Power.csv 1 training.csv 16 2 8

""" % ( pn, pn, ) )

    
#/ =======================================================================================
def main( argc, argv ):
    #/ -----------------------------------------------------------------------------------

    use_col = 0
    num_inp = 0
    num_out = 0
    num_inc = 1

    if ( 7 < argc ):
        return usage(argv[0],'Too many arguments')
    else:
        if ( 5 < argc ):
            use_col = int( argv[2] )
            num_inp = int( argv[4] )
            num_out = int( argv[5] )
            if ( 4 == argc ):
                num_inc = int( argv[6] )
        else:
            return usage(argv[0],'Too few arguments')

    return ProcessFile( argv[1], argv[3], use_col, num_inp, num_out, num_inc )


#/ =======================================================================================
if ( '__main__' == __name__ ):
    sys.exit( main( len( sys.argv ), sys.argv ) )
#/ =======================================================================================
#/ **                             S L I D I N G W I N D O W                             **
#/ =========================================================================== END FILE ==
