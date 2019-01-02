#/ ====================================================================== BEGIN FILE =====
#/ **                                      F L I B                                      **
#/ =======================================================================================
import numpy.random as rnd
import sys

SYM='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'
MAX_SYM=len(SYM)-1
START=SYM[0]

#/ =======================================================================================
class Flib:
    #/ -----------------------------------------------------------------------------------
    def __init__(self):
        #/ -------------------------------------------------------------------------------
        self.map     = {}
        self.current = None
        self.score   = 0
        self.nState  = 0
        
    #/ -----------------------------------------------------------------------------------
    def randomize(self,n):
        #/ -------------------------------------------------------------------------------
        if ( n>MAX_SYM ):
            sys.stderr.write('Only %d or fewer states supported' % (MAX-SYM,))
            sys.exit(1)
        box0=[]
        box1=[]
        for i in range(n):
            box0.append(SYM[i])
            box1.append(SYM[i])
        rnd.shuffle(box0)
        rnd.shuffle(box1)
        self.map = {}
        for i in range(n):
            key  = box1[i]
            nxt0 = box0[i]
            nxt1 = box1[(i+1) % n]
            out0 = (0, 1)[0.3 < rnd.uniform()]
            out1 = (0, 1)[0.3 < rnd.uniform()]
            self.map[key] = ((nxt0,out0),(nxt1,out1))
        self.current = self.map[START]
        self.score   = 0

    #/ ===================================================================================
    def reset(self):
        #/ -------------------------------------------------------------------------------
        self.current=self.map[START]

    #/ ===================================================================================
    def set(self,key):
        #/ -------------------------------------------------------------------------------
        self.current=self.map[key]

    #/ ===================================================================================
    def change(self,input):
        #/ -------------------------------------------------------------------------------
        next   = self.current[input]
        output = next[1]
        self.current = self.map[next[0]]
        return output

    #/ ===================================================================================
    def display(self):
        #/ -------------------------------------------------------------------------------
        for x in self.map:
            print( x,' ==> ',self.map[x] )

    #/ ===================================================================================
    def toParam(self):
        #/ -------------------------------------------------------------------------------
        P = []
        for x in self.map:
            rec = self.map[x]
            P.append( '%s' % (x,) )
            P.append( '%s' % (rec[0][0],) )
            P.append( '%d' % (rec[0][1],) )
            P.append( '%s' % (rec[1][0],) )
            P.append( '%d' % (rec[1][1],) )
        return P
        

    #/ ===================================================================================
    def fromParam(self, pat):
        #/ -------------------------------------------------------------------------------
        n = len(pat)
        self.map = {}
        if (0 == len(pat) % 5):
            for i in range(0,n,5):
                key  = pat[i]
                nxt0 = pat[i+1]
                out0 = int(pat[i+2])
                nxt1 = pat[i+3]
                out1 = int(pat[i+4])
                self.map[key] = ((nxt0,out0),(nxt1,out1))
            self.current=self.map[START]
            self.score = 0
        else:
            sys.stderr.write('parameter is a string that is not a multiple of 5')
            sys.exit(1)

    #/ ===================================================================================
    def measure(self, pat, n):
        #/ -------------------------------------------------------------------------------
        self.score = 0
        m = len(pat)
        self.reset()
        for i in range(n):
            cur  = int(pat[i%m])
            test = int(pat[(i+1)%m])
            nxt  = self.change(cur)
            if ( test == nxt ):
                self.score += 1
        return self.score

#/ ===================================================================================
def clone(src,noise=None):
    #/ -------------------------------------------------------------------------------
    nF = Flib()
    if ( None==noise ):
        for key in src.map:
            rec = src.map[key]
            nx0 = rec[0][0]
            op0 = rec[0][1]
            nx1 = rec[1][0]
            op1 = rec[1][1]
            nF.map[key] = ((nx0,op0),(nx1,op1))
    else:
        for key in src.map:
            rec = src.map[key]
            #/ -------------------------------
            if ( noise < rnd.uniform() ):
                pass
            else:
                nx0 = rec[0][0]
            #/ -------------------------------
            if ( noise < rnd.uniform() ):
                op0 = (rec[0][1]+1)%2
            else:
                op0 = rec[0][1]
            #/ -------------------------------
            if ( noise < rnd.uniform() ):
                pass
            else:
                nx1 = rec[1][0]
            #/ -------------------------------
            if ( noise < rnd.uniform() ):
                op1 = (rec[1][1]+1)%2
            else:
                op1 = rec[1][1]
            #/ -------------------------------
            nF.map[key] = ((nx0,op0),(nx1,op1))
        
            
#/ =======================================================================================
def main( argc, argv ):
    #/ -----------------------------------------------------------------------------------

    rnd.seed()
    
    F = Flib()
    F.randomize(7)

    F.display()

    value = 1

    sys.stdout.write( '%d' % (value,) )

    env = []
    for i in range(60):
        value = F.change( value )
        env.append( '%d' % (value,) )
        
    print(''.join(env))

    P = F.toParam()

    n2 = int(len(P)/5)

    print(len(P),n2)
    
    G = Flib()

    print( ''.join(P) )

    G.fromParam(P)

    G.display()

    k = len(env)-1
    x = F.measure(env,k)

    print( k, x, 100.0*float(x)/float(k) )

    return 0
    
#/ =======================================================================================
if ( '__main__' == __name__ ): sys.exit( main( len( sys.argv ), sys.argv ) )
#/ =======================================================================================
#/ **                                      F L I B                                      **
#/ =========================================================================== END FILE ==
