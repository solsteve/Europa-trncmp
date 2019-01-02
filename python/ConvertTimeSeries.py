#!/usr/bin/python3
#/ ====================================================================== BEGIN FILE =====
#/ **                         C O N V E R T T I M E S E R I E S                         **
#/ =======================================================================================

import numpy  as np
import time, sys

J2000 = 946684800.0

COLUMN_NAMES = [
    'Date','Time',
    'Global_active_power',
    'Global_reactive_power',
    'Voltage',
    'Global_intensity',
    'Sub_metering_1',
    'Sub_metering_2',
    'Sub_metering_3'
]

#/ =======================================================================================
def cubic_interpolate( xt, T0, T1, T2, T3 ):
    #/ -----------------------------------------------------------------------------------
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
    
#/ =======================================================================================
def linear_interpolate( x, p0, p1, xi, yi ):
    #/ -----------------------------------------------------------------------------------
    x0 = p0[xi]
    y0 = p0[yi]
    x1 = p1[xi]
    y1 = p1[yi]
    dif = x1 - x0
    m = (y1 - y0) / dif
    b = (x1*y0 - x0*y1)/dif
    return x * m + b

#/ =======================================================================================
fp = open( 'test.csv', 'r' )
dummy = fp.readline()

table = []
for line in fp:
    parts = line.strip().split(';')
    if ( 2 < len(parts) ):
        try:
            dtm = time.mktime(time.strptime( parts[0]+' '+parts[1], '%d/%m/%Y %H:%M:%S' ))-946684800.0
            pwr = float( parts[2] )
            s1 = float( parts[6] )
            s2 = float( parts[7] )
            s3 = float( parts[8] )
            table.append( (dtm,pwr,s1,s2,s3) )
        except ValueError:
            pass

fp.close()

mx_index = len(table)-2
mn_index = 1

tm = 219600000.0
dt = 60.0

while( tm < table[mn_index][0] ):
    tm += dt
    
t1 = 1

fp = open( 'Power.csv', 'w' )

fp.write( 'Time,Power,Sub1,Sub2,Sub3\n' )

count  = 0.0e0
tot_pw = 0.0e0
tot_s1 = 0.0e0
tot_s2 = 0.0e0
tot_s3 = 0.0e0

try:
    while 1:
        while (table[t1][0] < tm):
            t1 += 1
            if ( t1 > mx_index ):
                raise 1
        t0 = t1

        while (table[t0][0] > tm):
            t0 -= 1
            if ( t0 < mn_index ):
                print( 'too low' )
                raise 2

        dif = table[t1][0] - table[t0][0]

        if ( dif > 0.01 ):
            pwr = linear_interpolate( tm, table[t0], table[t1], 0, 1 )
            s1  = linear_interpolate( tm, table[t0], table[t1], 0, 2 )
            s2  = linear_interpolate( tm, table[t0], table[t1], 0, 3 )
            s3  = linear_interpolate( tm, table[t0], table[t1], 0, 4 )
            #p3 = cubic_interpolate( tm, table[t0-1], table[t0], table[t1], table[t1+1] )
            #fp.write( '%12.1f,%7.3f,%7.3f,%7.3f\n' % ( tm, p2, p3, p3/p2 ) )
            fp.write( '%12.1f,%7.3f,%7.3f,%7.3f,%7.3f\n' % ( tm, pwr, s1, s2, s3, ) )
            count  += 1.0e0
            tot_pw += pwr
            tot_s1 += s1
            tot_s2 += s2
            tot_s3 += s3
        tm += dt
    
except:
    pass        

fp.close()

print(count)
print(tot_pw,tot_s1,tot_s2,tot_s3)
print(tot_pw/count,tot_s1/count,tot_s2/count,tot_s3/count)

#/ =======================================================================================
#/ **                         C O N V E R T T I M E S E R I E S                         **
#/ =========================================================================== END FILE ==
