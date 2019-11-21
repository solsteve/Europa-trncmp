#!/usr/bin/python3
#/ ====================================================================== BEGIN FILE =====

import sys
import numpy as np

DEG2RAD = np.pi/1.8e2
RAD2DEG = 1.8e2/np.pi

A0 = 6.36748980392568e+06
A2 = 1.069258979408e+04
A4 = 2.244474504e+01
A6 = 5.102528e-02


#/ =======================================================================================
def earth_radius( lat ):
    #/ -----------------------------------------------------------------------------------    
    r1 = DEG2RAD * lat  
    r2 = r1 + r1
    r4 = r2 + r2
    r6 = r4 + r2

    return A0 + A2*np.cos(r2) - A4*np.cos(r4) + A6*np.cos(r6)

#/ =======================================================================================
def haversine(lat1, lon1, lat2, lon2):
    #/ -----------------------------------------------------------------------------------    
    R = 6378137.0
 
    dLat = DEG2RAD*(lat2 - lat1)
    dLon = DEG2RAD*(lon2 - lon1)
    lat1 = DEG2RAD*(lat1)
    lat2 = DEG2RAD*(lat2)
 
    a = np.sin(5.0e-1*dLat)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(5.0e-1*dLon)**2
    c = 2.0e0 * np.arcsin(np.sqrt(a))
 
    return (R * c)


#/ =======================================================================================
def central_angle( lon1, lon2, lat1, lat2 ):
    #/ -----------------------------------------------------------------------------------    
    hs = 1.0e0 - (1.0e0 - np.cos(lon1)*np.cos(lon2)*np.cos(lat1)*np.cos(lat2) - \
                   np.cos(lat1)*np.cos(lat2)*np.sin(lon1)*np.sin(lon2) - np.sin(lat1)*np.sin(lat2))

    return np.arccos(hs)

    
#/ =======================================================================================
def CenterAngle( ang ):
    #/ -----------------------------------------------------------------------------------

    S = 0.0e0
    C = 0.0e0

    for a in ang:
        S += np.sin( a )
        C += np.cos( a )

    return np.arctan( S / C )



#/ =======================================================================================
def checkCenter( db ):
    #/ -----------------------------------------------------------------------------------

    mse = 0.0e0
    for rec in db:
        lat = [ rec['latUL'], rec['latUR'], rec['latLL'], rec['latLR'] ]
        lon = [ rec['lonUL'], rec['lonUR'], rec['lonLL'], rec['lonLR'] ]

        rlat = rec['clat']
        rlon = rec['clon']
        
        clat = CenterAngle( lat )
        clon = CenterAngle( lat )

        dlat = clat - rlat
        dlon = clon - rlon

        mse += ( (dlat*dlat) + (dlon*dlon) )

    return mse

#/ =======================================================================================
def record( line ):
    #/ -----------------------------------------------------------------------------------
    dict = {}
    parts = line.split(',')
    dict['area']  = float( parts[0] )
    dict['clon']  = float( parts[1] )
    dict['clat']  = float( parts[2] )
    dict['lonUL'] = float( parts[3] )
    dict['latUL'] = float( parts[4] )
    dict['lonUR'] = float( parts[5] )
    dict['latUR'] = float( parts[6] )
    dict['lonLL'] = float( parts[7] )
    dict['latLL'] = float( parts[8] )
    dict['lonLR'] = float( parts[9] )
    dict['latLR'] = float( parts[10] )
    dict['xUL']   = float( parts[11] )
    dict['yUL']   = float( parts[12] )
    dict['xUR']   = float( parts[13] )
    dict['yUR']   = float( parts[14] )
    dict['xLL']   = float( parts[15] )
    dict['yLL']   = float( parts[16] )
    dict['xLR']   = float( parts[17] )
    dict['yLR']   = float( parts[18] )
    return dict


#/ =======================================================================================
def main( argc, argv ):
    #/ -----------------------------------------------------------------------------------




    print(  haversine( 26.871817, -89.806556,  25.707974, -89.082590 ) )
    print(  haversine(36.12,-86.67,33.94,-118.40) )

    sys.exit(0)








    
    fp = open( 'Geo-test.dat', 'r' )
    a  = fp.readline()

    db = []
    for line in fp:
        db.append( record( line ) )
    fp.close()
    
    a = checkCenter( db )
    print( a )
        
    return 0

#/ =======================================================================================
if ( '__main__' == __name__ ): sys.exit( main( len( sys.argv ), sys.argv ) )
#/ =========================================================================== END FILE ==
