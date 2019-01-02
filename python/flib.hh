// ====================================================================== BEGIN FILE =====
// **                                      F L I B                                      **
// =======================================================================================

#ifndef __FLIB_HH
#define __FLIB_HH

#include <stdlib.h>
#include <stdio.h>


#define MSG(a) fprintf( stdout, "%s: %d: %s\n", __FILE__, __LINE__, (a) )
#define MARK MSG("<<=============================>>")

// =======================================================================================
class FLIB {
  // -------------------------------------------------------------------------------------
 private:
  
  int*   state;
  int    current;
  int    num_state;
  double metric;

  void init( int ns, bool r=false );

  void destroy(void);

 public:

  FLIB  ( void );
  FLIB  ( int ns, bool r=false );
  FLIB  ( FLIB* src );
  ~FLIB ( void );

  double getMetric ( void ) { return metric; }
  int    numState  ( void ) { return num_state; }

  void   randomize ( void );
  void   copy      ( FLIB* src );
  void   reset     ( void );
  void   set       ( int new_state );
  int    step      ( int inp );
  double score     ( int* pat, int n, int m );
  void   display   ( FILE* fp=stdout, bool sd=false );

  int*   toState   ( void );
  void   fromState ( int* param, int ns );

  static void   mutate ( FLIB* dr, FLIB* sr, double pmutate );
  static void   cross  ( FLIB* c1, FLIB* c2, FLIB* p1, FLIB* p2 );
};

inline double uniform ( void )             { return drand48(); }
inline int    roll    ( int n )            { return (int)(lrand48() % (long)n); }
inline int    rbit    ( double threshold ) { return ((threshold < drand48()) ? (0) : (1)); }
inline bool   thresh  ( double threshold ) { return ((threshold < drand48()) ? (false) : (true)); }
void seed_set( long int ss = 0 );

#endif

// =======================================================================================
// **                                      F L I B                                      **
// =========================================================================== END FILE ==
