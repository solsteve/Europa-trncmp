// ====================================================================== BEGIN FILE =====
// **                                       F G A                                       **
// =======================================================================================

#ifndef __FGA_HH
#define __FGA_HH

#include <flib.hh>


// =======================================================================================
class FGA {
  // -------------------------------------------------------------------------------------

 public:

  // =====================================================================================
  class Builder {
    // -----------------------------------------------------------------------------------

   private:
    int    ns;
    int    np;
    int    nt;
    double pm;
    double pc;

   public:

    Builder( int s, int p ) : ns(s), np(p), nt(7), pm(0.05), pc(0.95) {};

    FGA::Builder* tour    ( int    n ) { nt=n; return this; };
    FGA::Builder* pCross  ( double n ) { pc=n; return this; };
    FGA::Builder* pMutate ( double n ) { pm=n; return this; };

    int    getState  ( void ) { return ns; };
    int    getPop    ( void ) { return np; };
    int    getTour   ( void ) { return nt; };
    double getCross  ( void ) { return pc; };
    double getMutate ( void ) { return pm; };

    FGA* build(void) { return new FGA( this ); };

  }; // end class FGA::Builder

 protected:
  
  FLIB** popA;
  FLIB** popB;

  int num_state;
  int num_pop;
  int num_tour;

  double pCross;
  double pMutate;

  FGA( FGA::Builder* B );

  void destroy( void );
  
 public:

  ~FGA( void ) { destroy(); };

  void step   ( int* idx, int* pat, int n, int m );
  void evolve ( int* idx, int max_gen, int report, int* pat, int n, int m );

  FLIB* get( int i ) { return popA[i]; };
  

  static void score   ( int* idx, FLIB** pop, int np, int* pat, int n, int m );
  static int select  ( FLIB** pop, int np, int ts );

  static void cross  ( FLIB* fc1, FLIB* fc2, FLIB* fp1, FLIB* fp2 );
  static void mutate ( FLIB* D,   FLIB* S, double pm );

}; // end class FGA

#endif

// =======================================================================================
// **                                       F G A                                       **
// =========================================================================== END FILE ==
