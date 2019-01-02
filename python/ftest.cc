// ====================================================================== BEGIN FILE =====
// **                                     F T E S T                                     **
// =======================================================================================

#include <fga.hh>


// =======================================================================================
void display( FLIB* F ) {
  // -------------------------------------------------------------------------------------
  int* p = F->toState();
  int  n = F->numState() * 4;
  for ( int i=0; i<n; i++ ) {
    fprintf( stdout, "%d", p[i] );
  }
  fprintf( stdout, "\n" );
}

// =======================================================================================
int main( void ) {
  // -------------------------------------------------------------------------------------

  seed_set();
  
  FLIB* F = new FLIB(7);
  F->randomize();
  F->display(stdout,true);
  
  int value = 1;

  int npat = 60;
  int* pat = new int[npat];
  pat[0] = value;
  F->reset();
  for ( int i=1; i<npat; i++ ) {
    value = F->step( value );
    pat[i] = value;
  }

  int* p = F->toState();

  FLIB* G = new FLIB();

  G->fromState( p, 7 );
  G->display(stdout,true);
  
  value = 1;
  int nkat = 60;
  int* kat = new int[npat];
   kat[0] = value;
  G->reset();
  for ( int i=1; i<nkat; i++ ) {
    value = G->step( value );
    kat[i] = value;
  }

  for ( int i=0; i<npat; i++ ) { fprintf(stdout,"%d",pat[i]); } fprintf(stdout,"\n");
  for ( int i=0; i<nkat; i++ ) { fprintf(stdout,"%d",kat[i]); } fprintf(stdout,"\n");

  FLIB* Q = new FLIB(7);
  
  display(G);
  FLIB::mutate( Q, G, 0.50 );
  display(Q);

  fprintf( stdout, "\n--------------\n\n" );

  FLIB* p1 = new FLIB( 10, true );
  FLIB* p2 = new FLIB( 10, true );
  display(p1);
  display(p2);
  FLIB* c1 = new FLIB( 10, true );
  FLIB* c2 = new FLIB( 10, true );
  FLIB::cross( c1, c2, p1, p2 );
  display(c1);
  display(c2);
  

  return 0;
}

// =======================================================================================
// **                                     F T E S T                                     **
// =========================================================================== END FILE ==
