// ====================================================================== BEGIN FILE =====
// **                                       F G A                                       **
// =======================================================================================

#include <fga.hh>

// =======================================================================================
void FGA::destroy( void ) {
  // -------------------------------------------------------------------------------------
  if ( (FLIB**)0 != popA ) {
    for ( int i=0; i<num_pop; i++ ) {
      if ( (FLIB*)0 != popA[i] ) {
        delete popA[i];
        popA[i] = (FLIB*)0;
      }
    }
    delete popA;
  }
    
  if ( (FLIB**)0 != popB ) {
    for ( int i=0; i<num_pop; i++ ) {
      if ( (FLIB*)0 != popB[i] ) {
        delete popB[i];
        popB[i] = (FLIB*)0;
      }
    }
    delete popB;
  }

  popA = (FLIB**)0;
  popB = (FLIB**)0;

  num_state = 0;
  num_pop   = 0;
  num_tour  = 0;
}


// =======================================================================================
FGA::FGA( FGA::Builder* B )
    : popA(0), popB(0), num_state(0), num_pop(0), num_tour(0), pCross(0.0), pMutate(0.0) {
  // -------------------------------------------------------------------------------------
  num_state = B->getState();
  num_pop   = B->getPop();
  num_tour  = B->getTour();
  pCross    = B->getCross();
  pMutate   = B->getMutate();

  popA = new FLIB*[ num_pop ];
  popB = new FLIB*[ num_pop ];

  for ( int i=0; i<num_pop; i++ ) {
    popA[i] = new FLIB( num_state );
    popB[i] = new FLIB( num_state );
    popA[i]->randomize();
    popB[i]->randomize();
  }

}



// =======================================================================================
void FGA::score( int* idx, FLIB** pop, int np, int* pat, int n, int m ) {
  // -------------------------------------------------------------------------------------
  int    min_index = -1;
  int    max_index = -1;
  double min_score =  1.0e10;
  double max_score = -1.0e10;
  
  for ( int i=0; i<np; i++ ) {
    double test = pop[i]->score( pat, n, m );
    if ( test > max_score ) {
      max_score = test;
      max_index = i;
    }
    if ( test < min_score ) {
      min_score = test;
      min_index = i;
    }
}

  idx[0] = min_index;
  idx[1] = max_index;
}


// =======================================================================================
int FGA::select( FLIB** pop, int np, int ts ) {
  // -------------------------------------------------------------------------------------
  int    mi = roll(np);
  double ms = pop[mi]->getMetric();
  for ( int i=1; i<ts; i++ ) {
    int    ki = roll(np);
    double ks = pop[ki]->getMetric();
    if ( ks > ms ) {
      ms = ks;
      mi = ki;
    }
  }
  return mi;
}


// =======================================================================================
void FGA::cross( FLIB* fc1, FLIB* fc2, FLIB* fp1, FLIB* fp2 ) {
  // -------------------------------------------------------------------------------------
  FLIB::cross( fc1, fc2, fp1, fp2 );
}


// =======================================================================================
void FGA::mutate( FLIB* D, FLIB* S, double pm ) {
  // -------------------------------------------------------------------------------------
  FLIB::mutate( D, S, pm );
}


// =======================================================================================
void FGA::step( int* idx, int* pat, int n, int m ) {
  // -------------------------------------------------------------------------------------

  for ( int i=0; i<num_pop; i+=2 ) {
    int t1 = select( popA, num_pop, num_tour );
    int t2 = select( popA, num_pop, num_tour );
    if ( thresh( pCross ) ) {
      if ( t1 == t2 ) {
        popB[i]->copy(   popA[t1] );
        popB[i+1]->copy( popA[t2] );
      } else {
        cross( popB[i], popB[i+1], popA[t1], popA[t2] );
      }
    } else {
      popB[i]->copy(   popA[t1] );
      popB[i+1]->copy( popA[t2] );
    }
  }

  for ( int i=0; i<num_pop; i++ ) {
    mutate( popA[i], popB[i], pMutate );
  }
  
  score( idx, popA, num_pop, pat, n, m );
}


// =======================================================================================
void FGA::evolve( int* idx, int max_gen, int report, int* pat, int n, int m ) {
  // -------------------------------------------------------------------------------------

  score( idx, popA, num_pop, pat, n, m );

  fprintf( stdout, "%d: %f\n", 0, popA[idx[1]]->getMetric() );

  for ( int gen=0; gen<max_gen; gen++ ) {
    int g = gen+1;
    step( idx, pat, n, m );
    if ( 0 == ( g % report ) ) {
      fprintf( stdout, "%d: %f %f\n", g, popA[idx[0]]->getMetric(), popA[idx[1]]->getMetric() );
    }


    

    
  }

  fprintf( stdout, "%d: %f\n", max_gen, popA[idx[1]]->getMetric() );
}

// =======================================================================================
// **                                      F L I B                                      **
// =========================================================================== END FILE ==
