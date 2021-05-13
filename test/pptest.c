#include "test/pptest.h"

#define DOUBLEPI 2*PI

// should be true
#if DOUBLEPI > 6

  // should be true
  #if defined VARPREFIX

    // this should be included
    int MAKEVARNAME(1) = 1;

  #endif

  // should be false
  #if ! defined PI

    // this shouldn't be included
    int MAKEVARNAME(2) = 2;

    // should be false
  #elif DOUBLEPI > 7

    // this shouldn't be included
    int MAKEVARNAME(3) = 3;

  #else

    // this should be included
    int MAKEVARNAME(4) = 4;

  #endif

#endif

// should be false
#ifndef PI

  // this shouldn't be included
  int MAKEVARNAME(5) = 5;

#endif

#undef PI

// should be true
#ifndef PI

  // this should be included
  int MAKEVARNAME(6) = 6;

#endif

