/*
  Part of: ATSOFA
  Contents: test for the function "iauCal2jd()"
  Date: Sun Jan 13, 2013

  Abstract

	Test conversion Gregorian Calendar to Julian Date.

  Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the COPYING file for copying permissions.
*/

#include <stdio.h>
#include <stdlib.h>
#include <sofa.h>

int
main (int argc, const char *const argv[])
{
  int		exit_code	= EXIT_FAILURE;
  double	result		= 0.0;
  double	expected_result = 2456306.0;

  /* Input */
  int		iy;	/* Year in Gregorian calendar. */
  int		im;	/* Month in Gregorian calendar. */
  int		id;	/* Day in Gregorian calendar. */

  /* Output */
  double	djm0;	/* Modified Julian Day zero-point: always 2400000.5. */
  double	djm;	/* Modified Julian Date for 0 hrs. */
  int		status;	 /* 0 = OK
			    -1 = bad year   (Note 3: JD not computed)
			    -2 = bad month  (JD not computed)
			    -3 = bad day    (JD computed) */

  iy = 2012;
  im = 1;
  id = 13;

  status = iauCal2jd(iy, im, id, &djm0, &djm);
  if (0 == status) {
    result = djm + djm0;
  }

  fprintf(stderr, "Gregorian: %d/%02d/%02d\nExpected Julian day: %f\nComputed Julian day: %f\n",
	  iy, im, id, expected_result, result);

  if (0 == status) {
    exit_code = EXIT_SUCCESS;
  } else {
    exit_code = EXIT_FAILURE;
  }
  exit(exit_code);
}

/* end of file */
