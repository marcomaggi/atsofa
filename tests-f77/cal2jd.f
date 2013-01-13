* Part of: ATSOFA
* Contents: test for the function "cal2jd"
* Date: Sun Jan 13, 2013
*
* Abstract
*
*       Test conversion  Gregorian Calendar to Julian  Date.  The Julian
*       Date is  the interval  of time  in days and  fractions of  a day
*       since -4714-11-24T12:00:00Z (November 24, -4714 at noon, UTC).
*
* Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
*
* See the COPYING file for copying permissions.

      program cal2jd_f
      implicit none

      integer exit_code
      double precision result
*     Julian day number at 12:00.
      double precision expected_result

*     Input
*     Year in Gregorian calendar.
      integer iy
*     Month in Gregorian calendar.
      integer im
*     Day in Gregorian calendar.
      integer id

*     Output
*     Modified Julian Day zero-point: always 2400000.5.
      double precision djm0
*     Modified Julian Date for 0 hrs.
      double precision djm
*     Return code
*     0  = OK
*     -1 = bad year   (Note 3: JD not computed)
*     -2 = bad month  (JD not computed)
*     -3 = bad day    (JD computed)
      integer status

      exit_code = 1
      result = 0.0
      expected_result = 2456306.0

      iy = 2013
      im = 1
      id = 13

      call cal2jd(iy, im, id, djm0, djm, status)
      if (0.EQ.status) then
         result = djm + djm0 + 0.5
      end if

      if ((0.EQ.status) .and. (expected_result.EQ.result)) then
         exit_code = 0
      else
         exit_code = 1
      end if
      call exit (exit_code)
      end

*** end of file
