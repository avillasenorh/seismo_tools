#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define TRUE	1
#define FALSE	0

#define PI 3.141592653589793
#define FAC 0.9932773

/*******************************************************************************
Function: gdelaz

Purpose: Computes distance and azimuth between two points.
         Input are floats in degrees.
         Computations are done in double precision and radians.
         Output are floats in degrees.

         If the flag geocen is set to TRUE, then the latitudes are
         converted to geocentric.  The flat geocen == FALSE is
         usefull to do computations on a sphere, or when the coordinates
         (latitude) have already been converted to geocentric.

The spherical trigonometry formulas used can be found on pages 30-31 of:

Snyder, J.P. (1987). Map projections--a working manual, U.S. Geological Survey
  Professional Paper 1395, 383 pp.


The symbol FAC is defined as (1 - e*e) where e is the "eccentricity" of the
ellipsoid.  e = sqrt(1 - (b*b)/(a*a)) where a = equatorial radius and
b = polar radius.

For the Clarke (1986) ellipsoid:  e*e = 0.006768658   => FAC = 0.99323134
For the GRS 80 ellipsoid:         e*e = 0.0066943800  => FAC = 0.99330562

Other values used are:

FAC = 0.99328               (in the bulletins of the ISS)
FAC = 0.9932773             (subroutine geocen in progs.f)
FAC = 0.993305621334896     (subroutines bgeocen, geogrf in bounce.f)
*******************************************************************************/

void gdelaz(float lon1, float lat1, float lon2, float lat2,
            float *delta, float *azim, int geocen)
{
	double	dum;
	double	xl1, yl1, xl2, yl2;
	double	t1, t2, c2, xl, yl;
	double	c, az;

	/* valid latitudes */

	if (lat1 < -90. || lat1 > 90. || lat2 < -90. || lat2 > 90.) {
		fprintf(stderr, "ERROR: invalid latitude: %f %f\n",lat1,lat2);
		exit(1);
	}

	/* valid longitudes */
        if (lon1 > 180.) lon1 -= 360.;
        if (lon2 > 180.) lon2 -= 360.;
	if (lon1 < -180. || lon1 > 180. || lon2 < -180. || lon2 > 180.) {
		fprintf(stderr, "ERROR: invalid longitude: %f %f\n",lon1,lon2);
		exit(1);
	}

	if (geocen) {
		if (lat1 == 90.) {
			yl1 = (double)(PI/2.);
		} else if (lat1 == -90.) {
			yl1 = (double)(-PI/2.);
		} else {
			dum = ((double)lat1)*PI/180.;
			yl1 = atan(FAC*tan(dum));
		}
		if (lat2 == 90.) {
			yl2 = (double)(PI/2.);
		} else if (lat2 == -90.) {
			yl2 = (double)(-PI/2.);
		} else {
			dum = ((double)lat2)*PI/180.;
			yl2 = atan(FAC*tan(dum));
		}
	} else {
		yl1 = ((double)lat1)*PI/180.;
		yl2 = ((double)lat2)*PI/180.;
	}
	xl1 = ((double)lon1)*PI/180.;
	xl2 = ((double)lon2)*PI/180.;

	/* distance */

	t1 = sin((yl2-yl1)/2.);
	t2 = sin((xl2-xl1)/2.);
	c2 = sqrt(t1*t1 + cos(yl1)*cos(yl2)*t2*t2);
	c = 2.*asin(c2);

	/* azimuth */

	xl = cos(yl2)*sin(xl2-xl1);
	yl = cos(yl1)*sin(yl2) - sin(yl1)*cos(yl2)*cos(xl2-xl1);
	az = atan2(xl,yl);

	/* backazimuth ?? */
	
	/* quadrant adjustments ?? */

	/* go back to degrees */

	*delta = (float)(c*180./PI);
	*azim =  (float)(az*180./PI);

}
