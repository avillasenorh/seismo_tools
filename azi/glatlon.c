#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define TRUE	1
#define FALSE	0

#define PI 3.141592653589793
#define FAC 0.9932773

/*******************************************************************************
Function: glatlon

Purpose: Computes the coordinates of a point given a reference point
         (lon1,lat1) and the distance and azimuth from that point.
         Input are floats in degrees.
         Computations are done in double precision and radians.
         Output are floats in degrees.

         If the flag geocen is set to TRUE, then the latitudes are
         converted to geocentric.  The flat geocen == FALSE is
         usefull to do computations on a sphere, or when the coordinates
         (latitude) have already been converted to geocentric.

The spherical trigonometry formulas used can be found on page 31 of:

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

void glatlon(float lon1, float lat1, float delta, float azim,
            float *lon2, float *lat2, int geocen)
{
	double	dum;
	double	xl1, yl1, xl2, yl2;
	double	c2, xl, yl, dlon;
	double	c, az;

	/* valid latitude */
	if (lat1 < -90. || lat1 > 90. ) {
		fprintf(stderr, "ERROR: invalid latitude: %f\n",lat1);
		exit(1);
	}

	/* valid longitude */
        if (lon1 > 180.) lon1 -= 360.;
	if (lon1 < -180. || lon1 > 180.) {
		fprintf(stderr, "ERROR: invalid longitude: %f\n",lon1);
		exit(1);
	}

	/* valid distance */
	if (delta < 0. || delta > 180.) {
		fprintf(stderr, "ERROR: invalid distance: %f\n",delta);
		exit(1);
	}
	c = ((double)delta)*PI/180.;

	/* valid azimuth */
        if (azim > 180.) azim -= 360.;
	if (azim < -180. || azim > 180.) {
		fprintf(stderr, "ERROR: invalid azimuth: %f\n",azim);
		exit(1);
	}
	az = ((double)azim)*PI/180.;

	if (geocen) {
		if (lat1 == 90.) {
			yl1 = (double)(PI/2.);
		} else if (lat1 == -90.) {
			yl1 = (double)(-PI/2.);
		} else {
			dum = ((double)lat1)*PI/180.;
			yl1 = atan(FAC*tan(dum));
		}
	} else {
		yl1 = ((double)lat1)*PI/180.;
	}
	xl1 = ((double)lon1)*PI/180.;

	/* latitude */

	c2 = sin(yl1)*cos(c) + cos(yl1)*sin(c)*cos(az);
	yl2 = asin(c2);

	/* longitude */

	xl = sin(c)*sin(az);
	yl = cos(yl1)*cos(c) - sin(yl1)*sin(c)*cos(az);
	dlon = atan2(xl,yl);
	*lon2 = lon1 + (float)dlon;
	
	/* quadrant adjustments */

	*lon2 = lon1 + (float)(dlon*180./PI);

	/* convert latitude to degrees (and to geographic) */

	if (geocen) {
		*lat2 = (float)(atan(tan(yl2)/FAC)*180./PI);
	} else {
		*lat2 = (float)(yl2*180/PI);
	}

}
