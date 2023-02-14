MODULE fitdata
	
	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 28 Aug. 1996
	!==========================================================================
	!=======
	!Purpose
	!=======
	! Module fitdata contains the numeric potential
	! nr	--- the number of points
	! r		--- the vector of points
	! y		--- the vector of potential values at the points in r
	! wt	--- the vector of weighting factors for the fit
	!==========================================================================
	IMPLICIT NONE

	INTEGER :: nr
	
	DOUBLE PRECISION, DIMENSION(nr), SAVE :: r, y, wt
	! r is dat(:,1)    y is dat(:,2)   wt is dat(:,3)  

	DOUBLE PRECISION, DIMENSION(nr,3) :: dat																					
	

END MODULE fitdata