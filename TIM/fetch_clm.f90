!=======================================================================================!
SUBROUTINE fetch_clm (lat, lon, reso, year, nyrs, clm, sce, inDir)
!=======================================================================================!
! This subroutine reads daily climatic inputs at 0.5 x 0.5 degree resolution from annual 
! netcdf files and returns vectors of daily climatic input field for a given number of
! years (nyrs) at a demanded resolution (reso) and converted to needed units. The 
! netcdf files must be located in the appropriate directory for climate (WATCH of GCM
! name) and scenario (hist or rcp?p?) which will read inDir/clm/sce.
!
! At the moment this is limited to WATCH data, GCM data is to follow.
!
! The outputs are as follows:
!   Variable                                   Variable name           Units
!---------------------------------------------------------------------------------------!
!   Daily total precipiation                   prec_out                (mm d-1)
!   Daily minimum surface temperature          tmin_out                (degC)
!   Daily maximum surface temperature          tmax_out                (degC)
!   Daily mean surface temperature             tair_out                (degC)
!   Daily mean atmospheric pressure            pres_out                (hPa)
!   Daily mean atmospheric absolute humidity   humi_out                (hPa)
!   Daily mean downwelling shortwave radiation rads_out                (W m-2)
!   Daily total snowfall                       snow_out                (kg m-2 s-1)
!   Daily mean downwelling longwave radiation  radl_out                (W m-2)
!   Daily mean near-surface wind speed at 10m  wind_out                (m s-1)
!---------------------------------------------------------------------------------------!

 USE netcdf 

!---------------------------------------------------------------------------------------!
 IMPLICIT NONE
!---------------------------------------------------------------------------------------!
! Input variables
!---------------------------------------------------------------------------------------!
 REAL, INTENT (IN) :: lat           ! Mid-grid cell latitude of simulated site    (deg)
 REAL, INTENT (IN) :: lon           ! Mid-grid cell longitude of simulated site   (deg)
 REAL, INTENT (IN) :: reso          ! Demanded resolution for output data         (deg)
 INTEGER, INTENT (IN) :: year       ! Start year for data extraction
 INTEGER, INTENT (IN) :: nyrs       ! Number of years for which to extract climate data
 CHARACTER (LEN = 4) :: yr          ! Julian year for which data is extracted
 CHARACTER (LEN = 14) :: clm        ! Name of climate directory containing netcdf files
 CHARACTER (LEN = 17) :: sce        ! Name of period or scenario (spin_up, hist, rcp)
 CHARACTER (LEN = 30) :: inDir      ! Absolute path to the input directory
!---------------------------------------------------------------------------------------!
! Climatic input variables at 0.5 degree x 0.5 degree resolution, units vary by source
!---------------------------------------------------------------------------------------!
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: prec ! Daily precipitation flux
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: snow ! Daily snowfall flux      
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: tmin ! Daily min:um surface temperature 
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: tmax ! Daily max:um surface temperature 
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: tair ! Daily mean surface temperature at 2m
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: pres ! Daily mean atmospheric pressure
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: humi ! Daily mean humidity  
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: rads ! Daily downwelling shortwave radiation  
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: radl ! Daily downwelling longwave radiation  
 REAL, DIMENSION (:,:,:), ALLOCATABLE :: wind ! Daily mean wind speed at 10m 
! REAL, DIMENSION (:,:,:), ALLOCATABLE :: uwnd ! Eastward near-surface wind  
! REAL, DIMENSION (:,:,:), ALLOCATABLE :: vwnd ! Northwar near-surface wind 
!---------------------------------------------------------------------------------------!
! Output climate fields of daily climate for the year
!---------------------------------------------------------------------------------------!
 REAL, DIMENSION (:), ALLOCATABLE :: prec_out ! Precipitation flux        (mm d-1)
 REAL, DIMENSION (:), ALLOCATABLE :: tmin_out ! Minimum air temperature   (degC)
 REAL, DIMENSION (:), ALLOCATABLE :: tmax_out ! Maximum air temperature   (degC)
 REAL, DIMENSION (:), ALLOCATABLE :: tair_out ! Mean air temperature      (degC)
 REAL, DIMENSION (:), ALLOCATABLE :: pres_out ! Mean atmospheric pressure (hPa)
 REAL, DIMENSION (:), ALLOCATABLE :: humi_out ! Mean absolute humidity    (hPa)
 REAL, DIMENSION (:), ALLOCATABLE :: rads_out ! Mean shortwave radiation  (W m-2) 
 REAL, DIMENSION (:), ALLOCATABLE :: radl_out ! Mean longwave radiation   (W m-2)
 REAL, DIMENSION (:), ALLOCATABLE :: snow_out ! Total snowfall            (kg m-2 s-1)
 REAL, DIMENSION (:), ALLOCATABLE :: wind_out ! Mean wind speed at 10m    (m s-1)
!---------------------------------------------------------------------------------------!
 INTEGER (KIND = 2) :: prec_f         ! Precipiation unit conversion flag
 INTEGER (KIND = 2) :: temp_f         ! Temperature unit conversion flag
 INTEGER (KIND = 2) :: humi_f         ! Humidity unit conversion flag
 INTEGER (KIND = 2) :: pres_f         ! Pressure unit conversion flag
 INTEGER (KIND = 2) :: radi_f         ! Radiation unit conversion flag
 INTEGER (KIND = 2) :: wind_f         ! Wind unit conversion flag
 INTEGER (KIND = 2) :: snow_f         ! Snowfall unit conversion flag
 REAL :: missing                      ! Value of missing values in netcdf files
 LOGICAL :: leap                      ! leap answers: Does input data have leap years?
 INTEGER :: clm_l, sce_l, dir_l       ! Length of character string of clm, sce and inDir
 INTEGER :: kyr                       ! Loop variable for annual loop / current year
!---------------------------------------------------------------------------------------!
 CHARACTER (LEN = 100) :: netcdf_filename         ! File name to be opened and read from
 INTEGER :: var_num                               ! Number of input variables
 CHARACTER (LEN = 10), DIMENSION (:), ALLOCATABLE :: var_names  ! Names of input vars
 CHARACTER (LEN = 10) :: var_name                 ! Name of current input variable 
 INTEGER :: var_l                                 ! Length of current input variable
!---------------------------------------------------------------------------------------!
 INTEGER (KIND = 4) :: var_index, status
 INTEGER (KIND = 4), DIMENSION (:), ALLOCATABLE :: varID, ncID 
 INTEGER (KIND = 4), DIMENSION (3) :: numTimes, dimIDs
!---------------------------------------------------------------------------------------!
 INTEGER (KIND = 2), PARAMETER :: jm  = 360 ! Number of latitudinal grid cells
 INTEGER (KIND = 2), PARAMETER :: im  = 720 ! Number of longitudinal grid cells
 INTEGER :: j0                              ! Lat. grid cell number on 0.5x0.5 deg grid
 INTEGER :: i0                              ! Lon. grid cell number on 0.5x0.5 deg grid
 INTEGER :: jcell                           ! Latitudinal loop variable
 INTEGER :: icell                           ! Longitudinal loop variable
 INTEGER  :: ncells                         ! Number of cell in grid cell band
 REAL :: glat                               ! Grid cell latitude starting at lower  
 REAL :: glon                               ! Grid cell longitude starting at lower
!---------------------------------------------------------------------------------------!
 INTEGER :: ndays              ! Number of days per year
 INTEGER :: cdays              ! Cumulative number of days to last day of current year
 INTEGER :: sday               ! Cumulative number of days to first day of current year
 INTEGER :: counter            ! Counter of sub-grid cell per grid cell
!---------------------------------------------------------------------------------------!

!---------------------------------------------------------------------------------------!
! Determine the length of clm and sce
!---------------------------------------------------------------------------------------!
 clm_l = index (clm  , ' ') - 1
 sce_l = index (sce  , ' ') - 1
 dir_l = index (inDir, ' ') - 1

!---------------------------------------------------------------------------------------!
! Determine variable names and length depending on climate input files
!---------------------------------------------------------------------------------------!
 climate_input : if (clm (1:3) == 'WAT') then
   var_num = 10
   allocate (var_names (var_num))
   var_names = (/'Rainf ','Tmin  ','Tmax  ','Qmean ','SWdown' ,'Tair  ','PSurf ',       &
                 'Snowf ','LWdown','Wind  '/)
   missing = 1.0e20
   prec_f = 1       ! Convert (kg m-2 s-1) to (mm d-1)   
   temp_f = 1       ! Convert (degC) i     to (K)   
   humi_f = 1       ! Convert (kg kg-1)    to (hPa)   
   pres_f = 1       ! Convert (Pa)         to (hPa)   
   radi_f = 1       ! Convert (W m-2)      to (W m-2)  
   snow_f = 1       ! Convert (kg m-2 s-1) to (?)
   wind_f = 1       ! Convert (m s-1)      to (m s-1)  
   leap = .true.    ! The input data has leap days 
 else if (clm (1:3) == 'Had') then ! TTR Need to add the other GCMs and introduce flags
   var_num = 12
   allocate (var_names (var_num))
   var_names = (/'','','','','','','','','','','',''/)
 end if climate_input
 allocate (varID     (var_num))
 allocate (ncID      (var_num))
!---------------------------------------------------------------------------------------!
 
!---------------------------------------------------------------------------------------!
! Determine the number of days for all years and allocate output arrays 
!---------------------------------------------------------------------------------------!
 year_loop : do kyr = year, year + nyrs - 1
   if (leap .and. mod (kyr, 4) == 0) then
     ndays = 366
   else
     ndays = 365
   end if 
   cdays = cdays + ndays
 end do year_loop
   
 allocate (prec_out (cdays))
 allocate (tmin_out (cdays))
 allocate (tmax_out (cdays))
 allocate (tair_out (cdays))
 allocate (pres_out (cdays))
 allocate (humi_out (cdays))
 allocate (rads_out (cdays))
 allocate (radl_out (cdays))
 allocate (snow_out (cdays))
 allocate (wind_out (cdays))
 cdays = 0 ! Rest cumulative days to zero for use in next annual loop

 annual_loop : do kyr = year, year + nyrs - 1 
   write (yr (1:4), '(i4)') kyr
 !write (*, *) 'Read from "',inDir (1:dir_l),'/',clm (1:clm_l),'/',sce (1:sce_l),'" for the year ', yr (1:4),'.' 
 !write (*, *) 'For the year ', yr (1:4),'.' 

!---------------------------------------------------------------------------------------!
! Loop over variable names to open the climate netcdf files and extract annual fields
!---------------------------------------------------------------------------------------!
   variables : do var_index = 1, var_num
     var_name  = var_names (var_index)
     var_l = index (var_name,' ') - 1

     netcdf_filename = inDir (1:dir_l)//'/'//clm (1:clm_l)//'/'//sce (1:sce_l)//'/'//   &
                       var_name (1:var_l)//'_'//yr (1:4)//'.nc'

     !write (*, *) netcdf_filename, '* *',var_name (1:var_l)

     status = nf90_open (netcdf_filename, nf90_nowrite, ncid (var_index))
     if (status .ne. nf90_noerr) write(*,*) 'Error in open: ', nf90_strerror (status)

     !write (*, *) ncid (var_index), '*',var_name, '*', varID (var_index)
     status = nf90_inq_varid (ncid (var_index), var_name (1:var_l), varID (var_index))
     if (status .ne. nf90_noerr) write(*,*) 'Error in inq_varid: ', status,             &
                                             nf90_strerror (status)

     status = nf90_inquire_variable (ncid (var_index), varID (var_index),               &
              dimids = dimIDs)
     if (status .ne. nf90_noerr) write(*,*) 'Error in inquire_variable: ', status,      &
                                             nf90_strerror (status)

     status = nf90_inquire_dimension (ncid (var_index), dimIDs (1), len = numTimes (1))
     !write (*,*) 'lon', numTimes (1)
     if (status .ne. nf90_noerr) write(*,*) 'Error in inquire_dimension: ', status,     &
                                             nf90_strerror (status)

     status = nf90_inquire_dimension (ncid (var_index), dimIDs (2), len = numTimes (2))
     !write (*,*) 'lat', numTimes (2)
     if (status .ne. nf90_noerr) write(*,*) 'Error in inquire_dimension: ', status,     &
                                             nf90_strerror (status)

     status = nf90_inquire_dimension (ncid (var_index), dimIDs (3), len = numTimes (3))
     if (status .ne. nf90_noerr) write(*,*) 'Error in inquire_dimension: ', status,     &
                                             nf90_strerror (status)
     !write (*,*) 'times', numTimes (3)
     
     if (var_index == 1) then      ! Daily total precipitation
       allocate (prec (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), prec)
     else if (var_index == 2) then ! Daily minimum surface temperature
       allocate (tmin (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), tmin)
     else if (var_index == 3) then ! Daily maximum surface temperature
       allocate (tmax (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), tmax)
     else if (var_index == 4) then ! Daily mean humidity
       allocate (humi (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), humi)
     else if (var_index == 5) then ! Downward shortwave radiation
       allocate (rads (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), rads)
     else if (var_index == 6) then ! Daily near surface mean air temperature at 2m
       allocate (tair (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), tair)
     else if (var_index == 7) then ! Daily mean surface air pressure 
       allocate (pres (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), pres)
     else if (var_index == 8) then ! Daily total snowfall flux
       allocate (snow (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), snow)
     else if (var_index == 9) then ! Mean daily incident longwave radiation
       allocate (radl (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), radl)
     else if (var_index == 10) then ! Daily mean wind speed at 10m
       allocate (wind (numTimes (1), numTimes (2), numTimes (3)))
       status = nf90_get_var (ncid (var_index), varID (var_index), wind)
     end if
     if (status .ne. nf90_noerr) write(*,*) 'Error in nf90_get_var', status, &
                                             nf90_strerror (status)
     status = nf90_close (ncID (var_index))
     if (status .ne. nf90_noerr) write(*,*) 'Error in nf90_close', status, &
                                             nf90_strerror (status)
   end do variables
 
!---------------------------------------------------------------------------------------!
! Determine the number of days in the current year
!---------------------------------------------------------------------------------------!
   ndays = numTimes (3)  ! Number of days in current year
   sday  = cdays + 1     ! Cumulative number of days to first day of current year
   cdays = cdays + ndays ! Cumulative number of days to last day of current year

!---------------------------------------------------------------------------------------!
! Convert precipitation according to prec_f flag
!---------------------------------------------------------------------------------------!
   if (prec_f == 1) then ! From (kg m-2 s-1) to (mm d-1)
     where (prec /= missing) prec = prec * 86400
   end if

!---------------------------------------------------------------------------------------!
! Convert temperatures according to temp_f flag
!---------------------------------------------------------------------------------------!
   if (temp_f == 1) then! From (degC) to (K)
     where (tmin /= missing) tmin = tmin - 273.15
     where (tmax /= missing) tmax = tmax - 273.15
     where (tair /= missing) tair = tair - 273.15
   end if

!---------------------------------------------------------------------------------------!
! Convert pressure according to pres_f flag
!---------------------------------------------------------------------------------------!
   if (pres_f == 1) then ! From (Pa) to (hPa) 
    where (pres /= missing) pres = pres / 100.0  
   end if   

!---------------------------------------------------------------------------------------!
! Convert humidity according to humi_f flag
!---------------------------------------------------------------------------------------!
   if (humi_f == 1) then ! From (kg kg-1) to (hPa) 
     where (humi /= missing) humi = humi / (1 - humi) ! Mixing ratio (kg kg-1)
     where (humi /= missing) humi = humi / ((18.01528 / 28.9645) + humi) * pres ! Absolute humidity (hPa)
   end if
! Partial pressure from mixing ratio given the molar masses of water and dry air (18
! and 29 g mol-1 respectively) at atmospheric pressure (pres)

!---------------------------------------------------------------------------------------!
! Convert radiation according to radi_f flag
!---------------------------------------------------------------------------------------!
   if (radi_f == 2) then ! From (W m-2) to (MJ d-1) 
     where (rads /= missing) rads = rads * 1.0e6 / 86400
     where (radl /= missing) radl = radl * 1.0e6 / 86400
   end if

!---------------------------------------------------------------------------------------!
! Convert snowfall according to snow_f flag
!---------------------------------------------------------------------------------------!
   if (snow_f == 2) then ! From (kg m-2 s-1) to (?) 
     where (snow /= missing) snow = snow 
   end if

!---------------------------------------------------------------------------------------!
! Convert wind speed according to wind_f flag
!---------------------------------------------------------------------------------------!
   if (wind_f == 2) then ! From (m s-1) to (?) 
     where (wind /= missing) wind = wind * 1.0e6 / 86400
   end if
   !write (*,*) 'Converted all units.' 

   resolution : if (reso == 0.5) then
     j0 = int ((lat +  90.25) / reso) ! Latitudinal grid cell number
     i0 = int ((lon + 180.25) / reso) ! Longitudinal grid cell number
     prec_out (sday:cdays) = prec (i0, j0, :)
     tmin_out (sday:cdays) = tmin (i0, j0, :)
     tmax_out (sday : cdays) = tmax (i0, j0, :)
     tair_out (sday : cdays) = tair (i0, j0, :)
     pres_out (sday : cdays) = pres (i0, j0, :)
     humi_out (sday : cdays) = humi (i0, j0, :)
     rads_out (sday : cdays) = rads (i0, j0, :)
     radl_out (sday : cdays) = radl (i0, j0, :)
     snow_out (sday : cdays) = snow (i0, j0, :)
     wind_out (sday : cdays) = wind (i0, j0, :)
   else ! Need to re-gridd the climate data for the site
     if (mod (reso, 0.5) /= 0) stop "Error: the input resolution is not a multiple of  &
                                    &the intrinsic resolution." 
     ncells = int (reso / 0.5) ! Number of grid cell in latitudinal or longitudinal band 
     glat = lat - (reso / 2.0) ! Set initial latitude to lower boundary of grid cell
     glon = lon - (reso / 2.0) ! Set initial longitude to lower boundary of grid cell
     counter = 0               ! Set sub-grid cell counter to 0

!---------------------------------------------------------------------------------------!
! Loop over grid cells around the mid-lat and mid-lon given the resolution 
!---------------------------------------------------------------------------------------!
     lat_grid_cells : do jcell = 1, ncells
       lon_grid_cells : do icell = 1, ncells
         glat = glat + 0.5 * jcell - 1
         glon = glon + 0.5 * icell - 1
         j0 = int ((glat + (90.0  + reso / 2.0)) / 0.5)
         i0 = int ((glon + (180.0 + reso / 2.0)) / 0.5)

!---------------------------------------------------------------------------------------!
! Sum the totals for all existing sub-grid cells
!---------------------------------------------------------------------------------------!
         if (prec (i0, j0, 1) /=  missing) then
           prec_out (sday : cdays) = prec_out + prec (i0, j0, :) 
           tmin_out (sday : cdays) = tmin_out + tmin (i0, j0, :) 
           tmax_out (sday : cdays) = tmax_out + tmax (i0, j0, :) 
           tair_out (sday : cdays) = tair_out + tair (i0, j0, :) 
           pres_out (sday : cdays) = pres_out + pres (i0, j0, :) 
           humi_out (sday : cdays) = humi_out + humi (i0, j0, :) 
           rads_out (sday : cdays) = rads_out + rads (i0, j0, :) 
           radl_out (sday : cdays) = radl_out + radl (i0, j0, :) 
           snow_out (sday : cdays) = snow_out + snow (i0, j0, :) 
           wind_out (sday : cdays) = wind_out + wind (i0, j0, :) 
           counter = counter + 1
         end if
       end do lon_grid_cells
     end do lat_grid_cells
 
!---------------------------------------------------------------------------------------!
! Average the sums of all grid cells, by the number of sub-grid cells 
!---------------------------------------------------------------------------------------!
     tmin_out (sday : cdays) = tmin_out (sday : cdays) / counter
     tmax_out (sday : cdays) = tmax_out (sday : cdays) / counter
     tair_out (sday : cdays) = tair_out (sday : cdays) / counter
     pres_out (sday : cdays) = pres_out (sday : cdays) / counter
     humi_out (sday : cdays) = humi_out (sday : cdays) / counter
     rads_out (sday : cdays) = rads_out (sday : cdays) / counter
     radl_out (sday : cdays) = radl_out (sday : cdays) / counter
     snow_out (sday : cdays) = snow_out (sday : cdays) / counter
     wind_out (sday : cdays) = wind_out (sday : cdays) / counter
   end if resolution
!   write (*, 8000) j0, i0, lat, lon, prec_out (180), tmin_out (180), tmax_out (180),  &
!                   tair_out (180), pres_out (180), humi_out (180), rads_out (180),    &
!                   radl_out (180), snow_out (180), wind_out (180)
!8000 format (2i5, 2f7.2, 10f12.5)
 
!---------------------------------------------------------------------------------------!
! Deallocate the annual climate arrays, so they can be used for next year 
!---------------------------------------------------------------------------------------!
   deallocate (prec)
   deallocate (tmin)
   deallocate (tmax)
   deallocate (tair)
   deallocate (pres)
   deallocate (humi)
   deallocate (rads)
   deallocate (radl)
   deallocate (snow)
   deallocate (wind)
 end do annual_loop

 RETURN
!--------------------------------------------------------------------------------------!
END SUBROUTINE fetch_clm
!======================================================================================!
