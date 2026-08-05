module med_ccpp_driver

  use cmeps_ccpp_cap,       only: ccpp_register
  use cmeps_ccpp_cap,       only: ccpp_init
  use cmeps_ccpp_cap,       only: ccpp_physics_init
  use cmeps_ccpp_cap,       only: ccpp_physics_timestep_init
  use cmeps_ccpp_cap,       only: ccpp_physics_run
  use cmeps_ccpp_cap,       only: ccpp_physics_final
  use cmeps_ccpp_cap,       only: ccpp_final
  use MED_data,             only: physics
  use iso_fortran_env,      only: error_unit
  implicit none

  !--------------------------------------------------------!
  ! CCPP control (mandatory) data.
  !--------------------------------------------------------!
  integer :: mythread
  integer :: nthreads
  integer :: nphys_threads
  integer :: lb
  integer :: ub
  integer :: errflg
  character(len=512) :: errmsg
  character(len=256) :: ccpp_suite='undefined'
  character(len=256) :: group_name='undefined'

  private ! default private

  public :: med_ccpp_driver_init
  public :: med_ccpp_driver_run
  public :: med_ccpp_driver_finalize

!===============================================================================
contains
!===============================================================================

  subroutine med_ccpp_driver_init(aoflux_ccpp_suite)
    implicit none
    character(len=*), intent(in) :: aoflux_ccpp_suite

    ccpp_suite = aoflux_ccpp_suite
 
    ! CCPP register
    call ccpp_register(ccpp_suite=trim(aoflux_ccpp_suite), errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
       write(error_unit,'(a,i0,a)') 'An error occurred in ccpp_register: ' // trim(errmsg) // '. Exiting...'
       return
    end if

    ! Initialize CCPP framework
    call ccpp_init(ccpp_suite=trim(aoflux_ccpp_suite), errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
       write(error_unit,'(a,i0,a)') 'An error occurred in ccpp_init: ' // trim(errmsg) // '. Exiting...'
       return
    end if

    ! initialize CCPP physics (run all _init routines)
    call ccpp_physics_init(ccpp_suite=trim(aoflux_ccpp_suite), group_name='all', &
            errmsg=errmsg, errflg=errflg, lb=1, ub=physics%init%im,       &
            mythread=1, nthreads=1, nphys_threads=1)
    if (errflg/=0) then
       write(error_unit,'(a,i0,a)') 'An error occurred in ccpp_physics_init: ' // trim(errmsg) // '. Exiting...'
       return
    end if

    ! timestep_init (Doesn't do anything. Just sets ccpp_group_state)
    call ccpp_physics_timestep_init(ccpp_suite=trim(aoflux_ccpp_suite), group_name='all', &
            errmsg=errmsg, errflg=errflg, lb=1, ub=physics%init%im,       &
            mythread=1, nthreads=1, nphys_threads=1)
    if (errflg/=0) then
       write(error_unit,'(a,i0,a)') 'An error occurred in ccpp_physics_timestep_init: ' // trim(errmsg) // '. Exiting...'
       return
    end if
  end subroutine med_ccpp_driver_init

  !=============================================================================
  subroutine med_ccpp_driver_run(aoflux_ccpp_suite)
    implicit none
    character(len=*), intent(in) :: aoflux_ccpp_suite
    
    ! run CCPP physics (run all _run routines)
    call ccpp_physics_run(ccpp_suite=trim(aoflux_ccpp_suite), group_name='all', &
         errmsg=errmsg, errflg=errflg, lb=1, ub=physics%init%im,         &
         mythread=1, nthreads=1, nphys_threads=1)
    if (errflg/=0) then
       write(error_unit,'(a,i0,a)') 'An error occurred in ccpp_physics_run: ' // trim(errmsg) // '. Exiting...'
       return
    end if

  end subroutine med_ccpp_driver_run

  !=============================================================================
  subroutine med_ccpp_driver_finalize(aoflux_ccpp_suite)
    implicit none
    character(len=*), intent(in) :: aoflux_ccpp_suite

    ! finalize CCPP physics (run all _finalize routines)
    call ccpp_physics_final(ccpp_suite=trim(aoflux_ccpp_suite), group_name='all', &
         errmsg=errmsg, errflg=errflg, lb=1, ub=physics%init%im,           &
         mythread=1, nthreads=1, nphys_threads=1)
    if (errflg/=0) then
       write(error_unit,'(a,i0,a)') 'An error occurred in ccpp_physics_final: ' // trim(errmsg) // '. Exiting...'
       return
    end if

  end subroutine med_ccpp_driver_finalize

end module med_ccpp_driver
