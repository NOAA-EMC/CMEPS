module flux_atmocn_driver_mod

  use ESMF,          only : ESMF_GridComp
  use ufs_kind_mod,  only : R8=>SHR_KIND_R8, IN=>SHR_KIND_IN ! shared kinds
  use ufs_const_mod, only : SHR_CONST_SPVAL
  use flux_atmocn_bulk_mod, only : flux_atmocn_bulk
  !use flux_atmocn_ccpp_mod, only : flux_atmocn_ccpp

  implicit none
  public

  integer, private, parameter :: ocn_flux_scheme_bulk = 0
  integer, private, parameter :: ocn_flux_scheme_ccpp = 1

contains
  ! subroutine flux_adjust_constants( flux_convergence_tolerance, &
  !      flux_convergence_max_iteration, coldair_outbreak_mod)

  !   ! Adjust local constants.  Used to support simple models.
  !   real(r8)    , optional, intent(in) :: flux_convergence_tolerance
  !   integer(in) , optional, intent(in) :: flux_convergence_max_iteration
  !   logical     , optional, intent(in) :: coldair_outbreak_mod
  !   !----------------------------------------------------------------------------

  !   if (present(flux_convergence_tolerance)) flux_con_tol = flux_convergence_tolerance
  !   if (present(flux_convergence_max_iteration)) flux_con_max_iter = flux_convergence_max_iteration
  !   if (present(coldair_outbreak_mod)) use_coldair_outbreak_mod = coldair_outbreak_mod

  ! end subroutine flux_adjust_constants

  subroutine flux_atmocn_driver(ocn_surface_flux_scheme,     &
       gcomp, garea, maintask, logunit, nMax, mask,          &
       zbot, ubot, vbot, qbot, rbot, tbot, thbot, pbot,      &
       ts, us, vs,                                           &
       usfc, vsfc, psfc, lwdn,                               &
       sen, lat, lwup, taux, tauy, evap, tref, qref, duu10n, &
       missval)
!       missval,                                              &
!       ustar_sv, re_sv, ssq_sv )

    implicit none

    !--- input arguments --------------------------------
    integer,             intent(in) :: ocn_surface_flux_scheme  ! flux scheme
    type(ESMF_GridComp), intent(in) :: gcomp       ! gridded component
    real(R8),            intent(in) :: garea(nMax) ! grid area (m^2)
    logical,             intent(in) :: maintask    ! main task

    integer,  intent(in) :: logunit     !
    integer,  intent(in) :: nMax        ! data vector length
    integer,  intent(in) :: mask (nMax) ! ocn domain mask 0 <=> out of domain

    real(R8), intent(in) :: zbot (nMax) ! atm level height (m)
    real(R8), intent(in) :: ubot (nMax) ! atm u wind (bottom) (m/s)
    real(R8), intent(in) :: vbot (nMax) ! atm v wind (bottom) (m/s)
    real(R8), intent(in) :: qbot (nMax) ! atm specific humidity (bottom) (kg/kg)
    real(R8), intent(in) :: rbot (nMax) ! atm air density (kg/m^3)
    real(R8), intent(in) :: tbot (nMax) ! atm T (bottom) (K)
    real(R8), intent(in) :: ts   (nMax) ! ocn temperature (K)
    real(R8), intent(in) :: us   (nMax) ! ocn u-velocity (m/s)
    real(R8), intent(in) :: vs   (nMax) ! ocn v-velocity (m/s)
    real(R8), intent(in) :: thbot(nMax) ! atm potential T (K)
    real(R8), intent(in) :: usfc (nMax) ! atm u wind (surface) (m/s)
    real(R8), intent(in) :: vsfc (nMax) ! atm v wind (surface) (m/s)
    real(R8), intent(in) :: psfc (nMax) ! atm P (surface) (Pa)
    real(R8), intent(in) :: pbot (nMax) ! atm P (bottom) (Pa)
    real(R8), intent(in) :: lwdn (nMax) ! atm lw downward (W/m^2)

    !--- output arguments -------------------------------
    real(R8), intent(out) :: sen   (nMax) ! heat flux: sensible (W/m^2)
    real(R8), intent(out) :: lat   (nMax) ! heat flux: latent (W/m^2)
    real(R8), intent(out) :: lwup  (nMax) ! heat flux: lw upward (W/m^2)
    real(R8), intent(out) :: taux  (nMax) ! surface stress, zonal (N)
    real(R8), intent(out) :: tauy  (nMax) ! surface stress, maridional (N)
    real(R8), intent(out) :: evap  (nMax) ! heat flux ((kg/s)/m^2)
    real(R8), intent(out) :: tref  (nMax) ! diagnostic : 2m ref height T (K)
    real(R8), intent(out) :: qref  (nMax) ! diagnostic:  2m ref humidity (kg/kg)
    real(R8), intent(out) :: duu10n(nMax) ! diagnostic: 10m wind speed squared (m/s)^2

    real(R8), intent(in), optional :: missval ! masked value

    !real(R8), intent(out) :: ustar_sv(nMax)  ! diagnostic: ustar
    !real(R8), intent(out) :: re_sv   (nMax)  ! diagnostic: sqrt of exchange coeff (water)
    !real(R8), intent(out) :: ssq_sv  (nMax)  ! diagnostic: sea surface humidity (kg/kg)

    ! local
    real(R8) :: spval

    !--------------------------------------------------------------------------------

    if (present(missval)) then
       spval = missval
    else
       spval = shr_const_spval
    endif
    ! If caller provided optional diagnostics but chosen path does not set them:
    !if (present(ustar_sv)) ustar_sv = spval
    !if (present(re_sv))    re_sv    = spval
    !if (present(ssq_sv))   ssq_sv   = spval

    if (ocn_surface_flux_scheme == ocn_flux_scheme_bulk) then
        call flux_atmocn_bulk(logunit, nMax, mask,                       &
           zbot, ubot, vbot, qbot, rbot, tbot, ts, us, vs, thbot, spval, &
           sen, lat, lwup, taux, tauy, evap, tref, qref, duu10n)

    else if  (ocn_surface_flux_scheme == ocn_flux_scheme_ccpp) then
       ! call flux_atmocn_ccpp( logunit=logunit, &
       !      nMax=aoflux_in%lsize,              &
       !      mask=aoflux_in%mask,               &
       !      zbot=aoflux_in%zbot,               &
       !      ubot=aoflux_in%ubot,               &
       !      vbot=aoflux_in%vbot,               &
       !      qbot=aoflux_in%shum,               &
       !      rbot=aoflux_in%dens,               &
       !      tbot=aoflux_in%tbot,               &
       !      ts=aoflux_in%tocn,                 &
       !      gcomp=gcomp,                       &
       !      maintask=maintask,                 &
       !      garea=aoflux_in%garea,             &
       !      usfc=aoflux_in%usfc,               &
       !      vsfc=aoflux_in%vsfc,               &
       !      psfc=aoflux_in%psfc,               &
       !      pbot=aoflux_in%pbot,               &
       !      lwdn=aoflux_in%lwdn,               &
       !      ! optional in
       !      missval=0.0_r8,                    &
       !      ! out
       !      sen=aoflux_out%sen,                &
       !      lat=aoflux_out%lat,                &
       !      lwup=aoflux_out%lwup,              &
       !      taux=aoflux_out%taux,              &
       !      tauy=aoflux_out%tauy,              &
       !      evap=aoflux_out%evap,              &
       !      tref=aoflux_out%tref,              &
       !      qref=aoflux_out%qref,              &
       !      duu10n=aoflux_out%duu10n,          &
       !      ustar_sv=aoflux_out%ustar,         &
       !      re_sv=aoflux_out%re,               &
       !      ssq_sv=aoflux_out%ssq)
    end if

  end subroutine flux_atmocn_driver
end module flux_atmocn_driver_mod
