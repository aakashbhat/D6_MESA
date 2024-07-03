! ***********************************************************************
!
!   Copyright (C) 2010-2019  Bill Paxton & The MESA Team
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful, 
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************
 
      module run_star_extras
      use auto_diff
      use star_lib
      use star_def
      use const_def
      use math_lib
      
      implicit none
      
      ! these routines are called by the standard run_star check_model
      contains
      
            subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         ! this is the place to set any procedure pointers you want to change
         ! e.g., other_wind, other_mixing, other_energy  (see star_data.inc)


         ! the extras functions in this file will not be called
         ! unless you set their function pointers as done below.
         ! otherwise we use a null_ version which does nothing (except warn).

         s% extras_startup => extras_startup
         s% extras_start_step => extras_start_step
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns  

         s% how_many_extra_history_header_items => how_many_extra_history_header_items
         s% data_for_extra_history_header_items => data_for_extra_history_header_items
         s% how_many_extra_profile_header_items => how_many_extra_profile_header_items
         s% data_for_extra_profile_header_items => data_for_extra_profile_header_items
         s% other_energy => entropy_inject
      end subroutine extras_controls
      
      !subroutine entropy_inject()
      
      subroutine extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine extras_startup
      

      integer function extras_start_step(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_start_step = 0
        if(s% time < s% x_ctrl(1) ) then
                s% MLT_option = 'none'
                s%energy_eqn_option='eps_grav'
                s%max_abar_for_burning=-1
                s% eos_rq% use_Skye=.false.
                s% eos_rq%use_PC=.false.
                !use_FreeEOS=.false.
                s% eos_rq%use_CMS=.false.
                s% eos_rq%use_OPAL_SCVH=.false.
                s% eos_rq%logT_max_freeEOS_HI= 8.2
                s% eos_rq%logT_max_freeEOS_lo=8.2
                s% eos_rq%logRho_max_FreeEOS_hi = 3.0d0
                s% eos_rq%logRho_max_FreeEOS_lo = 2.0d0
                s%super_eddington_scaling_factor=0
                s%super_eddington_wind_Ledd_factor=1
                s%super_eddington_wind_max_boost = 0
                !s%non_nuc_neu_factor=0
                s%max_timestep=1d-4


        else
                s% MLT_option = 'TDC'
                s%max_abar_for_burning=299
                s% eos_rq% use_Skye = .true.
                s%eos_rq%logT_min_for_any_Skye = 8.0
                s%eos_rq%logT_min_for_all_Skye = 8.2
                s%eos_rq%logrho_min_for_any_Skye =2.7
                s%eos_rq%logrho_min_for_all_Skye =3.0
                s%eos_rq%use_simple_Skye_blends=.true.
                s%super_eddington_wind_Ledd_factor=1.0
                s%super_eddington_scaling_factor=1
                s%super_eddington_wind_max_boost = 1
                s%log_tau_function_weight = 2
                s%max_surface_cell_dq = 1d-17
                s%min_dq=1d-18

                s%max_timestep= 0
                !s%diffusion_min_Z_for_radaccel=26
                !s%diffusion_max_Z_for_radaccel=30
                !s%do_element_diffusion=.true.
                !s%diffusion_max_T_for_radaccel=1e7
                !s%diffusion_screening_for_radaccel = .true.
                !s%diffusion_use_full_net=.true.
                !s%energy_eqn_option='dedt'
        end if
      end function extras_start_step

!      subroutine default_other_energy(id, ierr)
!         use const_def, only: Rsun
!         integer, intent(in) :: id
!         integer, intent(out) :: ierr
!         type (star_info), pointer :: s
!         integer :: k
!         ierr = 0
!         call star_ptr(id, s, ierr)
!         if (ierr /= 0) return
         !s% extra_heat(:) = s% extra_power_source
         ! note that extra_heat is type(auto_diff_real_star_order1) so includes partials.
!      end subroutine default_other_energy
        
      subroutine entropy_inject(id,ierr)
         use auto_diff
         !use const_def, only: Rsun
         !integer, intent(in) :: id
         !integer, intent(out) :: ierr
         !type (star_info), pointer :: s
         !real(dp) :: values
         !integer :: i,j,ios
         !ierr = 0
         !call star_ptr(id, s, ierr)
        ! if (ierr /= 0) return
         ! Read file
         integer, parameter :: numrows = 970
        integer, intent(in) :: id
        integer, intent(out) :: ierr
        real(dp), dimension(numrows) :: qm, heat
        real(dp) :: dS, heat2, alfa, beta
        type (star_info), pointer :: s
        integer :: k, i, j
        ierr = 0
        call star_ptr(id, s, ierr)
        if (ierr /= 0) return
        open(unit=27,file="entropy_normal_2.0_HELM.dat")

        do j=1,numrows
           read(27,*) qm(j), heat(j)
        end do
        close(27)
        !s% x_ctrl(1)=100
        ! mass is mass in grams
        ! deltaS needs to be multiplied by c_V to get into the right units (erg/g/K)

        ! inject over fixed amount of time specified in the inlist
        if(s% time <= s% x_ctrl(1)) then ! time = age in seconds
           ! read in file to find nearest heating value and add the heat.
	   
           do k=1,s% nz
              heat2 = 0d0
	      dS = 0d0
              do j=2,numrows ! starts at outside of star and moves in
                 if(s% q(k) <= qm(j)) then ! .and. s% m(k) < mass(j-1)) then
                    ! basic linear interpolation
                    if (s%q(k)<=qm(j-1)) then
                            alfa=1d0
                            beta=0d0
                    else
                            alfa =(s%q(k)-qm(j-1))/(qm(j) - qm(j-1))
                            beta=1d0-alfa
                    end if
                    !if (k>1360) then
                    !    print *,k,s%q(k),qm(j),qm(j-1),s%rho(k)
                    !
                    !end if
                    !if (s%q(k)>0.999) then
                    !    dS=0
                    !else
                    dS = heat(j-1)*alfa + heat(j)*beta
                    !end if
                    !dS = 1.5d0*boltzm*dS/(s% mu(k)*mp) ! Assumes ideal gas c_V
                    ! dS = s% Cv(k)*dS ! uses MESA Cv, less consistent with Chris's runs
                    exit
                 end if
              end do
              ! Inject at a rate such that total dS is achieved after x_ctrl seconds
              heat2 =0.5*s%T(k)*dS/(s% x_ctrl(1))
              s% extra_heat(k) = heat2
           end do
        else
            do k=1,s% nz
                s% extra_heat(k) = 0
            end do
        end if
        print *, "time:", s% time
         !s% duration_for_inject_extra_ergs_sec = 1d2
         !if (s%star_age<3d-8) then
          !      open(file='heat_replaced.dat',status='old',action='read',unit=1)
          !      do j=1, 970
          !              !do i=1, 1
          !              !        read(*,*) values
          !              !end do
          !              read(1,*,iostat=ios) values
          !              if(ios/=0 ) exit
          !              s%extra_heat(970-j)=s%T(970-j)*values/100
          !      end do
          !      close(1)
          !end if
       end subroutine entropy_inject

      ! returns either keep_going, retry, or terminate.
      integer function extras_check_model(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         integer :: i,j
         integer :: ios
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going         
         !if (.false. .and. s% star_mass_h1 < 0.35d0) then
            ! stop when star hydrogen mass drops to specified level
         !   extras_check_model = terminate
         !   write(*, *) 'have reached desired hydrogen mass'
         !   return
         !end if
        if (s%star_age<1d3) then
            s%Pextra_factor=1.5
            !if(s%star_age<4d-2) then
!            s%use_op_mono_alt_get_kap=.false.
            !else
            !    if (abs(s%star_mdot)>1d-8) then

            !            s%high_logT_op_mono_full_off = 7.8d0 !6.4d0
            !            s%high_logT_op_mono_full_on = 7.5d0 !6.0d0
            !            s%low_logT_op_mono_full_on = 6.8d0
            !            s%low_logT_op_mono_full_off = 6.5d0
            !    else
            !            s%high_logT_op_mono_full_off = 7.8d0 !6.4d0
            !            s%high_logT_op_mono_full_on = 7.5d0 !6.0d0
            !            s%low_logT_op_mono_full_on = 5.2d0
            !            s%low_logT_op_mono_full_off = 5.0d0
            !    end if
            !end if
            !use_op_mono_alt_get_kap = .true.
         else if ((s%star_age>1d3) .and. (s%star_age<1d5)) then
            s%Pextra_factor=1.2
            s%diffusion_min_Z_for_radaccel=10
            s%diffusion_max_Z_for_radaccel=28
            s%do_element_diffusion=.true.
            s%diffusion_max_T_for_radaccel=1e8
            s%diffusion_screening_for_radaccel = .true.
            s%diffusion_use_full_net=.false.
     !       s%diffusion_num_classes=6
            s%diffusion_v_max=1d-2
            s%diffusion_tol_correction_max = 3d-1
            s%diffusion_tol_correction_norm = 3d-3
            s%diffusion_min_X_hard_limit = -5d-3
            s%diffusion_dt_limit = 1 ! no element diffusion if dt < this limit (in seconds)


            s%diffusion_min_dq_at_surface = 5d-14
            ! treat at least this much at surface as a single cell for purposes of diffusion

            s%diffusion_min_T_at_surface = 1d4 ! this should be large enough to ensure hydrogen ionization
            ! treat cells cells at surface with T < this as a single cell for purposes of diffusion

            s%diffusion_min_dq_ratio_at_surface = 4

            s% diffusion_X_total_atol = 1d-9
            s%diffusion_X_total_rtol = 1d-6

            s%diffusion_upwind_abs_v_limit = 1d-6
            ! switch to upwind for i at face k if abs(v(i,k)) > this limit

            s%diffusion_T_full_on = 1d3
            s%diffusion_T_full_off = 1d3

            s%diffusion_calculates_ionization = .true.
            s%diffusion_nsmooth_typical_charge = 10




            !s%use_op_mono_alt_get_kap = .false.

            !s%op_mono_min_X_to_include = 1d-4 ! skip iso if mass fraction < this

            s%diffusion_num_classes = 8 ! number of classes of species for diffusion calculations
            s%diffusion_class_representative(1) = 'h1'
            s%diffusion_class_representative(2) = 'he4'
            s%diffusion_class_representative(3) = 'c12'
            s%diffusion_class_representative(7) = 'si28'
            s%diffusion_class_representative(4) = 'o16'
            s%diffusion_class_representative(5) = 'ne20'
            s%diffusion_class_representative(6) = 'mg24'
            s%diffusion_class_representative(8) = 'fe56'
            s%energy_eqn_option='dedt'
            
         else if (s%star_age>1d5) then
            s%Pextra_factor=1.2
            !s%high_logT_op_mono_full_off =6.3d0
            !s%high_logT_op_mono_full_on = 5.8d0
            !s%low_logT_op_mono_full_on=5.2d0
            !s%low_logT_op_mono_full_off=5.0d0
            !s%use_op_mono_alt_get_kap = .true.
            !s%op_mono_min_X_to_include = 1d-6 ! skip iso if mass fraction < this

         end if
            
    end function extras_check_model


         


      integer function how_many_extra_history_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 0
      end function how_many_extra_history_columns
      
      
      subroutine data_for_extra_history_columns(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         ! note: do NOT add the extras names to history_columns.list
         ! the history_columns.list is only for the built-in history column options.
         ! it must not include the new column names you are adding here.
         

      end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(id, n, nz, names, vals, ierr)
         integer, intent(in) :: id, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         ! note: do NOT add the extra names to profile_columns.list
         ! the profile_columns.list is only for the built-in profile column options.
         ! it must not include the new column names you are adding here.

         ! here is an example for adding a profile column
         !if (n /= 1) stop 'data_for_extra_profile_columns'
         !names(1) = 'beta'
         !do k = 1, nz
         !   vals(k,1) = s% Pgas(k)/s% P(k)
         !end do
         
      end subroutine data_for_extra_profile_columns


      integer function how_many_extra_history_header_items(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_header_items = 0
      end function how_many_extra_history_header_items


      subroutine data_for_extra_history_header_items(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         type(star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         call star_ptr(id,s,ierr)
         if(ierr/=0) return

         ! here is an example for adding an extra history header item
         ! also set how_many_extra_history_header_items
         ! names(1) = 'mixing_length_alpha'
         ! vals(1) = s% mixing_length_alpha

      end subroutine data_for_extra_history_header_items


      integer function how_many_extra_profile_header_items(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_header_items = 0
      end function how_many_extra_profile_header_items


      subroutine data_for_extra_profile_header_items(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(n)
         type(star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         call star_ptr(id,s,ierr)
         if(ierr/=0) return

         ! here is an example for adding an extra profile header item
         ! also set how_many_extra_profile_header_items
         ! names(1) = 'mixing_length_alpha'
         ! vals(1) = s% mixing_length_alpha

      end subroutine data_for_extra_profile_header_items


      ! returns either keep_going or terminate.
      ! note: cannot request retry; extras_check_model can do that.
      integer function extras_finish_step(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going

         ! to save a profile, 
            ! s% need_to_save_profiles_now = .true.
         ! to update the star log,
            ! s% need_to_update_history_now = .true.

         ! see extras_check_model for information about custom termination codes
         ! by default, indicate where (in the code) MESA terminated
         if (extras_finish_step == terminate) s% termination_code = t_extras_finish_step
      end function extras_finish_step
      
      
      subroutine extras_after_evolve(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine extras_after_evolve

      
      
            

      end module run_star_extras
      
