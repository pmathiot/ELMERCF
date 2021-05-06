!/*****************************************************************************/
! *
! *  Elmer/Ice, a glaciological add-on to Elmer
! *  http://elmerice.elmerfem.org
! *
! * 
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! * 
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the 
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/
! ******************************************************************************
! *
! *  Authors: f. Gillet-Chaulet (IGE, Grenoble,France)
! *  Email:   
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: Feb. 2021
! * 
! *  - Read the netcdf produced by Elmer2NEMO_GridGen
! *  - Locate the points in the given element (no remumbering in parallel!!)
! *  - Interpolate requested varaibles using the FE basis function
! *  - Save results on the NEMO Grid
!! *****************************************************************************
       SUBROUTINE Temporal_Mean_Step2_init0(Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
       USE DefUtils
!------------------------------------------------------------------------------
       IMPLICIT NONE
!------------------------------------------------------------------------------
       TYPE(Solver_t), TARGET :: Solver
       TYPE(Model_t) :: Model
       REAL(KIND=dp) :: dt
       LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
      ! Solver reading
      CHARACTER(LEN=MAX_NAME_LEN) :: VarName,str,cvar
      REAL(dp) :: rtime
      INTEGER :: nvar
      LOGICAL :: GotIt
      TYPE(ValueList_t), POINTER :: SolverParams

      ! Get input and output netcdf files
      SolverParams => GetSolverParams()

      ! Initialisation
      ! define cumulated variable for each variable in sif
      nvar = 0
      DO WHILE( .TRUE. )
         nvar = nvar + 1
         str = ComponentName( 'Time Mean Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )
         ! check also that we can get it
         IF(.NOT. GotIt) EXIT
         
         ! add exported variable
         WRITE(cvar, '(a,i1)') 'Exported Variable ',nvar
         CALL ListAddNewString( Solver % Values, TRIM(cvar), '-dofs 1 -elem Time_Mean_'//TRIM(VarName))
         ! 
      END DO

      END SUBROUTINE Temporal_Mean_Step2_init0

!------------------------------------------------------------------------------
      SUBROUTINE Temporal_Mean_Step2( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
      USE DefUtils
!------------------------------------------------------------------------------
      IMPLICIT NONE
!------------------------------------------------------------------------------
      TYPE(Solver_t) :: Solver
      TYPE(Model_t) :: Model
      REAL(KIND=dp) :: dt
      LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
      TYPE(ValueList_t), POINTER :: SolverParams
      TYPE(Variable_t) , POINTER :: Time_Int_Variable
      TYPE(Variable_t) , POINTER :: Mean_Variable
      TYPE(Variable_t) , POINTER :: ptime_end, ptime_start

      ! Time mean variable
      REAL(kind=dp) :: rtime_start
      REAL(kind=dp) :: rtime_end
      REAL(kind=dp) :: rtotal_time

      ! Solver reading
      CHARACTER(LEN=MAX_NAME_LEN) :: SolverName='Time_mean_step2'
      CHARACTER(LEN=MAX_NAME_LEN) :: VarName,str
      INTEGER :: nvar
      LOGICAL, SAVE :: Firsttime = .TRUE.
      LOGICAL :: GotIt

      REAL(KIND=dp) :: val

      ! Get variable list
      SolverParams => GetSolverParams()

      ! get total time
      rtime_start = GetConstReal( Model % Simulation,'Time_Start')
      rtime_end   = GetTime()
      rtotal_time = rtime_end - rtime_start
      IF (rtotal_time == 0._dp) CALL FATAL(SolverName,' ERROR: averaged period is 0.0, cannot compute the time mean')

      ! compute the time mean
      nvar = 0
      DO WHILE( .TRUE. )
         nvar = nvar + 1
         str = ComponentName( 'Time Mean Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )
         ! check also that we can get it
         IF(.NOT. GotIt) EXIT

         ! retreive the time integration (defined in step 1)
         Time_Int_Variable => VariableGet(Model % Mesh % Variables,'Time_Int_'//TRIM(VarName),UnFoundFatal=.TRUE.)

         ! compute time mean
         Mean_Variable => VariableGet(Model % Mesh % Variables,'Time_Mean_'//TRIM(VarName),UnFoundFatal=.TRUE.)
         Mean_Variable % values (:) = Time_Int_Variable % values (:) / rtotal_time

         ! reset integrated variable for next vtu
         Time_Int_Variable % values (:) = 0.0_dp

      END DO

      ! GetTime()swap time for next average mean
      CALL ListAddConstReal( Model % Simulation,'Time_Start', rtime_end)

      END
