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
! *****************************************************************************
! *****************************************************************************
       SUBROUTINE Temporal_Mean_Step1_init0(Model,Solver,dt,TransientSimulation )
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
         str = ComponentName( 'Time integration Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )
         ! check also that we can get it
         IF(.NOT. GotIt) EXIT
          
         ! add exported variable
         WRITE(cvar, '(a,i1)') 'Exported Variable ',nvar
         CALL ListAddNewString( Solver % Values, TRIM(cvar), '-dofs 1 -elem Time_Int_'//TRIM(VarName))

      END DO

      END SUBROUTINE Temporal_Mean_Step1_init0
!
!------------------------------------------------------------------------------
      SUBROUTINE Temporal_Mean_Step1( Model,Solver,dt,TransientSimulation )
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
      TYPE(Variable_t) , POINTER :: Variable
      TYPE(Variable_t) , POINTER :: Time_Int_Variable

      LOGICAL,SAVE :: FirstTime=.TRUE.

      ! Solver reading
      CHARACTER(LEN=MAX_NAME_LEN) :: SolverName='Time_mean_step1'
      CHARACTER(LEN=MAX_NAME_LEN) :: VarName,str
      INTEGER :: nvar
      LOGICAL :: GotIt

      ! Get input and output netcdf files
      SolverParams => GetSolverParams()

      ! Initialisation
      IF (Firsttime) then
         ! define cumulated variable for each variable in sif
         nvar = 0
         DO WHILE( .TRUE. )
            nvar = nvar + 1
            str = ComponentName( 'Time integration Variable', nvar )
            VarName = ListGetString( SolverParams, str, GotIt )
            IF(.NOT. GotIt) EXIT
            ! check also that we can get it
            Variable => VariableGet( Model % Mesh % Variables,'Time_Int_'//TRIM(VarName), UnFoundFatal=.TRUE.)
            Variable % values(:) = 0.0_dp
         END DO
         
         ! sanity check if nvar = 0
         IF (nvar == 0) CALL FATAL(SolverName,'No Time integration Variable found, ERROR')

         ! initalise start time
         ! add -dt because this solver is call after time step
         CALL ListAddConstReal( Model % Simulation,'Time_Start',GetTime() - dt)

         FirstTime=.FALSE.

      END IF

      ! compute the time integration
      nvar = 0
      DO WHILE( .TRUE. )
         nvar = nvar + 1
         str = ComponentName( 'Time integration Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )
         IF(.NOT. GotIt) EXIT
         ! check also that we can get it

         ! retreive icedischarge
         Variable          => VariableGet(Model % Mesh % Variables,             TRIM(VarName),UnFoundFatal=.TRUE.)

         ! compute time integration
         Time_Int_Variable => VariableGet(Model % Mesh % Variables,'Time_Int_'//TRIM(VarName),UnFoundFatal=.TRUE.)
         Time_Int_Variable % values (:) = Time_Int_Variable % values (:) + dt * Variable % values (:)
      END DO

      END
