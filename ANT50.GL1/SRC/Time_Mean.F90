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
       SUBROUTINE Time_Mean_init0(Model,Solver,dt,TransientSimulation )
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
      CHARACTER(LEN=MAX_NAME_LEN) :: SolverName="Time_Mean"
      INTEGER :: nvar
      LOGICAL :: GotIt
      TYPE(ValueList_t), POINTER :: SolverParams

      ! Get input and output netcdf files
      SolverParams => GetSolverParams()

      ! Initialisation
      ! define cumulated variable for each variable in sif
      CALL INFO(SolverName,'',Level=1)
      CALL INFO(SolverName,'--------------------------',Level=1)
      CALL INFO(SolverName,'Time Mean Init'            ,Level=1)
      CALL INFO(SolverName,'',Level=1)

      nvar = 0
      DO WHILE( .TRUE. )
         nvar = nvar + 1
         str = ComponentName( 'Time Mean Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )

         ! check also that we can get it
         IF(.NOT. GotIt) EXIT
          
         WRITE(Message,'(a,a)') 'Average variable : ',TRIM(VarName)
         CALL INFO(SolverName,Message,Level=1)

         ! add exported variable
         WRITE(cvar, '(a,i1)') 'Exported Variable ',nvar
         CALL ListAddNewString( Solver % Values, TRIM(cvar), & 
                               '-dofs 1 -elem Mean_'//TRIM(VarName))

      END DO

      CALL INFO(SolverName,'--------------------------',Level=1)
      CALL INFO(SolverName,'',Level=1)

      END SUBROUTINE Time_Mean_init0
!
!------------------------------------------------------------------------------
      SUBROUTINE Time_Mean( Model,Solver,dt,TransientSimulation )
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
      TYPE(Variable_t) , POINTER :: Mean_Variable

      INTEGER, POINTER :: tmp(:)
      INTEGER, SAVE :: Output_interval, Time_interval
      INTEGER, SAVE :: ktime

      LOGICAL,SAVE :: FirstTime=.TRUE.
      LOGICAL,SAVE :: lmean

      ! Solver reading
      CHARACTER(LEN=MAX_NAME_LEN) :: SolverName='Time_mean'
      CHARACTER(LEN=MAX_NAME_LEN) :: VarName,str
      INTEGER :: nvar
      LOGICAL :: GotIt, Found

      ! Get input and output netcdf files
      SolverParams => GetSolverParams()

      ! Initialisation
      IF (Firsttime) then
         FirstTime=.FALSE.

         ! get output frequency
         tmp => ListGetIntegerArray( Model % Simulation, &
                   'Output Intervals', Found, UnfoundFatal=.TRUE.)
         IF(SIZE(tmp,1) > 1) CALL Fatal(SolverName, &
                                    'Only 1 Output Intervals supported')
         Output_interval = tmp(1)
         
         ! get simulation length
         tmp => ListGetIntegerArray( Model % Simulation, &
                   'Timestep Intervals', Found, UnfoundFatal=.TRUE.)
         IF(SIZE(tmp,1) > 1) CALL Fatal(SolverName, &
                                  'Only 1 Timestep Intervals supported')
         Time_interval = tmp(1)

         ! initialise time step count
         ktime = 0

         ! initialise lmean state
         lmean = .TRUE.  ! => this trigger an initialisation to 0 of Mean_Variable % values(:)
      END IF

      ! compute the time integration
      ktime = ktime + 1

      ! start temporal mean
      nvar = 0
      DO WHILE( .TRUE. )
         nvar = nvar + 1
         str = ComponentName( 'Time Mean Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )
         IF(.NOT. GotIt) EXIT

         ! retreive variable
         Variable => VariableGet(Model % Mesh % Variables, TRIM(VarName), UnFoundFatal=.TRUE.)

         ! retreive mean value
         Mean_Variable => VariableGet(Model % Mesh % Variables,'Mean_'//TRIM(VarName),UnFoundFatal=.TRUE.)

         ! reset Mean if already outputed at previous time step or first
         ! time step
         IF ( lmean ) THEN
            Mean_Variable % values(:) = 0.0_dp
         END IF

         ! compute mean if last time step or at each output time step
         lmean = .FALSE.
         IF ( ktime == Time_interval ) lmean = .TRUE.
         IF ( Output_interval > 0 ) THEN
            IF ( MOD(ktime - 1, Output_interval)== 0 ) lmean = .TRUE.
         END IF 

         ! compute time integration
         Mean_Variable % values (:) = Mean_Variable % values (:) + Variable % values (:)

         ! compute mean
         IF ( lmean ) THEN
             Mean_Variable % values (:) = Mean_Variable % values (:) / ktime
         END IF
      END DO

      ! sanity check if nvar = 0
      IF (nvar == 0) CALL FATAL(SolverName, &
                            'No Time Mean Variable found, ERROR')

      END
