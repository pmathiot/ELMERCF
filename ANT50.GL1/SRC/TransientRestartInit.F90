      SUBROUTINE TransientRestartInit( Model,Solver,dt,TransientSimulation )
      USE DefUtils
      IMPLICIT NONE

      TYPE(Model_t) :: Model
      TYPE(Solver_t):: Solver
      REAL(KIND=dp) :: dt
      LOGICAL :: TransientSimulation

      TYPE(Variable_t), POINTER :: DtVar 
      REAL(KIND=dp) :: dtPrev
      TYPE(Solver_t),POINTER ::  PSolver

      LOGICAL :: Trestart
      LOGICAL :: Found
      LOGICAL :: Reset
      CHARACTER(LEN=MAX_NAME_LEN) :: EqName
      CHARACTER(LEN=MAX_NAME_LEN) :: Caller="TransientRestartInit"

      INTEGER :: s


      Reset = ListGetLogical(Solver % Values,&
                  "Do not use previous values",Found)
      IF (Reset) THEN

        DO s=1,Model % NumberOfSolvers
          PSolver => Model % Solvers(s)
          Trestart = ListGetLogical( PSolver  % Values, &
                               "Transient Restart",Found)
          EqName = ListGetString(PSolver % Values, 'Equation',Found)
          
          IF ((Trestart).AND.(PSolver % DoneTime.GT.0)) THEN
            CALL Warn(Caller,"*******")
            CALL Warn(Caller,"Use cold restart for equation"//TRIM(EqName))
            CALL Warn(Caller,"*******")
            PSolver % DoneTime = 0
          END IF
        END DO
      END IF


      dtPrev = ListGetConstReal( Solver % Values, &
              'Time Step size Previous Values',UnFoundFatal=.TRUE.)

      DtVar => VariableGet( Solver % Mesh % Variables, 'Timestep size' )
      IF (.NOT.ASSOCIATED(DtVar % PrevValues)) &
       CALL FATAL("TransientRestartInit", &
                  "DtVar PreValues Not Associated")
      DtVar % PrevValues=dtPrev

      End SUBROUTINE TransientRestartInit
