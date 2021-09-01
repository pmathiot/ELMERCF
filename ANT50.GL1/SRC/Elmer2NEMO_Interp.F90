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
       SUBROUTINE Elmer2NEMO_Interp_init0(Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
       USE DefUtils
       IMPLICIT NONE
!------------------------------------------------------------------------------
       TYPE(Solver_t), TARGET :: Solver
       TYPE(Model_t) :: Model
       REAL(KIND=dp) :: dt
       LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN) :: Name
  
        Name = ListGetString( Solver % Values, 'Equation',UnFoundFatal=.TRUE.)
        CALL ListAddNewString( Solver % Values,'Variable',&
           '-nooutput '//TRIM(Name)//'_var')
        CALL ListAddLogical(Solver % Values, 'Optimize Bandwidth',.FALSE.)

        CALL ListAddInteger(Solver % Values,&
                'Nonlinear System Norm Degree',0)

      END SUBROUTINE Elmer2NEMO_Interp_init0
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      SUBROUTINE Elmer2NEMO_Interp( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
      USE ParticleUtils
      USE Interpolation
      USE Netcdf
!------------------------------------------------------------------------------
      IMPLICIT NONE
!------------------------------------------------------------------------------
      TYPE(Solver_t) :: Solver
      TYPE(Model_t) :: Model
      REAL(KIND=dp) :: dt
      LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
      TYPE(ValueList_t), POINTER :: SolverParams
      TYPE(Variable_t),POINTER :: Variable
      REAL(kind=dp), POINTER :: Values(:)
      INTEGER, POINTER :: Perm(:)

      INTEGER :: nx,ny
      INTEGER :: ncid
      INTEGER :: vid,dimid(2)
      INTEGER,allocatable :: varid(:)
      REAL(kind=dp), Allocatable :: lon(:,:),lat(:,:)
      REAL(kind=dp), Allocatable :: x(:,:),y(:,:),Var(:,:,:),ParVar(:,:,:)
      INTEGER,Allocatable :: eindex(:,:)
      INTEGER :: ii,jj,ll
      INTEGER :: Gmax
      INTEGER,Allocatable :: GElementIndex(:)
      INTEGER :: ierr
      REAL(KIND=dp) :: FillValue=-HUGE(1.0)  ! fill value; make it small
                                             ! so we can reduce the
                                             ! results in paralle using
                                             ! MPI_MAX

      ! Variable related to elements
      TYPE(Element_t),POINTER ::  Element
      TYPE(Nodes_t),SAVE :: ElementNodes
      INTEGER :: ElementIndex
      INTEGER, POINTER :: NodeIndexes(:)
      REAL(KIND=dp),ALLOCATABLE,SAVE :: Basis(:), dBasisdx(:,:)
      REAL(KIND=dp) :: SqrtElementMetric,UVW(3)
      INTEGER :: n
      LOGICAL :: stat
      
      REAL(kind=dp) :: Coord(3)

      CHARACTER(LEN=MAX_NAME_LEN) :: SOLVERNAME="Elmer2Nemo"
      CHARACTER(MAX_NAME_LEN) :: GRID_FILE,OUTPUT_FILE,OFILE
      CHARACTER(MAX_NAME_LEN) :: VarName,str
      INTEGER :: nvar

      LOGICAL :: GotIt
      LOGICAL :: Parallel
      LOGICAL :: lfile                  ! GRID_FILE presence flag
      LOGICAL,SAVE :: FirstTime=.TRUE.

      CALL INFO(SOLVERNAME,'',Level=1)
      CALL INFO(SOLVERNAME,'Interpol file to NEMO grid',Level=1)
      CALL INFO(SOLVERNAME,'--------------------------',Level=1)

      ! Parallel run
      Parallel=(ParEnv % PES > 1)

      ! Do some allocation
      If (Firsttime) then
        N = model % MaxElementNodes
        allocate(ElementNodes % x(N), ElementNodes % y(N), ElementNodes % z(N))
        allocate(Basis(N), dBasisdx(N,3))

        FirstTime=.False.
      END IF

      ! Get input and output netcdf files
       SolverParams => GetSolverParams()
       GRID_FILE=ListGetString(SolverParams,'Input File',UnFoundFatal=.TRUE.)
       OUTPUT_FILE=ListGetString(SolverParams,'OutPut File',UnFoundFatal=.TRUE.)
       CALL INFO(SOLVERNAME,'Output File : '//OUTPUT_FILE,Level=1)
       CALL INFO(SOLVERNAME,'',Level=1)

      ! get variables to interpolate
       nvar = 0
       DO WHILE( .TRUE. )
         nvar = nvar + 1
         str = ComponentName( 'Save Variable', nvar )
         VarName = ListGetString( SolverParams, str, GotIt )
         IF(.NOT. GotIt) EXIT
         ! check also that we can get it
         Variable => VariableGet(Solver%Mesh%Variables,TRIM(VarName),UnFoundFatal=.TRUE.)
       END DO
       nvar=nvar-1
       allocate(varid(4+nvar))

      ! CHECK PRESENCE OF GRID_FILE
       INQUIRE(FILE=TRIM(GRID_FILE), EXIST=lfile)
       IF (.NOT. lfile) THEN
          PRINT *, TRIM(GRID_FILE)," is missing ; STOP 99"
          STOP 99
       END IF

      ! READ GRID FILE
        call check(nf90_open(TRIM(GRID_FILE), NF90_NOWRITE, ncid))
        call check(nf90_inq_dimid(ncid,'x',vid))
        call check(nf90_inquire_dimension(ncid,vid,len=nx))
        call check(nf90_inq_dimid(ncid,'y',vid))
        call check(nf90_inquire_dimension(ncid,vid,len=ny))

      ! read grid coordinates
        allocate(x(nx,ny),y(nx,ny),eindex(nx,ny),Var(nx,ny,nvar),ParVar(nx,ny,nvar))
        allocate(lon(nx,ny),lat(nx,ny))

        call check(nf90_inq_varid(ncid,'x',vid))
        call check(nf90_get_var(ncid,vid,x))

        call check(nf90_inq_varid(ncid,'y',vid))
        call check(nf90_get_var(ncid,vid,y))

        call check(nf90_inq_varid(ncid,'eindex',vid))
        call check(nf90_get_var(ncid,vid,eindex))

        call check(nf90_inq_varid(ncid,'lon',vid))
        call check(nf90_get_var(ncid,vid,lon))

        call check(nf90_inq_varid(ncid,'lat',vid))
        call check(nf90_get_var(ncid,vid,lat))

        call check( nf90_close(ncid))
        
        ! For parallel run we the element number correspond
        ! to the GElementIndex (no renumbering between serial and  partitionned mesh !!)
        ! Make a perm table for the conversion
        IF (Parallel) THEN
         Gmax=MAXVAL(Solver%Mesh%Elements(:)%GElementIndex)
         PRINT *,' Gmax : ',Gmax,' eindex max = ',MAXVAL(eindex)
         Gmax=MAX(Gmax,MAXVAL(eindex))
         ALLOCATE(GElementIndex(Gmax))
         GElementIndex=-1
         DO ii=1,Solver%Mesh%NumberOfBulkElements
           Element => Solver%Mesh%Elements(ii)
           GElementIndex(Element % GElementIndex) = Element % ElementIndex
         END DO
        ENDIF

       CALL StartAdvanceOutput( TRIM(SolverName),&
                   'start to locate grid in mesh')

       Var=FillValue
        Do ii=1,nx
          Do jj=1,ny

            CALL AdvanceOutput((ii-1)*ny+jj,nx*ny,percent_t=5._dp)

            Coord(1)=x(ii,jj)
            Coord(2)=y(ii,jj)
            Coord(3)=0._dp

            IF (Parallel) THEN
              ElementIndex=-1
              IF (eindex(ii,jj).GT.0) &
                ElementIndex=GElementIndex(eindex(ii,jj))
            Else
              ElementIndex=eindex(ii,jj)
            End if

            ! ElementIndex>0 if the point is in the mesh
            IF (ElementIndex.GT.0) THEN
               Element => Solver % Mesh % Elements(ElementIndex)
               n = GetElementNOFNodes(Element)
               NodeIndexes => Element % NodeIndexes
               ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
               ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
               ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)

               ! The point should be found in the element
               IF (.NOT.PointInElement(Element,ElementNodes,Coord,UVW))  THEN
                PRINT *,ParEnv%MyPE,'eindex',Element % ElementIndex
                PRINT *,ParEnv%MyPE,'obs. coords',Coord(1:2)
                PRINT *,ParEnv%MyPE,'node indexes',NodeIndexes(1:n)
                PRINT *,ParEnv%MyPE,'nodex',ElementNodes % x(1:n)
                PRINT *,ParEnv%MyPE,'nodey',ElementNodes % y(1:n)
                CALL FATAL(SolverName,&
                        'Point was supposed to be found in this element')
               ELSE
                 stat = ElementInfo( Element,ElementNodes,UVW(1),UVW(2),UVW(3),SqrtElementMetric, &
                              Basis,dBasisdx )

                 ! interpolate variable results
                 Do ll=1,nvar
                   str = ComponentName( 'Save Variable', ll )
                   VarName = ListGetString( SolverParams, str, GotIt )
                   Variable => VariableGet(Solver%Mesh%Variables,TRIM(VarName),UnFoundFatal=.TRUE.)
                   Values => Variable % Values
                   Perm => Variable % Perm
                   Var(ii,jj,ll)=SUM(Values(Perm(NodeIndexes(1:n)))*basis(1:n))
                 End do

               END IF

             END IF

          End Do
        End Do

         ! Save on the NEMO grid
        IF (ParEnv%MyPE==0) THEN
         call check( nf90_create(TRIM(OUTPUT_FILE),NF90_CLOBBER,ncid))
         call check( nf90_def_dim(ncid,'x',nx,dimid(1)))
         call check( nf90_def_dim(ncid,'y',ny,dimid(2)))

         call check( nf90_def_var(ncid,'lon',NF90_DOUBLE,(/dimid(1),dimid(2)/),varid(1)))
         call check( nf90_put_att(ncid, varid(1), "standard_name", "longitude") )
         call check( nf90_put_att(ncid, varid(1), "long_name", "longitude") )
         call check( nf90_put_att(ncid, varid(1), "units", "degrees_east") )

         call check( nf90_def_var(ncid,'lat',NF90_DOUBLE,(/dimid(1),dimid(2)/),varid(2)))
         call check( nf90_put_att(ncid, varid(2), "standard_name","latitude") )
         call check( nf90_put_att(ncid, varid(2), "long_name", "latitude") )
         call check( nf90_put_att(ncid, varid(2), "units", "degrees_north") )


         call check(nf90_def_var(ncid,'x',NF90_DOUBLE,(/dimid(1),dimid(2)/),varid(3)))
         call check(nf90_def_var(ncid,'y',NF90_DOUBLE,(/dimid(1),dimid(2)/),varid(4)))
        
          Do ll=1,nvar
            str = ComponentName( 'Save Variable', ll )
            VarName = ListGetString( SolverParams, str, GotIt )
            vid=4+ll
            call check(nf90_def_var(ncid,TRIM(VarName),NF90_DOUBLE,(/dimid(1),dimid(2)/),varid(vid)))
            call check( nf90_put_att(ncid, varid(vid), "_FillValue", FillValue) )
          End do

          call check( NF90_ENDDEF(ncid))

          call check(nf90_put_var(ncid,varid(1),lon))
          call check(nf90_put_var(ncid,varid(2),lat))
          call check(nf90_put_var(ncid,varid(3),x))
          call check(nf90_put_var(ncid,varid(4),y))
       END IF

      ! make reduction for parallel case
       IF (Parallel) THEN
           CALL MPI_Reduce(Var,ParVar,nx*ny*nvar,MPI_DOUBLE,MPI_MAX,0,ELMER_COMM_WORLD,ierr)
       END IF

        Do ll=1,nvar
          vid=4+ll

          IF (ParEnv%MyPE==0) THEN
           IF (Parallel) THEN
             call check(nf90_put_var(ncid,varid(vid),ParVar(:,:,ll)))
           ELSE
             call check(nf90_put_var(ncid,varid(vid),Var(:,:,ll)))
           END IF
          END IF
        End do

         IF (ParEnv%MyPE==0)  call check( nf90_close(ncid))


        deallocate(x,y)
        deallocate(eindex)
        deallocate(lon,lat)
        deallocate(Var,ParVar)
        deallocate(varid)
        IF (Parallel) deallocate(GElementIndex)

        CONTAINS
        subroutine check(status)
          integer, intent ( in) :: status
          CHARACTER(LEN=MAX_NAME_LEN) :: Message
          if (status /= nf90_noerr)  then
             print *, trim(nf90_strerror(status))
             STOP 99
          endif
       end subroutine check
       END
