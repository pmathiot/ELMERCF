      SUBROUTINE SaveNetcdf( Model,Solver,dt,TransientSimulation )
      USE DefUtils
      USE Netcdf
      IMPLICIT NONE
      TYPE(Solver_t),TARGET :: Solver 
      TYPE(Model_t) :: Model
      REAL(KIND=dp) :: dt
      LOGICAL :: TransientSimulation 

      TYPE(Mesh_t),POINTER :: Mesh

      INTEGER,parameter  :: three=3,two=2
      INTEGER,parameter  :: ndim=7
      !1:nVertices,2:nInterfaces,3:nBulkElements,4:Three,5:nBCElements,6:Two,7:time
      INTEGER,parameter  :: nvar=25
      !1:Vertices_GlobalDOFs,2:Vertices_INTERFACES,3:BulkElement_vertices,4:BCElement_vertices,
      !5:BCElement_Parent,
      !6:BulkElement_Area,7:BCElement_Area,8:Node_Area,9:x,10:y
      !11: time, 12:ux, 13:uy, 14:h, 15: bed, 16:zd, 17:zs, 18:basaldrag,
      !19:mask
      !20 : smbflux, 21: dhdtflux, 22: outflow
      !23 : H residual Flux , 24: bmbflux

      Character(LEN=MAX_NAME_LEN),parameter :: &
                SolverName='INITMIP_SaveNetcdf'
      Character(LEN=MAX_NAME_LEN) :: OUTPUT_FName
      Character(LEN=MAX_NAME_LEN) :: OUTPUT_FName_D='SaveNetcdf'
      Character(LEN=MAX_NAME_LEN),SAVE :: FileName

      TYPE(Element_t),POINTER :: Element,Parent
      TYPE(Nodes_t),SAVE :: ElementNodes
      TYPE(GaussIntegrationPoints_t) :: IntegStuff
      TYPE(Variable_t),POINTER :: TVar
      TYPE(ValueList_t),POINTER :: SolverParams
      REAL(KIND=dp) :: U,V,W,SqrtElementMetric 
      REAL(KIND=dp),ALLOCATABLE,SAVE :: Basis(:), dBasisdx(:,:)
      REAL(KIND=dp),ALLOCATABLE,SAVE :: NodalGM(:),NodalH(:),MinH(:)
      REAL(KIND=dp),ALLOCATABLE,SAVE :: NodalSMB(:),NodalBMB(:)
      REAL(KIND=dp),ALLOCATABLE,SAVE :: LocalArea(:)
      REAL(KIND=dp),ALLOCATABLE,SAVE :: SMB(:),BMB(:),SMB_Min(:),DHDT(:),CALV(:)
      REAL(KIND=dp),ALLOCATABLE,SAVE :: NodeArea(:)
      REAL(KIND=dp) :: bdrag,dtc
      REAL(KIND=dp),SAVE :: SAVE_INTERVAL
      REAL(KIND=dp) :: modlo
      INTEGER :: ncid
      INTEGER :: mask
      INTEGER :: i,M
      INTEGER, POINTER :: Indexes(:)
      INTEGER, POINTER :: Permutation(:)
      INTEGER,SAVE :: nTime=0,nVisit=0
      INTEGER,SAVE :: dimid(ndim),varid(nvar)
      INTEGER,SAVE :: NInterfaces

      LOGICAL,SAVE :: IsParallel
      LOGICAL,SAVE :: AllocationDone=.False.
      LOGICAL,SAVE :: FirstVisit=.TRUE.
      LOGICAL :: Gotit

      IF (.NOT.ASSOCIATED(Solver%Variable)) & 
         CALL FATAL(SolverName,'Solver Variable Not associated')
      Permutation => Solver%Variable%Perm

      Mesh => Model % Mesh

      IF (.NOT.AllocationDone) THEN
         AllocationDone=.TRUE.
         M=Model % MaxElementNodes
         ALLOCATE(Basis(M),dBasisdx(M,3), &
                  NodalGM(M),NodalH(M),MinH(M),&
                  NodalSMB(M),NodalBMB(M),LocalArea(M))

         ALLOCATE(NodeArea(Mesh % NumberOfNodes))

        ALLOCATE(SMB(Mesh%NumberOfBulkElements),&
                 BMB(Mesh%NumberOfBulkElements),&
                  SMB_Min(Mesh%NumberOfBulkElements),&
                  DHDT(Mesh%NumberOfBulkElements),&
                  CALV(Mesh%NumberOfBoundaryElements))
         SMB=0._dp
         BMB=0._dp
         SMB_Min=0._dp
         DHDT=0._dp
         CALV=0._dp
      END IF


      IF (FirstVisit) THEN
        NInterfaces=1
        IsParallel=(ParEnv % PEs > 1)
       
        SolverParams => GetSolverParams()
        OUTPUT_FName = ListGetString(SolverParams,'File Name',Gotit)
        IF (.NOT.Gotit) OUTPUT_FName=OUTPUT_FName_D

        SAVE_INTERVAL=ListGetConstReal(SolverParams,'Save Interval',UnFoundFatal=.TRUE.)

        If (IsParallel) THEN
         write(FileName,'(a,I0,a)') trim(OUTPUT_FName),ParEnv%MyPe,'.nc'
         Do i=1,Mesh%NumberOfNodes
            NInterfaces=Max(NInterfaces,size(Mesh % ParallelInfo % NeighbourList(i) % Neighbours(:)))
         End do
        Else
          write(FileName,'(a,a)') trim(OUTPUT_FName),'.nc'
        End if
        call CreatNetcdfFile(FileName, Mesh%NumberOfNodes,&
                                     NInterfaces,&
                                     Mesh%NumberOfBulkElements,&
                                     Mesh%NumberOfBoundaryElements,&
                                     ncid,dimid,varid)
        CALL WriteMeshInfo(FileName,varid)
        FirstVisit=.FALSE.
      END IF

      TVar => VariableGet(Model%Mesh%Variables,'Time')
      IF (.NOT.ASSOCIATED(TVar)) &
         CALL FATAL(SolverName,'Variable <Time> missing') 
      
      nVisit=nVisit+1
      CALL COMPUTE_FLUX_VARIABLES(SMB,BMB,SMB_Min,DHDT,CALV)

      modlo=modulo(TVar%Values(1),SAVE_INTERVAL)
      dtc=MIN(modlo,(SAVE_INTERVAL-modlo))
      IF (dtc.GT.(0.5*dt)) RETURN
      nTime=nTime+1

      CALL SAVE_SCALAR_VARIABLES(FileName,nTime)
      CALL SAVE_FLUX_VARIABLES(FileName,nTime,nVisit,SMB,BMB,SMB_Min,DHDT,CALV)

      CONTAINS

      SUBROUTINE SAVE_FLUX_VARIABLES(FName,nTime,nVisit,SMB,BMB,SMB_Min,DHDT,CALV)
      IMPLICIT NONE
      CHARACTER(LEN=MAX_NAME_LEN),INTENT(IN) :: FName
      INTEGER :: nTime
      INTEGER :: nVisit
      REAL(KIND=dp) :: SMB(:),BMB(:),SMB_Min(:),DHDT(:),CALV(:)
      INTEGER :: M
      INTEGER :: N

        SMB=SMB/nVisit
        BMB=BMB/nVisit
        SMB_Min=SMB_Min/nVisit
        DHDT=DHDT/nVisit
        CALV=CALV/nVisit
       
         M=Mesh%NumberOfBulkElements
         N=Mesh%NumberOfBoundaryElements

        call check(nf90_open(trim(FileName),NF90_WRITE,ncid))
        call check(nf90_put_var(ncid,varid(20),SMB(1:M),start=(/1,nTime/),count=(/M,1/)))
        call check(nf90_put_var(ncid,varid(24),BMB(1:M),start=(/1,nTime/),count=(/M,1/)))
        call check(nf90_put_var(ncid,varid(21),DHDT(1:M),start=(/1,nTime/),count=(/M,1/)))
        IF (Mesh%NumberOfBoundaryElements.NE.0) &
          call check(nf90_put_var(ncid,varid(22),CALV(1:N),start=(/1,nTime/),count=(/N,1/)))
        call check(nf90_put_var(ncid,varid(23),SMB_Min(1:M),start=(/1,nTime/),count=(/M,1/)))
        call check(nf90_put_var(ncid,varid(25),nVisit,start=(/nTime/)))

        call check(nf90_close(ncid))

        SMB=0._dp
        BMB=0._dp
        SMB_Min=0._dp
        DHDT=0._dp
        CALV=0._dp
        nVisit=0
      END SUBROUTINE SAVE_FLUX_VARIABLES

      SUBROUTINE COMPUTE_FLUX_VARIABLES(SMB,BMB,SMB_Min,DHDT,CALV)
      IMPLICIT NONE
      REAL(KIND=dp) :: SMB(:),BMB(:),SMB_Min(:),DHDT(:),CALV(:)
      INTEGER :: t,n,i,j,EINdex
      TYPE(ValueList_t), POINTER :: BodyForce,BC
      REAL(KIND=dp) :: wgt,cellarea,SMBAtIP,BMBAtIP,SMBFlux,BMBFlux,coeff
      REAL(KIND=dp) :: SMB_MinFlux,DHDTFlux,DHDTAtIP,CalvingFlux
      LOGICAL :: Found
      LOGICAL :: stat
      TYPE(Variable_t),POINTER :: DHDTVar,HRVar,SSAVar,HVar
      INTEGER :: FlowDOfs
      REAL(KIND=dp) :: Flow(3),Normal(3)
      LOGICAL :: CalvingFront

      DHDTVar => VariableGet(Model%Mesh%Variables,'dhdt',UnFoundFatal=.TRUE.)
      HRVar => VariableGet(Model%Mesh%Variables,'H Residual',UnFoundFatal=.TRUE.)
      SSAVar => VariableGet(Model%Mesh%Variables,'SSAVelocity',UnFoundFatal=.TRUE.)
      FlowDOfs=SSAVar%DOFs
      HVar => VariableGet(Model%Mesh%Variables,'h',UnFoundFatal=.TRUE.)

      Do t=1,Mesh%NumberOfBulkElements
         Element => Mesh % Elements(t)
         Model % CurrentElement => Mesh % Elements(t)

         n = GetElementNOFNodes(Element)
         Indexes => Element % NodeIndexes
         CALL GetElementNodes( ElementNodes,Element)

         BodyForce => GetBodyForce(Element)
         IF (.NOT.ASSOCIATED(BodyForce)) &
             CALL FATAL(SolverName,'No BodyForce Found')

         NodalSMB=0._dp
         NodalSMB(1:n) =&
            ListGetReal(BodyForce,'Top Surface Accumulation',n,Indexes,UnfoundFatal=.TRUE. )

         NodalBMB=0._dp
         NodalBMB(1:n) =&
            ListGetReal(BodyForce,'Bottom Surface Accumulation',n,Indexes,UnfoundFatal=.TRUE. )

         cellarea=0._dp
         DHDTFlux=0._dp
         SMBFlux=0._dp
         BMBFlux=0._dp
         LocalArea=0._dp

         IntegStuff = GaussPoints( Element )
         DO i=1,IntegStuff % n
          U = IntegStuff % u(i)
          V = IntegStuff % v(i)
          W = IntegStuff % w(i)

          stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
                        Basis,dBasisdx )

          wgt=SqrtElementMetric*IntegStuff % s(i)
         ! cell area
         cellarea=cellarea+wgt
         
         LocalArea(1:n)=LocalArea(1:n)+ wgt* Basis(1:n)
   
         SMBAtIP=SUM(NodalSMB(1:n)*Basis(1:n))
         BMBAtIP=SUM(NodalBMB(1:n)*Basis(1:n))
         DHDTAtIP=SUM(DHDTVAR%Values(DHDTVAR%Perm(Indexes(1:n)))*Basis(1:n))

         DHDTFlux=DHDTFlux+DHDTAtIP*wgt
         SMBFlux=SMBFlux+SMBAtIP*wgt
         BMBFlux=BMBFlux+BMBAtIP*wgt
         End DO
         
         SMB_MinFlux=0._dp
         Do i=1,n
           SMB_MinFlux=SMB_MinFlux + &
              HRVar%Values(HRVar%Perm(Indexes(i)))*LocalArea(i)/NodeArea(Permutation(Indexes(i)))
         End do

         SMB_Min(t)=SMB_Min(t)+SMB_MinFlux/cellarea
         SMB(t)=SMB(t)+SMBFlux/cellarea
         BMB(t)=BMB(t)+BMBFlux/cellarea
         DHDT(t)=DHDT(t)+DHDTFlux/cellarea
      End do

      Do t=1,Mesh % NumberOfBoundaryElements
         EIndex=Mesh % NumberOfBulkElements+t
         Element => Mesh % Elements(EIndex)
         Model % CurrentElement => Mesh % Elements(EIndex)

         !IF ( .NOT. ActiveBoundaryElement(Element) ) CYCLE
         IF ( GetElementFamily(Element) == 1 ) CYCLE

         BC => GetBC(Element)
         IF ( .NOT. ASSOCIATED(BC) ) CYCLE
         CalvingFront=.FALSE.
         CalvingFront=ListGetLogical(BC,'Calving Front', Found)
         IF (.NOT.CalvingFront) CYCLE

         n = GetElementNOFNodes(Element)
         Indexes => Element % NodeIndexes
         CALL GetElementNodes( ElementNodes,Element)

         NodalH(1:n) = HVar%Values(HVar%Perm(Indexes(1:n)))
         
         CalvingFlux=0._dp
         IntegStuff = GaussPoints( Element )
         DO i=1,IntegStuff % n
           U = IntegStuff % u(i)
           V = IntegStuff % v(i)
           W = IntegStuff % w(i)
           stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
                                   Basis,dBasisdx )
          
           Normal=0._dp
           Normal = NormalVector( Element,ElementNodes,u,v,.TRUE. )

           coeff=SUM(NodalH(1:n)*Basis(1:n))
           Flow=0._dp
           DO j=1,FlowDofs
              Flow(j) = coeff*SUM( SSAVar%Values(FlowDofs*(SSAVar%Perm(Indexes(1:n))-1)+j) * Basis(1:n) )
           END DO
           CalvingFlux=CalvingFlux+&
                       SUM(Normal * Flow)*SqrtElementMetric*IntegStuff%s(i)
         END DO
         CALV(t)=CALV(t)+CalvingFlux
      END DO
      END SUBROUTINE COMPUTE_FLUX_VARIABLES

      SUBROUTINE SAVE_SCALAR_VARIABLES(FName,nTime)
      IMPLICIT NONE
      CHARACTER(LEN=MAX_NAME_LEN),INTENT(IN) :: FName
      INTEGER,INTENT(IN) :: nTime
      TYPE(ValueList_t), POINTER :: Material
      TYPE(Variable_t),POINTER :: Var,Var2,HVar,GMVar
      REAL(dp) :: COMPUTE_SSA_NODAL_Bdrag
      INTEGER :: t,n
      LOGICAL :: Found
      
        call check(nf90_open(trim(FileName),NF90_WRITE,ncid))
        !11: time, 12:ux, 13:uy, 14:h, 15: bed, 16:zd, 17:zs, 18:basaldrag,
        call check(nf90_put_var(ncid,varid(11),TVar%Values(1),start= (/nTime/)))
        PRINT *,trim(FileName),nTime,TVar%Values(1)

        Var => VariableGet(Model%Mesh%Variables,'SSAVelocity 1',UnFoundFatal=.TRUE.)
        call check(nf90_put_var(ncid,varid(12),Var%Values(Var%Perm(1:Mesh%NumberOfNodes)),start= (/1,nTime/)))

        Var => VariableGet(Model%Mesh%Variables,'SSAVelocity 2',UnFoundFatal=.TRUE.)
        call check(nf90_put_var(ncid,varid(13),Var%Values(Var%Perm(1:Mesh%NumberOfNodes)),start= (/1,nTime/)))

        IF (nTime.EQ.1) THEN
          Var => VariableGet(Model%Mesh%Variables,'Bedrock',UnFoundFatal=.TRUE.)
          call check(nf90_put_var(ncid,varid(15),Var%Values(Var%Perm(1:Mesh%NumberOfNodes))))
        END IF

        HVar => VariableGet(Model%Mesh%Variables,'h',UnFoundFatal=.TRUE.)
        call check(nf90_put_var(ncid,varid(14),HVar%Values(HVar%Perm(1:Mesh%NumberOfNodes)),start= (/1,nTime/)))

        Var => VariableGet(Model%Mesh%Variables,'zb',UnFoundFatal=.TRUE.)
        call check(nf90_put_var(ncid,varid(16),Var%Values(Var%Perm(1:Mesh%NumberOfNodes)),start= (/1,nTime/)))

        Var => VariableGet(Model%Mesh%Variables,'zs',UnFoundFatal=.TRUE.)
        call check(nf90_put_var(ncid,varid(17),Var%Values(Var%Perm(1:Mesh%NumberOfNodes)),start= (/1,nTime/)))

        Do i=1,Mesh%NumberOfNodes
           bdrag=COMPUTE_SSA_NODAL_Bdrag(i)
           call check(nf90_put_var(ncid,varid(18),bdrag,start= (/i,nTime/)))
        End do

        GMVar => VariableGet(Model%Mesh%Variables,'groundedMask',UnFoundFatal=.TRUE.)

        Do t=1,Mesh % NumberOfBulkElements
           Element => Mesh % Elements(t)
           n = GetElementNOFNodes(Element)
           Indexes => Element % NodeIndexes

           Material => GetMaterial(Element)
           IF (.NOT.ASSOCIATED(Material)) &
             CALL FATAL(SolverName,'No Material Found')

           MinH=0._dp
           MinH(1:n) =   ListGetReal(Material,'Min H',n,Indexes,UnfoundFatal=.TRUE. )

           NodalH(1:n)=HVar%Values(HVar%Perm(Indexes(1:n)))

           IF (ALL((NodalH(1:n)-MinH(1:n)).LE.0._dp)) THEN
             mask=3
           ELSE
              NodalGM(1:n) = GMVar%Values(GMVar%Perm(Indexes(1:n)))
              IF (ANY(NodalGM(1:n).LT.0._dp)) THEN
                mask=2
             ELSE
                mask=1
             ENDIF
          ENDIF
          call check(nf90_put_var(ncid,varid(19),mask,start= (/t,nTime/)))
        End do

        call check(nf90_close(ncid))
      END SUBROUTINE SAVE_SCALAR_VARIABLES


      SUBROUTINE WriteMeshInfo(FName,varid)
      IMPLICIT NONE
      CHARACTER(LEN=MAX_NAME_LEN),INTENT(IN) :: FName 
      INTEGER,INTENT(IN) :: varid(:)
      !1:Vertices_GlobalDOFs,2:Vertices_INTERFACES,3:BulkElement_vertices,4:BCElement_vertices,
      !5:BCElement_Parent,
      !6:BulkElement_Area,7:BCElement_Area,8:Node_Area,9:x,10:y

      INTEGER :: ncid
      REAL(KIND=dp) :: area
      LOGICAL :: stat
      INTEGER, ALLOCATABLE :: interf(:)
      INTEGER :: t,i
      INTEGER :: n,ni
      INTEGER :: Vertice
      INTEGER :: status
      INTEGER :: start(1)
      INTEGER :: EIndex
      
      call check(nf90_open(trim(FName),NF90_WRITE,ncid))

      allocate(interf(NInterfaces))
      Do i=1,Mesh % NumberOfNodes
         interf(:)=-1
         IF (IsParallel) THEN
            Vertice = Mesh % ParallelInfo % GlobalDOFs(i)
            ni=size(Mesh % ParallelInfo % NeighbourList(i) % Neighbours(:))
            interf(1:ni)=Mesh % ParallelInfo % NeighbourList(i) % Neighbours(:)
         Else
            Vertice = i
         End if
         call check(nf90_put_var(ncid,varid(1),Vertice, start = (/i/)))
         !IF (IsParallel) &
         !   call check(nf90_put_var(ncid,varid(2),interf(1:NInterfaces),&
         !        start = (/i,1/), count = (/1,NInterfaces/)))
         call check(nf90_put_var(ncid,varid(9),Model%Mesh%Nodes%x(i),start = (/i/)))
         call check(nf90_put_var(ncid,varid(10),Model%Mesh%Nodes%y(i),start = (/i/)))
      End do
      deallocate(interf)

      NodeArea=0._dp
      Do t=1,Mesh % NumberOfBulkElements
         Element => Mesh % Elements(t)
         IF (Element % TYPE % NumberOfNodes.NE.3) &
          CALL FATAL(SolverName,'This work only with 303 Bulk Elements')
         n = GetElementNOFNodes(Element)
         Indexes => Element % NodeIndexes
         CALL GetElementNodes( ElementNodes, Element )
         IntegStuff = GaussPoints( Element )
         area=0._dp
         Do i=1,IntegStuff % n
            U = IntegStuff % u(i)
            V = IntegStuff % v(i)
            W = IntegStuff % w(i)
            stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
                        Basis,dBasisdx )
            NodeArea(Permutation(Indexes(1:n)))=NodeArea(Permutation(Indexes(1:n)))+&
                SqrtElementMetric*IntegStuff % s(i) * Basis(1:n)
            area=area+SqrtElementMetric*IntegStuff % s(i)
         End do
         call check(nf90_put_var(ncid,varid(3),Indexes(1:n), start = (/t,1/),count=(/1,3/)))
         call check(nf90_put_var(ncid,varid(6),area, start = (/t/) ))
      End do
      IF (IsParallel) CALL ParallelSumVector( Solver % Matrix, NodeArea, 0 )
      call check(nf90_put_var(ncid,varid(8),NodeArea(Permutation(1:Mesh % NumberOfNodes)), start = (/1/)))
  
      Do t=1,Mesh % NumberOfBoundaryElements
         EIndex=Mesh % NumberOfBulkElements+t
         Element => Mesh % Elements(EIndex)
         IF (Element % TYPE % NumberOfNodes.NE.2) &
          CALL FATAL(SolverName,'This work only with 202 BC Elements')

         n = GetElementNOFNodes(Element)
         Indexes => Element % NodeIndexes
         CALL GetElementNodes( ElementNodes , Element)

         IntegStuff = GaussPoints( Element )
         Do i=1,IntegStuff % n
            U = IntegStuff % u(i)
            V = IntegStuff % v(i)
            W = IntegStuff % w(i)
            stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
                        Basis,dBasisdx )
            area=area+SqrtElementMetric*IntegStuff % s(i)
         End do
         call check(nf90_put_var(ncid,varid(4),Indexes(1:n), start = (/t,1/) ,count = (/1,2/)))
         call check(nf90_put_var(ncid,varid(7),area, start = (/t/) ))

         IF (.NOT.ASSOCIATED(Element % BoundaryInfo)) CYCLE
         Parent => Element % BoundaryInfo % Left
         IF (.NOT.ASSOCIATED(Parent) ) &
            Parent => Element % BoundaryInfo % Right
         IF (.NOT.ASSOCIATED(Parent)) CYCLE
         call check(nf90_put_var(ncid,varid(5),Parent%ElementIndex,start = (/t/) ))
      End do

      call check(nf90_close(ncid))

      END SUBROUTINE WriteMeshInfo

      SUBROUTINE CreatNetcdfFile(FName,&
                                NNodes,NInterfaces,NBulkElements,NBCElements,&
                                ncid,dimid,varid)

      implicit none
      Character(LEN=MAX_NAME_LEN),INTENT(IN) :: FName
      INTEGER, INTENT(IN) :: NNodes,NInterfaces,NBulkElements,NBCElements
      INTEGER,INTENT(OUT) :: ncid
      INTEGER, DIMENSION(:),INTENT(OUT) :: dimid,varid

      integer :: status
      integer :: id

      call check( nf90_create(TRIM(FName),NF90_CLOBBER,ncid))
      call check( nf90_def_dim(ncid,'nVertices',NNodes,dimid(1)))
      !If (IsParallel) &
      !  call check( nf90_def_dim(ncid,'nInterfaces',NInterfaces,dimid(2)))
      call check( nf90_def_dim(ncid,'nBulkElements',NBulkElements,dimid(3)))
      call check( nf90_def_dim(ncid,'Three',three,dimid(4)))
      IF (NBCElements.NE.0) call check( nf90_def_dim(ncid,'nBCElements',NBCElements,dimid(5)))
      call check( nf90_def_dim(ncid,'Two',two,dimid(6)))
      call check( nf90_def_dim(ncid,'time',NF90_UNLIMITED,dimid(7)))


      call check( nf90_def_var(ncid,'Vertices_GlobalDOFs',NF90_INT,dimid(1),varid(1)))
      !If (IsParallel) &
      !  call check( nf90_def_var(ncid,'Vertices_INTERFACES',NF90_INT,dimid(1:2),varid(2)))
      call check( nf90_def_var(ncid,'BulkElement_vertices',NF90_INT,dimid(3:4),varid(3)))
      IF (NBCElements.NE.0) &
       call check( nf90_def_var(ncid,'BCElement_vertices',NF90_INT,dimid(5:6),varid(4)))
      IF (NBCElements.NE.0) &
       call check( nf90_def_var(ncid,'BCElement_Parent',NF90_INT,dimid(5),varid(5)))

      call check( nf90_def_var(ncid,'BulkElement_Area',NF90_DOUBLE,dimid(3),varid(6)))
      IF (NBCElements.NE.0) &
       call check( nf90_def_var(ncid,'BCElement_Area',NF90_DOUBLE,dimid(5),varid(7)))
      call check( nf90_def_var(ncid,'Node_Area',NF90_DOUBLE,dimid(1),varid(8)))

      call check( nf90_def_var(ncid,'x',NF90_DOUBLE,dimid(1),id))
      varid(9)=id
      call check( nf90_put_att(ncid,id,'standard_name',&
                             'projection_x_coordinate'))
      call check( nf90_put_att(ncid,id,'units','m'))

      call check( nf90_def_var(ncid,'y',NF90_DOUBLE,dimid(1),id))
      varid(10)=id
      call check( nf90_put_att(ncid,id,'standard_name',&
                             'projection_x_coordinate'))
      call check( nf90_put_att(ncid,id,'units','m'))
      call check( nf90_def_var(ncid,'time',NF90_DOUBLE,dimid(7),varid(11)))
      call check( nf90_def_var(ncid,'ssa_ux',NF90_DOUBLE,(/dimid(1),dimid(7)/),varid(12)))
      call check( nf90_def_var(ncid,'ssa_uy',NF90_DOUBLE,(/dimid(1),dimid(7)/),varid(13)))
      call check( nf90_def_var(ncid,'h',NF90_DOUBLE,(/dimid(1),dimid(7)/),varid(14)))
      call check( nf90_def_var(ncid,'bedrock',NF90_DOUBLE,dimid(1),varid(15)))
      call check( nf90_def_var(ncid,'zb',NF90_DOUBLE,(/dimid(1),dimid(7)/),varid(16)))
      call check( nf90_def_var(ncid,'zs',NF90_DOUBLE,(/dimid(1),dimid(7)/),varid(17)))
      call check( nf90_def_var(ncid,'basaldrag',NF90_DOUBLE,(/dimid(1),dimid(7)/),varid(18)))

      call check( nf90_def_var(ncid,'mask',NF90_INT,(/dimid(3),dimid(7)/),varid(19)))
      call check( nf90_def_var(ncid,'smbflux',NF90_DOUBLE,(/dimid(3),dimid(7)/),varid(20)))
      call check( nf90_def_var(ncid,'dhdtflux',NF90_DOUBLE,(/dimid(3),dimid(7)/),varid(21)))
      IF (NBCElements.NE.0) & 
        call check( nf90_def_var(ncid,'OutFlow',NF90_DOUBLE,(/dimid(5),dimid(7)/),varid(22)))
      call check( nf90_def_var(ncid,'smbflux_MinH',NF90_DOUBLE,(/dimid(3),dimid(7)/),varid(23)))
      call check( nf90_def_var(ncid,'bmbflux',NF90_DOUBLE,(/dimid(3),dimid(7)/),varid(24)))
      call check( nf90_def_var(ncid,'Flux_nVisit',NF90_INT,dimid(7),varid(25)))
      call check( NF90_ENDDEF(ncid))
      call check( nf90_close(ncid))

      END SUBROUTINE CreatNetcdfFile

      subroutine check(status)
          integer, intent ( in) :: status
          CHARACTER(LEN=MAX_NAME_LEN) :: Message
          if(status /= nf90_noerr) then
            write(message,'(a)') trim(nf90_strerror(status))
            CALL FATAL(SolverName,Message)
          end if
      end subroutine check  
      END
