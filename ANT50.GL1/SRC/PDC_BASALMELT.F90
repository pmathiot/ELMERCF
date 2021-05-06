! Pollard, D., DeConto, R.M., 2012. Description of a hybrid ice
! sheet-shelf model, and application to Antarctica. Geoscientific Model
! Development 5, 1273–1295. doi:10.5194/gmd-5-1273-2012
!
! Pas de modification en fn de l'angle d'open ocean (Eq.18)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Version Modifiée:
!  => MODIF DES DESCRIPTION DES AIRES
!  => K=2 pour EA
       FUNCTION PDC_BASALMELT_M(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn(4),VarOut
       REAL(kind=dp) :: LonLat(2),zb,mask
       REAL(kind=dp) :: depth,Tf,T0,K,dt
       REAL(kind=dp) :: melt
       INTEGER :: area
       INTEGER :: PDC_AREA
       LOGICAL :: Found
       REAL(kind=dp) :: sealevel,rw,cw,Lf,ri
       !Transfer coefficient for sub-ice oceanic melting (15.77ma−1 K−1)
       REAL(kind=dp),parameter :: Kt=15.77
       mask=VarIn(4)

       IF (mask.GT.0.5) THEN 
          VarOut=0._dp
       ELSE
         sealevel = GetCReal( Model % Constants, 'Sea Level', Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<Sea Level> not found')
         rw=GetCReal( Model % Constants, 'water density', Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<water density> not found')
         cw=GetCReal( Model % Constants, 'Sea Water Specific heat',Found )
         IF (.NOT.Found) &
           CALL FATAL('ABMB_To_BM','<Sea Water Specific heat> not found')
         Lf=GetCReal( Model % Constants, 'Ice fusion latent heat',Found )
         IF (.NOT.Found) &
           CALL FATAL('ABMB_To_BM','<Ice fusion latent heat> not found')
         ri=GetCReal( Model % Constants, 'Ice density',Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<Ice density> not found')

         zb=VarIn(3)
         depth=sealevel-zb
         if (depth.lt.0._dp) THEN 
           VarOut=0._dp
           Return
         End if

         !# Sea water pressure melting point
         Tf=0.0939-0.057*34.5-0.000764*depth

         LonLat=VarIn(1:2)
         area=PDC_AREA(LonLat)
         SELECT CASE(area)
          CASE(1)
           IF (depth.LT.170.0) then
              dt=0.5
           Else IF (depth.GT.680.0) then
              dt=3.5
           Else
              dt=0.5+(depth-170.0)*3.0/510.0
           Endif
           K=8
          CASE(2)
           T0=-0.8
           dt=T0-Tf
           K=1
        !! Modif East Ant.   
          CASE(3)
           IF (depth.LT.170.0) then
              dt=0.5
           Else IF (depth.GT.680.0) then
              dt=3.5
           Else
              dt=0.5+(depth-170.0)*3.0/510.0
           Endif
           K=2
          !!
          CASE(4)
           T0=-1.5
           dt=T0-Tf
           K=1
          CASE DEFAULT
            CALL FATAL('PDC_BASALMELT','Area undefined')
         END SELECT

         melt=K*Kt*rw*cw*abs(dt)*dt/(ri*Lf)

         VarOut=melt
        END IF
       End FUNCTION PDC_BASALMELT_M
!##############
!!!!!!!!!!!!!!!!!!! idem mais pas de fonte a la GL
       FUNCTION PDC_BASALMELT_M2(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn(4),VarOut
       REAL(kind=dp) :: LonLat(2),zb,mask
       REAL(kind=dp) :: depth,Tf,T0,K,dt
       REAL(kind=dp) :: melt
       INTEGER :: area
       INTEGER :: PDC_AREA
       LOGICAL :: Found
       REAL(kind=dp) :: sealevel,rw,cw,Lf,ri
       !Transfer coefficient for sub-ice oceanic melting (15.77ma−1 K−1)
       REAL(kind=dp),parameter :: Kt=15.77
       mask=VarIn(4)

       IF (mask.GT.-0.5) THEN 
          VarOut=0._dp
       ELSE
         sealevel = GetCReal( Model % Constants, 'Sea Level', Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<Sea Level> not found')
         rw=GetCReal( Model % Constants, 'water density', Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<water density> not found')
         cw=GetCReal( Model % Constants, 'Sea Water Specific heat',Found )
         IF (.NOT.Found) &
           CALL FATAL('ABMB_To_BM','<Sea Water Specific heat> not found')
         Lf=GetCReal( Model % Constants, 'Ice fusion latent heat',Found )
         IF (.NOT.Found) &
           CALL FATAL('ABMB_To_BM','<Ice fusion latent heat> not found')
         ri=GetCReal( Model % Constants, 'Ice density',Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<Ice density> not found')

         zb=VarIn(3)
         depth=sealevel-zb
         if (depth.lt.0._dp) THEN 
           VarOut=0._dp
           Return
         End if

         !# Sea water pressure melting point
         Tf=0.0939-0.057*34.5-0.000764*depth

         LonLat=VarIn(1:2)
         area=PDC_AREA(LonLat)
         SELECT CASE(area)
          CASE(1)
           IF (depth.LT.170.0) then
              dt=0.5
           Else IF (depth.GT.680.0) then
              dt=3.5
           Else
              dt=0.5+(depth-170.0)*3.0/510.0
           Endif
           K=8
          CASE(2)
           T0=-0.8
           dt=T0-Tf
           K=1
        !! Modif East Ant.   
          CASE(3)
           IF (depth.LT.170.0) then
              dt=0.5
           Else IF (depth.GT.680.0) then
              dt=3.5
           Else
              dt=0.5+(depth-170.0)*3.0/510.0
           Endif
           K=2
          !!
          CASE(4)
           T0=-1.5
           dt=T0-Tf
           K=1
          CASE DEFAULT
            CALL FATAL('PDC_BASALMELT','Area undefined')
         END SELECT

         melt=K*Kt*rw*cw*abs(dt)*dt/(ri*Lf)

         VarOut=melt
        END IF
       End FUNCTION PDC_BASALMELT_M2
!###################################################################
!###################################################################
!###################################################################
!###################################################################
!###################################################################
      !! Version originale
       FUNCTION PDC_BASALMELT(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn(4),VarOut
       REAL(kind=dp) :: LonLat(2),zb,mask
       REAL(kind=dp) :: depth,Tf,T0,K,dt
       REAL(kind=dp) :: melt
       INTEGER :: area
       INTEGER :: PDC_AREA
       LOGICAL :: Found
       REAL(kind=dp) :: sealevel,rw,cw,Lf,ri
       !Transfer coefficient for sub-ice oceanic melting (15.77ma−1 K−1)
       REAL(kind=dp),parameter :: Kt=15.77
       mask=VarIn(4)

       IF (mask.GT.0.5) THEN 
          VarOut=0._dp
       ELSE
         sealevel = GetCReal( Model % Constants, 'Sea Level', Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<Sea Level> not found')
         rw=GetCReal( Model % Constants, 'water density', Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<water density> not found')
         cw=GetCReal( Model % Constants, 'Sea Water Specific heat',Found )
         IF (.NOT.Found) &
           CALL FATAL('ABMB_To_BM','<Sea Water Specific heat> not found')
         Lf=GetCReal( Model % Constants, 'Ice fusion latent heat',Found )
         IF (.NOT.Found) &
           CALL FATAL('ABMB_To_BM','<Ice fusion latent heat> not found')
         ri=GetCReal( Model % Constants, 'Ice density',Found )
         IF (.NOT.Found) &
             CALL FATAL('ABMB_To_BM','<Ice density> not found')

         zb=VarIn(3)
         depth=sealevel-zb
         if (depth.lt.0._dp) THEN 
           VarOut=0._dp
           Return
         End if

         !# Sea water pressure melting point
         Tf=0.0939-0.057*34.5-0.000764*depth

         LonLat=VarIn(1:2)
         area=PDC_AREA(LonLat)
         SELECT CASE(area)
          CASE(1,3)
           IF (depth.LT.170.0) then
              dt=0.5
           Else IF (depth.GT.680.0) then
              dt=3.5
           Else
              dt=0.5+(depth-170.0)*3.0/510.0
           Endif
           K=8
          CASE(2)
           T0=-0.8
           dt=T0-Tf
           K=1
          CASE(4)
           T0=-1.5
           dt=T0-Tf
           K=1
          CASE DEFAULT
            CALL FATAL('PDC_BASALMELT','Area undefined')
         END SELECT

         melt=K*Kt*rw*cw*abs(dt)*dt/(ri*Lf)

         VarOut=melt
        END IF
       End FUNCTION PDC_BASALMELT
       
       FUNCTION PDC_BASALMELT_AREA(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn(2),VarOut
       REAL(kind=dp) :: LonLat(2)
       INTEGER :: area
       INTEGER :: PDC_AREA
       LOGICAL :: Found

       LonLat=VarIn(1:2)
       area=PDC_AREA(LonLat)
       VarOut=area

       End FUNCTION PDC_BASALMELT_AREA

       FUNCTION PDC_AREA(VarIn) RESULT(area)
       USE DefUtils
       implicit none
       REAL(kind=dp) :: VarIn(2)
       INTEGER :: area
     ! Amundsen and Bellingshausen Seas, and Western Peninsula
       !! => Dernier cas modifié: -90 à -66 (au lieu de -90 a -65) sinon
       !mange une partie larsen
       IF (((VarIn(1).GE.-140.0).AND.(VarIn(1).LE.-120.0).AND.(VarIn(2).GT.-77.0))&
      .OR.((VarIn(1).GE.-120.0).AND.(VarIn(1).LE.-90.0).AND.(VarIn(2).GT.-85.0))&
      .OR.((VarIn(1).GE.-90.0).AND.(VarIn(1).LE.-66.0).AND.(VarIn(2).GT.-75.0))) THEN
         area=1
     ! Weddell embayment:
       !! => Dernier cas modifié: GE -66 (au lieu de -65)
       ELSE IF (((VarIn(1).GE.-120.0).AND.(VarIn(1).LE.-90.0).AND.(VarIn(2).LT.-85.0))&
      .OR.((VarIn(1).GE.-90.0).AND.(VarIn(1).LE.-65.0).AND.(VarIn(2).LT.-75.0))&
      .OR.((VarIn(1).GE.-66.0).AND.(VarIn(1).LE.-10.0))) THEN
        area=2
     ! East Antarctica:
       ELSE IF ((VarIn(1).GE.-10.0).AND.(VarIn(1).LE.160.0)) THEN
        area=3
     ! Ross embayment:
       ELSE IF (((VarIn(1).GE.160.0).AND.(VarIn(1).LE.180.0))&
      .OR.((VarIn(1).GE.-180.0).AND.(VarIn(1).LE.-140.0))&
      .OR.((VarIn(1).GE.-140.0).AND.(VarIn(1).LE.-120.0).AND.(VarIn(2).LT.-77.0))) THEN
        area=4
       ELSE
        CALL FATAL('PDC_AREA','Point not in defined area')
       ENDIF
       END FUNCTION PDC_AREA
!########
       FUNCTION AREA1(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn,VarOut
       INTEGER,parameter :: Area=1
       REAL(kind=dp),parameter :: Epsi=1.0e-2
       !  
       IF (abs(VarIn-Area).LT.Epsi) THEN
         VarOut=1._dp
       Else
         VarOut=0._dp
       END IF
       End FUNCTION AREA1
       FUNCTION AREA2(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn,VarOut
       INTEGER,parameter :: Area=2
       REAL(kind=dp),parameter :: Epsi=1.0e-2
       !  
       IF (abs(VarIn-Area).LT.Epsi) THEN
         VarOut=1._dp
       Else
         VarOut=0._dp
       END IF
       End FUNCTION AREA2
       FUNCTION AREA3(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn,VarOut
       INTEGER,parameter :: Area=3
       REAL(kind=dp),parameter :: Epsi=1.0e-2
       !  
       IF (abs(VarIn-Area).LT.Epsi) THEN
         VarOut=1._dp
       Else
         VarOut=0._dp
       END IF
       End FUNCTION AREA3
       FUNCTION AREA4(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp) :: VarIn,VarOut
       INTEGER,parameter :: Area=4
       REAL(kind=dp),parameter :: Epsi=1.0e-2
       !  
       IF (abs(VarIn-Area).LT.Epsi) THEN
         VarOut=1._dp
       Else
         VarOut=0._dp
       END IF
       End FUNCTION AREA4
