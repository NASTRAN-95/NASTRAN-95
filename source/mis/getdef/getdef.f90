!*==getdef.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE getdef(Dfrm,Ph,Mag,Conv,Plttyp,Buf,Gpt,D)
   USE c_blank
   USE c_xxparm
   USE c_zntpkx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Dfrm
   REAL :: Ph
   INTEGER :: Mag
   REAL :: Conv
   INTEGER :: Plttyp
   INTEGER , DIMENSION(1) :: Buf
   INTEGER , DIMENSION(1) :: Gpt
   REAL , DIMENSION(3,1) :: D
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cn , defval , sn
   INTEGER :: gp , gpx , i , i1 , i2 , k , sil1 , sil2 , sp , type
   INTEGER , SAVE :: inprew , rew
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , fread , gopen , intpk , rdtrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Defval,Defc(1))
   DATA inprew , rew/0 , 1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         last = 0
         k = 3*ngpset
         DO i = 1 , k
            D(i,1) = 0.0
         ENDDO
         trl(1) = Dfrm
         CALL rdtrl(trl(1))
         IF ( trl(5)<=0 ) RETURN
         sp = trl(5)
         ASSIGN 60 TO type
!
!     NOTE TRANSIENT RESPONSE HAS SP = 1
!
         IF ( sp>=3 ) THEN
            ASSIGN 40 TO type
            IF ( Mag==0 ) THEN
               ASSIGN 20 TO type
               sn = sin(Ph)*Conv
               cn = cos(Ph)*Conv
               IF ( Plttyp==2 ) THEN
!
!     VELOCITY
!
                  i1 = sp - 1
                  i2 = 1
               ELSE
!
!     DISPLACEMENT OR ACCELERATION
!
                  i1 = 1
                  i2 = sp - 1
                  IF ( Plttyp==3 .OR. Plttyp==4 ) cn = -cn
               ENDIF
            ENDIF
         ENDIF
         maxdef = 0.
         CALL intpk(*99999,Dfrm,0,sp,0)
         gp = 0
         siln = 0
         CALL gopen(msil,Buf(1),inprew)
         CALL fread(msil,sil2,1,0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     -GP- = PREVIOUS EXISTENT GRID POINT IN THIS SET. FIND NEXT ONE.
!
         k = gp + 1
         DO gpx = k , ngp
            IF ( Gpt(gpx)/=0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         sil1 = lsil + 1
         spag_nextblock_1 = 5
      CASE (3)
         DO WHILE ( gpx/=gp+1 )
            gp = gp + 1
            CALL fread(msil,sil2,1,0)
         ENDDO
         sil1 = sil2
!
!     -SIL1- = SIL NUMBER OF NEXT EXISTENT GRID POINT. READ SIL NUMBER
!              OF NEXT GRID POINT.
!
         gp = gpx
         gpx = iabs(Gpt(gp))
         IF ( gp==ngp ) sil2 = lsil + 1
         IF ( gp/=ngp ) CALL fread(msil,sil2,1,0)
         spag_nextblock_1 = 4
      CASE (4)
!
!     READ NEXT DEFORMATION VALUE AT THIS EXISTING GRID POINT.
!
         IF ( siln<=lsil .AND. siln>=sil1 ) THEN
            IF ( siln>sil1+2 .OR. siln>=sil2 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = siln - sil1 + 1
            D(k,gpx) = defval
            IF ( last==0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
            CALL close(msil,rew)
            RETURN
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( last/=0 ) THEN
            CALL close(msil,rew)
            RETURN
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         CALL zntpki
         GOTO type
 20      defval = defc(i1)*cn - defc(i2)*sn
         GOTO 60
 40      defval = Conv*sqrt(defc(1)**2+defc(sp-1)**2)
 60      IF ( abs(defval)>maxdef ) maxdef = abs(defval)
         spag_nextblock_1 = 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE getdef
