!*==getdef.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE getdef(Dfrm,Ph,Mag,Conv,Plttyp,Buf,Gpt,D)
   IMPLICIT NONE
   USE C_BLANK
   USE C_XXPARM
   USE C_ZNTPKX
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
         Last = 0
         k = 3*Ngpset
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
         Maxdef = 0.
         CALL intpk(*99999,Dfrm,0,sp,0)
         gp = 0
         Siln = 0
         CALL gopen(Msil,Buf(1),inprew)
         CALL fread(Msil,sil2,1,0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     -GP- = PREVIOUS EXISTENT GRID POINT IN THIS SET. FIND NEXT ONE.
!
         k = gp + 1
         DO gpx = k , Ngp
            IF ( Gpt(gpx)/=0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         sil1 = Lsil + 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         DO WHILE ( gpx/=gp+1 )
            gp = gp + 1
            CALL fread(Msil,sil2,1,0)
         ENDDO
         sil1 = sil2
!
!     -SIL1- = SIL NUMBER OF NEXT EXISTENT GRID POINT. READ SIL NUMBER
!              OF NEXT GRID POINT.
!
         gp = gpx
         gpx = iabs(Gpt(gp))
         IF ( gp==Ngp ) sil2 = Lsil + 1
         IF ( gp/=Ngp ) CALL fread(Msil,sil2,1,0)
         spag_nextblock_1 = 4
      CASE (4)
!
!     READ NEXT DEFORMATION VALUE AT THIS EXISTING GRID POINT.
!
         IF ( Siln<=Lsil .AND. Siln>=sil1 ) THEN
            IF ( Siln>sil1+2 .OR. Siln>=sil2 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = Siln - sil1 + 1
            D(k,gpx) = defval
            IF ( Last==0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
            CALL close(Msil,rew)
            RETURN
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Last/=0 ) THEN
            CALL close(Msil,rew)
            RETURN
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         CALL zntpki
         GOTO type
 20      defval = Defc(i1)*cn - Defc(i2)*sn
         GOTO 60
 40      defval = Conv*sqrt(Defc(1)**2+Defc(sp-1)**2)
 60      IF ( abs(defval)>Maxdef ) Maxdef = abs(defval)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE getdef
