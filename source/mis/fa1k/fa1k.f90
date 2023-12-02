!*==fa1k.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa1k(Imeth,K,Rho,Outfil,Ico)
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Imeth
   REAL :: K
   REAL :: Rho
   INTEGER :: Outfil
   INTEGER :: Ico
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buff , buff1 , i , idp , ifil , ig , iip , ij , ik , ilop , j , ji , jj , n , nc , ncm , ncol , ncore , nd , nf , ni ,&
            & ni2 , nip , nogo , nrd , nrho , nwc , nwr , type
   REAL :: cmach , eps , ok , omach , temp
   INTEGER , SAVE :: fsave , qhhl , scr2 , scr3 , scr4
   INTEGER , DIMENSION(7) :: mcb , trl
   LOGICAL :: new
   INTEGER , DIMENSION(2) , SAVE :: ns
   REAL , DIMENSION(1) :: z
   EXTERNAL bckrec , close , fwdrec , gopen , korsz , mesage , mintrp , open , pack , rdtrl , read , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     FA1K BUILDS AN INTERPOLATED MATRIX ON OUTFIL FROM QHHL OR FSAVE
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA fsave/201/ , qhhl/104/ , scr2 , scr3 , scr4/302 , 303 , 304/
   DATA ns/4HFA1K , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ncore = korsz(Iz) - Ico
         buff = ncore - Sysbuf
         buff1 = buff - Sysbuf
         trl(1) = fsave
         CALL rdtrl(trl)
!
!     READ IN DEPENDENT POINTS AND SET K AND RHO
!
         jj = trl(3)*3
         ifil = fsave
         CALL gopen(fsave,Iz(buff+1),0)
         CALL read(*120,*20,fsave,z,jj,1,nwr)
 20      i = (Floop-1)*3 + 1
         cmach = z(i)
         K = z(i+1)
         Rho = z(i+2)
         Incr1 = 1
         Incr = 1
         Ii = 1
         Inn = 1
         IF ( Imeth==2 ) THEN
!
!     LINEAR SPLINE INTERPOLATION
!
!
!     IS A GOOD MATRIZ ON FSAVE
!
            eps = .001
            new = .TRUE.
            ni = trl(4)
            IF ( Floop==1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ok = z(i-2)
            omach = z(i-3)
            IF ( abs(cmach-omach)<eps ) new = .FALSE.
!
!     REWRITE QHHL IF NEW IS TRUE
!
            IF ( .NOT.new ) THEN
               IF ( ok-K/=0.0 ) trl(7) = trl(7) + 1
               ij = trl(7) + 1
               CALL wrttrl(trl)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( Floop/=1 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     SURFACE SPLINE INTERPOLATION
!
         ELSEIF ( Floop/=1 ) THEN
!
!     GET A COLUMN FROM FSAVE AND BUILD QHH ON OUTFIL
!
            nf = 2 + (Floop-1)/trl(7)
            DO i = 1 , nf
               CALL fwdrec(*120,fsave)
            ENDDO
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SET UP CALL TO SPLINE INTERPOLATOR
!
            nrho = trl(7)
            ji = nrho*3
            j = 1
            DO i = 1 , jj , ji
               z(j) = z(i)
               z(j+1) = z(i+1)
               j = j + 2
            ENDDO
            nd = jj/ji
            ni = trl(4)
            type = 1
            idp = 1
            iip = nd*2 + idp
            ig = iip + 2*ni
            ni2 = ni*2
            CALL read(*120,*40,fsave,z(iip),ni2,1,nwr)
         ENDIF
 40      CALL fwdrec(*120,fsave)
!
!     REWRITE QHHL SO EACH LIST MATRIX IS A COLUMN
!
         CALL close(fsave,2)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         Iout = trl(6)
         Iti = Iout
         Ito = Iout
         nwc = 1
         IF ( Ito==2 .OR. Ito==3 ) nwc = 2
         IF ( Ito==4 ) nwc = 4
         mcb(1) = qhhl
         CALL rdtrl(mcb)
         nc = mcb(3)
         Nn = nc
         Nnn = nc*nc
         CALL unpack(*100,fsave,z)
         ij = 1
         CALL close(fsave,1)
         CALL gopen(Outfil,Iz(buff+1),1)
         mcb(1) = Outfil
         mcb(2) = 0
         mcb(3) = nc
         mcb(4) = 1
         mcb(5) = Iout
         mcb(6) = 0
         mcb(7) = 0
         DO i = 1 , nc
            CALL pack(z(ij),Outfil,mcb)
            ij = ij + nc*nwc
         ENDDO
         CALL close(Outfil,1)
         CALL wrttrl(mcb)
         RETURN
      CASE (3)
!
!     TEST TO SEE IF QHHL HAS ENOUGH MACH NUMBERS
!
         nip = ni*2
         nogo = 0
         iip = jj + 1
         CALL read(*120,*60,fsave,z(iip),nip,1,nwr)
 60      CALL bckrec(fsave)
         temp = 0.0
         DO i = 1 , jj , 3
            IF ( temp/=z(i) ) THEN
               temp = z(i)
               nf = 0
               DO j = 1 , nip , 2
                  IF ( temp-z(iip+j-1)<eps ) nf = nf + 1
               ENDDO
               IF ( nf<=1 ) THEN
                  WRITE (Out,99001) Ufm , temp
!
!     ERROR MESSAGES
!
99001             FORMAT (A23,' 2270, LINEAR INTERPOLATION WITHOUT ENOUGH IND. ','MACH NUMBERS EQUAL TO DEP. MACH ',F10.4)
                  nogo = 1
               ENDIF
            ENDIF
         ENDDO
         IF ( nogo==1 ) GOTO 100
         spag_nextblock_1 = 4
      CASE (4)
         j = 1
         nrd = 0
         DO i = 1 , jj , 3
            IF ( abs(cmach-z(i))<eps ) THEN
               IF ( z(i+2)==Rho ) THEN
                  z(j) = z(i)
                  z(j+1) = z(i+1)
                  j = j + 2
                  nrd = nrd + 1
               ENDIF
            ENDIF
         ENDDO
         idp = 1
         iip = nrd*2 + idp
         ni2 = ni*2
         CALL read(*120,*80,fsave,z(iip),ni2,1,nwr)
 80      CALL fwdrec(*120,fsave)
         CALL close(fsave,2)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         IF ( abs(cmach-z(iip+i-1))<eps ) THEN
            z(iip+ik) = z(iip+i)
            ik = ik + 2
            nf = nf + 1
            ji = ig
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SKIP MATRIX
!
            DO j = 1 , ncm
               CALL fwdrec(*120,qhhl)
            ENDDO
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         i = i + 2
         IF ( i/=jj ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(qhhl,1)
         CALL close(Outfil,1)
         CALL wrttrl(trl)
!
!     SET UP CALL TO SPLINE INTERPOLATION
!
         type = -1
         nd = nrd
         ni = nf
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         DO i = 1 , ij
            CALL fwdrec(*120,fsave)
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!     CALL MINTRP
!
         ig = iip + 2*ni
         nc = ncore - ig
         nogo = 0
         CALL mintrp(ni,z(iip),nd,z(idp),type,0,0,0.0,Outfil,scr2,scr3,scr4,z(ig),nc,nogo,Iprec)
         IF ( nogo==1 ) GOTO 100
!
!     INTERPOLATED MATRIX IS ON SCR2 MOVE TO FSAVE
!
         CALL open(*120,fsave,Iz(buff+1),3)
         CALL gopen(scr2,Iz(buff1+1),0)
         trl(1) = scr2
         CALL rdtrl(trl)
         ncol = trl(2)
         Nn = trl(3)
         Nnn = Nn
         Iti = trl(5)
         Ito = Iti
         Iout = Iti
         trl(1) = fsave
         trl(2) = 0
         trl(6) = 0
         trl(7) = 0
         i = 1
         DO
            CALL unpack(*100,scr2,z)
            CALL pack(z,fsave,trl)
            IF ( i==ncol ) THEN
               CALL close(scr2,1)
               CALL close(fsave,1)
               CALL rdtrl(trl)
               trl(6) = Ito
               IF ( Imeth==2 ) trl(7) = 1
               CALL wrttrl(trl)
!
!     GET COLUMN FROM FSAVE AND BUILD QHH
!
               CALL gopen(fsave,Iz(buff+1),0)
               ij = 3
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               i = i + 1
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
      CASE (9)
!
!     SET UP COLUMN - MATRIX COPY
!
         CALL gopen(qhhl,Iz(buff+1),0)
         trl(1) = qhhl
         CALL rdtrl(trl)
         ncol = trl(2)/trl(3)
         ncm = trl(3)
         CALL gopen(Outfil,Iz(buff1+1),1)
         Nnn = ncm
         Nn = ncm*ncm
         Iti = trl(5)
         Ito = Iti
         Iout = Iti
         nwc = 1
         IF ( Ito==2 .OR. Ito==3 ) nwc = 2
         IF ( Ito==4 ) nwc = 4
         trl(1) = Outfil
         trl(2) = 0
         trl(3) = Nn
         trl(6) = 0
         trl(7) = 0
         IF ( Imeth==1 ) THEN
            j = 1
            ji = ig
         ELSEIF ( Imeth==2 ) THEN
            ig = iip + ni*2
            nf = 0
            ik = 1
            ifil = qhhl
            jj = 2*ni + 1
            i = 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         SPAG_Loop_1_1: DO
!
!     MAKE A COLUMN INTO MATRIX
!
            DO ilop = 1 , ncm
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     CALL unpack(*82,qhhl,z(ji))
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
 82                  n = ncm*nwc
                     DO ij = 1 , n
                        z(ji+ij-1) = 0.0
                     ENDDO
                     spag_nextblock_2 = 2
                  CASE (2)
                     ji = ji + ncm*nwc
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            CALL pack(z(ig),Outfil,trl)
            IF ( Imeth==1 ) THEN
               IF ( j==ncol ) THEN
                  CALL close(qhhl,1)
                  CALL close(Outfil,1)
                  CALL wrttrl(trl)
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  j = j + 1
                  ji = ig
               ENDIF
            ELSEIF ( Imeth==2 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
 100     WRITE (Out,99002) Ufm
99002    FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 120     CALL mesage(-3,ifil,ns)
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(-61,0,ns)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fa1k
