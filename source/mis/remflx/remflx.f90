!*==remflx.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE remflx(Ngrids)
   USE c_biot
   USE c_gpta1
   USE c_hmatdd
   USE c_hmtout
   USE c_matin
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ngrids
!
! Local variable declarations rewritten by SPAG
!
   REAL :: angle , c , cs , csq , den , det , s , ssq , temp , x2
   INTEGER :: buf2 , buf3 , eltype , estwds , file , i , ict , idx , iflag , igrids , ihc , imid , ipt , irem , isil , ising ,      &
            & isub , itemp , ith , j , jcount , jel , kount , n , n3 , ncol , ncount , nextz , nhit , nrow
   INTEGER , SAVE :: dit , hest , mpt , remfld , scr1
   REAL , DIMENSION(200) :: ecpt
   REAL , DIMENSION(3,3) :: g
   LOGICAL :: hitone
   INTEGER , DIMENSION(32) :: ipoint
   INTEGER , DIMENSION(3,3) :: iwork
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(200) :: necpt
   INTEGER , DIMENSION(6,19) , SAVE :: pointr
   REAL , DIMENSION(3) :: rem
   EXTERNAL close , fwdrec , gopen , hmat , invers , mesage , prehma , rdtrl , read , rewind , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     CHECK FOR REMFLUX IN MAGNETIC FIELD PROBLEMS WHEN COMPUTING
!     PROLATE SPHEROIDAL COEFFICIENTS
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (ecpt(1),necpt(1))
   DATA remfld , hest , mpt , dit , scr1/107 , 108 , 109 , 110 , 301/
   DATA nam/4HREMF , 4HLX  /
!
!                   TYPE  ISIL   MID   ITH NGRIDS ITEMP
!
   DATA pointr/1 , 2 , 4 , 0 , 2 , 17 , 3 , 2 , 4 , 0 , 2 , 16 , 6 , 2 , 6 , 5 , 3 , 27 , 9 , 2 , 6 , 5 , 3 , 21 , 10 , 2 , 4 , 0 , &
      & 2 , 17 , 16 , 2 , 7 , 6 , 4 , 26 , 17 , 2 , 6 , 5 , 3 , 21 , 18 , 2 , 7 , 6 , 4 , 26 , 19 , 2 , 7 , 6 , 4 , 32 , 34 , 2 ,   &
      & 16 , 0 , 2 , 42 , 36 , 2 , 6 , 5 , 3 , 19 , 37 , 2 , 7 , 6 , 4 , 24 , 39 , 3 , 2 , 0 , 4 , 23 , 40 , 3 , 2 , 0 , 6 , 33 ,   &
      & 41 , 3 , 2 , 0 , 8 , 43 , 42 , 3 , 2 , 0 , 8 , 43 , 65 , 2 , 10 , 0 , 8 , 48 , 66 , 2 , 22 , 0 , 20 , 108 , 67 , 2 , 34 ,   &
      & 0 , 32 , 168/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         remfl = .FALSE.
         mcb(1) = remfld
         CALL rdtrl(mcb)
!
!     CHECK FOR ANY REMFLUX
!
         IF ( mcb(6)==0 ) RETURN
!
!     TOO BAD
!
         remfl = .TRUE.
         ncol = mcb(2)
         nrow = mcb(3)
         ncount = nrow/3
!
!     BRING IN MATERIALS SINCE H=B/MU
!
         iihmat = Ngrids
         nnhmat = lcore
         mptfil = mpt
         ditfil = dit
         CALL prehma(z)
         nextz = nnhmat + 1
!
         buf2 = buf1 - ibuf
         buf3 = buf2 - ibuf
!
!     SET UP POINTERS
!     IHC = START OF RESULTS HC = B/MU
!     IREM= REMFL COLUMN
!     ICT = COUNTER FOR NUMBER OF ELEMENTS AT EACH PROLATE GRID (FOR
!     AVERAGING
!
         ihc = nextz
         irem = ihc + 3*Ngrids
         ict = irem + nrow
         IF ( buf3<ict+Ngrids ) THEN
            n = -8
            file = 0
            CALL mesage(n,file,nam)
            CALL mesage(-61,0,0)
            RETURN
         ELSE
!
            CALL gopen(scr1,z(buf1),1)
            CALL gopen(remfld,z(buf2),0)
            CALL gopen(hest,z(buf3),0)
!
            ii = 1
            nn = nrow
            jncr = 1
            jout = 1
            jcount = 0
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         DO i = 1 , Ngrids
            iz(ict+i) = 0
         ENDDO
         n3 = 3*Ngrids
         DO i = 1 , n3
            z(ihc+i) = 0.
         ENDDO
!
!     UNPACK A COULMN OF REMFLD
!
         jcount = jcount + 1
         CALL unpack(*20,remfld,z(irem+1))
!
!     SINCE THE ELEMENT DATA DO NOT CHANGE WITH REMFLD COLIMN, THIS IS
!     NOT NECESSARILY THE BEST KIND OF LOOPING. BUT OTHER WAYS WOULD
!     NEED MORE CORE AND IF THERE IS MORE THAN ONE REMFLUX CASE, IT
!     WOULD BE A SURPRISE
!
         file = hest
         kount = 0
         GOTO 40
!
!     ZERO COLUMN
!
 20      DO i = 1 , n3
            z(ihc+i) = 0.
         ENDDO
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      CALL read(*60,*100,hest,eltype,1,0,iflag)
         idx = (eltype-1)*incr
         estwds = ne(idx+12)
!
!     PICK UP ELEMENT TYPE INFO
!
         SPAG_Loop_1_1: DO i = 1 , 19
            jel = i
            IF ( eltype<pointr(1,i) ) EXIT SPAG_Loop_1_1
            IF ( eltype==pointr(1,i) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
         WRITE (iout,99001) ufm
99001    FORMAT (A23,', ILLEGAL ELEMENT TYPE IN REMFLX')
         CALL mesage(-61,0,0)
         RETURN
      CASE (3)
!
         isil = pointr(2,jel)
         imid = pointr(3,jel)
         ith = pointr(4,jel)
         igrids = pointr(5,jel)
         itemp = pointr(6,jel)
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_2: DO
!
            CALL read(*80,*40,hest,ecpt,estwds,0,iflag)
!
!     PICK UP REMFLUX FOR THIS ELEMENT
!
            kount = kount + 1
            nhit = 0
            hitone = .FALSE.
            DO i = 1 , igrids
               ipoint(i) = 0
            ENDDO
            DO i = 1 , Ngrids
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     SPAG_Loop_5_1: DO j = 1 , igrids
                        ipt = necpt(isil+j-1)
                        IF ( ipt==iz(i) ) THEN
                           spag_nextblock_2 = 2
                           EXIT SPAG_Loop_5_1
                        ENDIF
                     ENDDO SPAG_Loop_5_1
                  CASE (2)
!
!     MATCH
!
                     hitone = .TRUE.
                     nhit = nhit + 1
                     iz(ict+i) = iz(ict+i) + 1
                     ipoint(j) = i
                     IF ( nhit==igrids ) EXIT SPAG_Loop_1_2
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            IF ( hitone ) EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
!
         isub = irem + 3*(kount-1)
         rem(1) = z(isub+1)
         rem(2) = z(isub+2)
         rem(3) = z(isub+3)
!
!     PICK UP MATERIALS
!
         matid = necpt(imid)
         eltemp = ecpt(itemp)
         inflag = 3
         sinth = 0.
         costh = 0.
         CALL hmat(necpt(1))
         g(1,1) = xmat(1)
         g(1,2) = xmat(2)
         g(1,3) = xmat(3)
         g(2,1) = xmat(2)
         g(2,2) = xmat(4)
         g(2,3) = xmat(5)
         g(3,1) = xmat(3)
         g(3,2) = xmat(5)
         g(3,3) = xmat(6)
!
!     FOR COMMENTS ON MATERIALS SEE EM2D
!
         IF ( ith/=0 ) THEN
            angle = ecpt(ith)*0.017453293
            IF ( xmat(3)==0. .AND. xmat(5)==0. ) THEN
               IF ( abs(angle)>.0001 ) THEN
                  s = sin(angle)
                  c = cos(angle)
                  csq = c*c
                  ssq = s*s
                  cs = c*s
                  x2 = 2.*cs*xmat(2)
                  g(1,1) = csq*xmat(1) - x2 + ssq*xmat(4)
                  g(1,2) = cs*(xmat(1)-xmat(4)) + (csq-ssq)*xmat(2)
                  g(2,2) = ssq*xmat(1) + x2 + csq*xmat(4)
                  g(2,1) = g(1,2)
                  g(3,3) = xmat(6)
                  g(1,3) = 0.
                  g(2,3) = 0.
                  g(3,1) = 0.
                  g(3,2) = 0.
!
!     SINCE MAT5 INFO FOR TRAPRG,TRIARG IS GIVEN IN X-Y ORDER,
!     INETRCHANGE YA AND Z
!
                  temp = g(2,2)
                  g(2,2) = g(3,3)
                  g(3,3) = temp
                  temp = g(1,2)
                  g(1,2) = g(1,3)
                  g(1,3) = temp
                  g(2,1) = g(1,2)
                  g(3,1) = g(1,3)
               ENDIF
            ENDIF
         ENDIF
!
!     SOLVE MU*H = B
!
         CALL invers(3,g,3,rem,1,det,ising,iwork)
         IF ( ising==2 ) THEN
            WRITE (iout,99002) ufm , matid
99002       FORMAT (A23,', MATERIAL',I9,' IS SINGULAR IN REMFLX')
            CALL mesage(-61,0,0)
            RETURN
         ELSE
!
!     REM NOW HAS HC- CHECK POINTER LIST TO SEE WHICH GRIDS ARE ON THE
!     SPHEROID AND ADD REMFLUX T THOSE ALREADY ACCUMULATED
!
            DO i = 1 , igrids
               IF ( ipoint(i)/=0 ) THEN
                  isub = ihc + 3*(ipoint(i)-1)
                  DO j = 1 , 3
                     z(isub+j) = z(isub+j) + rem(j)
                  ENDDO
               ENDIF
!
!     GO BACK FOR ANOTHER ELEMEENT
!
            ENDDO
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     DONE WITH ALL TYPES-AVERAGE THE RESULTS BY NUMBER OF ELEMENTS AT
!     EACH
!
 60      DO i = 1 , Ngrids
            den = float(iz(ict+i))
            IF ( den/=0. ) THEN
               isub = 3*(i-1) + ihc
               DO j = 1 , 3
                  z(isub+j) = z(isub+j)/den
               ENDDO
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
!
!     WRITE RESULTS TO SCR1
!
         CALL write(scr1,z(ihc+1),3*Ngrids,1)
!
!     GO BACK FOR ANOTHER REMFLD RECORD
!
         IF ( jcount==ncol ) THEN
!
!     DONE
!
            CALL close(scr1,1)
            mcb(1) = scr1
            mcb(2) = ncol
            mcb(3) = 3*Ngrids
            DO i = 4 , 7
               mcb(i) = 0
            ENDDO
            CALL wrttrl(mcb)
            CALL close(hest,1)
            CALL close(remfld,1)
            RETURN
         ELSE
            CALL rewind(hest)
            CALL fwdrec(*80,hest)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
 80      n = -2
         CALL mesage(n,file,nam)
         CALL mesage(-61,0,0)
         RETURN
 100     n = -3
         CALL mesage(n,file,nam)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE remflx
