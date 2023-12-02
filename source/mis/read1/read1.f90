!*==read1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE read1(Dm,Mr,Scr1,Scr2,Scr3,Phia,Uset,Nr1,Lama,Scr4)
USE C_BITPOS
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Dm
   INTEGER :: Mr
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Phia
   REAL :: Uset
   INTEGER :: Nr1
   INTEGER :: Lama
   INTEGER :: Scr4
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore
   INTEGER :: i , i3 , ialph , iloop , iphi , ivi , ivi2 , j , k , kk , kkk , l , lc , nprob , nr , nr2
   INTEGER , DIMENSION(7) :: imr , iscr1
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL(REAL64) :: si , term
   REAL :: ssi , sterm
   EXTERNAL close , gopen , korsz , makmcb , mesage , pack , rdtrl , sdr1b , ssg2b , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (Dcore(1),Core(1))
   DATA nam/4HREAD , 4H1   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     BRING MR INTO CORE
!
         lc = korsz(Core) - Sysbuf
         CALL gopen(Mr,Core(lc+1),0)
         imr(1) = Mr
         CALL rdtrl(imr)
         nr = imr(2)
         Nr1 = nr
         Ii = 1
         Jj = nr
         Incur = 1
         Itb = imr(5)
         nr2 = Itb*nr
         ivi = nr*nr
         iphi = ivi
         ivi2 = Itb*ivi
         ialph = 2*ivi
         iloop = 0
         k = 0
         DO i = 1 , nr
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL unpack(*2,Mr,Core(k+1))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!     NULL COLUMN
!
 2                DO j = 1 , nr2
                     Core(j+k) = 0.0
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
                  kkk = k + ivi2
                  DO j = 1 , nr2
                     Core(j+kkk) = 0.0
                  ENDDO
                  IF ( Itb==1 ) THEN
                     Core(kkk+i) = 1.0
                  ELSE
                     kkk = kkk/2
                     dcore(kkk+i) = 1.0D0
                  ENDIF
                  k = k + nr2
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(Mr,1)
!
!     COMPUTE SI
!
         IF ( Itb/=2 ) THEN
            DO
               ssi = 0.0
               DO i = 1 , nr
                  sterm = 0.0
                  DO j = 1 , nr
                     k = (j-1)*nr + i
                     kk = ivi + j
                     sterm = sterm + Core(k)*Core(kk)
                  ENDDO
                  k = ivi + i
                  ssi = ssi + sterm*Core(k)
               ENDDO
               IF ( ssi<=0.0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ssi = 1.0/sqrt(ssi)
!
!     CONVERT VI INTO PHI
!
               DO i = 1 , nr
                  k = ivi + i
                  Core(k) = Core(k)*ssi
               ENDDO
               iloop = iloop + 1
               IF ( iloop==nr ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     CALCULATE ALPHAJ
!
               DO j = 1 , iloop
                  k = ialph + j
                  Core(k) = 0.0
                  DO i = 1 , nr
                     sterm = 0.0
                     DO l = 1 , nr
                        kk = (l-1)*nr + i
                        kkk = ivi + nr + l
                        sterm = sterm + Core(kk)*Core(kkk)
                     ENDDO
                     kk = iphi + (j-1)*nr + i
                     Core(k) = Core(k) + sterm*Core(kk)
                  ENDDO
               ENDDO
!
!     COMPUTE NEXT V VECTOR
!
               DO i = 1 , nr
                  sterm = 0.0
                  DO j = 1 , iloop
                     kk = ialph + j
                     k = iphi + (j-1)*nr + i
                     sterm = sterm + Core(kk)*Core(k)
                  ENDDO
                  k = ivi + nr + i
                  Core(k) = Core(k) - sterm
               ENDDO
               ivi = ivi + nr
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         si = 0.0D0
         DO i = 1 , nr
            term = 0.0D0
            DO j = 1 , nr
               k = (j-1)*nr + i
               kk = ivi + j
               term = term + dcore(k)*dcore(kk)
            ENDDO
            k = ivi + i
            si = si + term*dcore(k)
         ENDDO
         IF ( si>0.0D0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (Nout,99001) Ufm
99001    FORMAT (A23,' 2200, INCONSISTENT RIGID BODY SYSTEM.')
         CALL mesage(-61,0,nam)
         spag_nextblock_1 = 4
      CASE (4)
         si = 1.0D0/dsqrt(si)
!
!     CONVERT VI INTO PHI
!
         DO i = 1 , nr
            k = ivi + i
            dcore(k) = dcore(k)*si
         ENDDO
         iloop = iloop + 1
         IF ( iloop/=nr ) THEN
!
!     CALCULATE ALPHAJ
!
            DO j = 1 , iloop
               k = ialph + j
               dcore(k) = 0.0D0
               DO i = 1 , nr
                  term = 0.0D0
                  DO l = 1 , nr
                     kk = (l-1)*nr + i
                     kkk = ivi + nr + l
                     term = term + dcore(kk)*dcore(kkk)
                  ENDDO
                  kk = iphi + (j-1)*nr + i
                  dcore(k) = dcore(k) + term*dcore(kk)
               ENDDO
            ENDDO
!
!     COMPUTE NEXT V VECTOR
!
            DO i = 1 , nr
               term = 0.0D0
               DO j = 1 , iloop
                  kk = ialph + j
                  k = iphi + (j-1)*nr + i
                  term = term + dcore(kk)*dcore(k)
               ENDDO
               k = ivi + nr + i
               dcore(k) = dcore(k) - term
            ENDDO
            ivi = ivi + nr
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     PACK PHIRO
!
         Ita1 = Itb
         Itb1 = Itb
         Ii1 = 1
         Jj1 = nr
         Incur1 = 1
         CALL gopen(Scr1,Core(lc+1),1)
         CALL makmcb(iscr1,Scr1,nr,1,Itb)
         DO i = 1 , nr
            k = ivi2 + (i-1)*nr2
            CALL pack(Core(k+1),Scr1,iscr1)
         ENDDO
         CALL close(Scr1,1)
         CALL wrttrl(iscr1(1))
!
!     COMPUTE PHILO = DM*PHIRO
!
         CALL ssg2b(Dm,Scr1,0,Scr2,0,Itb,1,Scr4)
!
!     MERGE PHIRP AND PHILO TO FORM PHIA
!
         CALL sdr1b(Scr3,Scr2,Scr1,Scr4,Ua,Ul,Ur,Uset,0,0)
         CALL gopen(Scr4,Core(lc+1),0)
         lc = lc - Sysbuf
         CALL gopen(Phia,Core(lc+1),1)
         imr(1) = Scr4
         CALL rdtrl(imr(1))
         nprob = imr(3)
         dcore(1) = 0.D0
         Jj = nprob
         Incur = 1
         i3 = 3
         DO j = 1 , nr
            Ii = 0
            CALL unpack(*10,Scr4,Core(i3))
            Ii1 = Ii
            Jj1 = Jj
            CALL pack(Core(i3),Phia,iscr1)
            CYCLE
!
!     NULL COLUMN
!
 10         Ii1 = 1
            Jj1 = 1
            CALL pack(Core,Phia,iscr1)
         ENDDO
         CALL close(Scr4,1)
         CALL close(Phia,1)
         lc = lc + Sysbuf
!
!     PUT NR ZEROS ON LAMA
!
         CALL gopen(Lama,Core(lc+1),1)
         dcore(1) = 0.D0
         DO i = 1 , nr
            CALL write(Lama,Core,Itb,1)
         ENDDO
         CALL close(Lama,2)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE read1
