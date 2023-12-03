!*==gigtkg.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gigtkg
   USE c_gicom
   USE c_packx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , ctype , i , icm , iss , j , jj , jjj , k , kcol , kst , ncore , nwr
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(6,5) , SAVE :: sdtab
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , gopen , korsz , mesage , pack , read , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA nam/4HGIGT , 4HKG  /
   DATA sdtab/9 , 9 , 0 , 9 , 1 , 9 , 9 , 0 , 1 , 9 , 2 , 3 , 9 , 9 , 0 , 9 , 9 , 9 , 9 , 9 , 0 , 9 , 1 , 2 , 9 , 9 , 0 , 9 , 1 , 2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ncore = korsz(z) - 2*sysbuf
         buf1 = ncore
         buf2 = buf1 + sysbuf
         iti = 1
         ito = 1
         ii = 1
         incr = 1
         trl(1) = scr2
         trl(2) = 0
         trl(3) = gsize
         trl(4) = 2
         trl(5) = 1
         trl(6) = 0
         trl(7) = 0
!
!     BUILD A G BY K MATRIX PUT OUT SPLINE3 COLUMNS WHEN NECESSARY
!
         CALL gopen(scr2,z(buf1),1)
         CALL gopen(scr3,z(buf2),0)
         iss = gsize + 1
         ncore = ncore - iss
         kcol = 0
         DO i = 1 , ksize
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( kcol<i ) THEN
                     CALL read(*2,*4,scr3,z(iss),ncore,0,nwr)
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  IF ( kcol==i ) THEN
!
!     BUILD COLUMN FOR SPLINE CARD
!
                     DO j = 1 , gsize
                        z(j) = 0.0
                     ENDDO
                     nn = gsize
                     jj = iss + 4
                     jjj = iss + nwr - 19
                     DO j = jj , jjj , 3
                        k = iz(j) + iz(j+1) - 1
                        z(k) = z(j+2)
                     ENDDO
                     CALL pack(z,scr2,trl)
                  ELSE
                     nn = 1
                     z(1) = 0.0
                     CALL pack(z,scr2,trl)
                  ENDIF
                  CYCLE
 2                kcol = ksize + 1
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 4                kst = iz(iss+2)
                  ctype = iz(iss+nwr-9)
                  icm = iz(iss+3)
                  k = sdtab(icm,ctype)
                  IF ( k==9 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  kcol = kst + k
                  spag_nextblock_2 = 2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(scr2,1)
         CALL close(scr3,1)
         CALL wrttrl(trl)
         RETURN
      CASE (2)
!
!     ERROR MESSAGES
!
         CALL mesage(-8,ncore,nam)
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (out,99001) ufm , iz(iss) , ctype , icm
99001    FORMAT (A23,' 2263, SPLINE3',I9,' FOR CAERO',I1,' HAS ILLEGAL COMPONENT',I6)
         CALL mesage(-37,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gigtkg