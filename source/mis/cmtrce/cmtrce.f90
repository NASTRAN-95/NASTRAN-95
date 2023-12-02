!*==cmtrce.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmtrce(Iertab,Iwds,Itomny)
   USE c_cmb003
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Iertab
   INTEGER :: Iwds
   INTEGER :: Itomny
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ib , ii , iloc , ip , ips , ipt , ist , itest , j , jj , ncomp , nline , nout , nwds
   INTEGER , DIMENSION(6) :: iout
   INTEGER , DIMENSION(2) :: nam
   INTEGER , SAVE :: nheqss
   EXTERNAL bisloc , page1 , sfetch , sort , suread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE TRACES BACK IMPROPER CONNECTIONS FINDING
!     GIRD POINT IDS FOR INTERNAL POINT  NUMBERS
!
!
   DATA nheqss/4HEQSS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL sort(0,0,4,2,Iertab(1),Iwds)
         ib = 1
         CALL page1
         WRITE (of,99001)
99001    FORMAT (/1X,61HTHE FOLLOWING CONNECTIONS HAVE BEEN FOUND TO BE INCONSISTANT.,/1X,                                          &
                &57HATTEMPTS HAVE BEEN MADE TO CONNECT INTERNAL POINTS WITHIN,/1X,                                                  &
                &57HTHE SAME PSEUDOSTRUCTURE DUE TO SPLIT DEGREES OF FREEDOM.,/1X,                                                  &
                &79HTHESE ERRORS MUST BE RESOLVED BY THE USER VIA RELES DATA OR MANUAL CONNECTIONS./)
         WRITE (of,99004)
         nline = nline + 5
         spag_nextblock_1 = 2
      CASE (2)
!
         ips = Iertab(ib+1)
         nam(1) = combo(ips,1)
         nam(2) = combo(ips,2)
         CALL sfetch(nam,nheqss,1,itest)
         CALL suread(z(1),-1,nout,itest)
         ipt = nout
!
!     READ EQSS FOR EACH COMPONENT
!
         ncomp = 3
         ncomp = z(ncomp)
         ist = ipt + ncomp + 2
         z(ipt+1) = ist
         DO i = 1 , ncomp
            CALL suread(z(ist),-1,nout,itest)
            z(ipt+1+i) = nout + ist
            CALL sort(0,0,3,2,z(ist),nout)
            ist = ist + nout
         ENDDO
         DO i = ib , Iwds , 4
            IF ( Iertab(i+1)/=ips ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO j = 1 , 2
               ip = Iertab(i+1+j)
               SPAG_Loop_3_1: DO jj = 1 , ncomp
                  ii = z(ipt+jj)
                  nwds = z(ipt+jj+1) - z(ipt+jj)
                  CALL bisloc(*5,ip,z(ii+1),3,nwds/3,iloc)
                  iout(3*j) = z(ii+iloc-1)
                  iout(3*j-2) = z(2*jj+3)
                  iout(3*j-1) = z(2*jj+4)
                  EXIT SPAG_Loop_3_1
 5             ENDDO SPAG_Loop_3_1
            ENDDO
            line = line + 1
            IF ( line>nlpp ) THEN
               CALL page1
               WRITE (of,99004)
               line = line + 2
            ENDIF
            WRITE (of,99002) Iertab(i) , iout
99002       FORMAT (6X,I1,10X,2A4,5X,I8,9X,2A4,5X,I8)
         ENDDO
         IF ( Itomny==0 ) RETURN
         WRITE (of,99003)
99003    FORMAT (/5X,93HTHE NUMBER OF FATAL MESSAGES EXCEEDED THE AVAILABLE STORAGE. SOME MESSAGES HAVE BEEN DELETED.)
         RETURN
      CASE (3)
!
!     GET NEXT PSEUDOSTRUCUTRE
!
         ib = i
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (5X,3HDOF,5X,12HSUBSTRUCTURE,5X,8H GRID ID,5X,12HSUBSTRUCTURE,5X,8H GRID ID/)
END SUBROUTINE cmtrce
