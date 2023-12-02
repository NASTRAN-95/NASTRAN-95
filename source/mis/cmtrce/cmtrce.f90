!*==cmtrce.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmtrce(Iertab,Iwds,Itomny)
   IMPLICIT NONE
   USE C_CMB003
   USE C_SYSTEM
   USE C_ZZZZZZ
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
         WRITE (Of,99001)
99001    FORMAT (/1X,61HTHE FOLLOWING CONNECTIONS HAVE BEEN FOUND TO BE INCONSISTANT.,/1X,                                          &
                &57HATTEMPTS HAVE BEEN MADE TO CONNECT INTERNAL POINTS WITHIN,/1X,                                                  &
                &57HTHE SAME PSEUDOSTRUCTURE DUE TO SPLIT DEGREES OF FREEDOM.,/1X,                                                  &
                &79HTHESE ERRORS MUST BE RESOLVED BY THE USER VIA RELES DATA OR MANUAL CONNECTIONS./)
         WRITE (Of,99004)
         nline = nline + 5
         spag_nextblock_1 = 2
      CASE (2)
!
         ips = Iertab(ib+1)
         nam(1) = Combo(ips,1)
         nam(2) = Combo(ips,2)
         CALL sfetch(nam,nheqss,1,itest)
         CALL suread(Z(1),-1,nout,itest)
         ipt = nout
!
!     READ EQSS FOR EACH COMPONENT
!
         ncomp = 3
         ncomp = Z(ncomp)
         ist = ipt + ncomp + 2
         Z(ipt+1) = ist
         DO i = 1 , ncomp
            CALL suread(Z(ist),-1,nout,itest)
            Z(ipt+1+i) = nout + ist
            CALL sort(0,0,3,2,Z(ist),nout)
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
                  ii = Z(ipt+jj)
                  nwds = Z(ipt+jj+1) - Z(ipt+jj)
                  CALL bisloc(*5,ip,Z(ii+1),3,nwds/3,iloc)
                  iout(3*j) = Z(ii+iloc-1)
                  iout(3*j-2) = Z(2*jj+3)
                  iout(3*j-1) = Z(2*jj+4)
                  EXIT SPAG_Loop_3_1
 5             ENDDO SPAG_Loop_3_1
            ENDDO
            Line = Line + 1
            IF ( Line>Nlpp ) THEN
               CALL page1
               WRITE (Of,99004)
               Line = Line + 2
            ENDIF
            WRITE (Of,99002) Iertab(i) , iout
99002       FORMAT (6X,I1,10X,2A4,5X,I8,9X,2A4,5X,I8)
         ENDDO
         IF ( Itomny==0 ) RETURN
         WRITE (Of,99003)
99003    FORMAT (/5X,93HTHE NUMBER OF FATAL MESSAGES EXCEEDED THE AVAILABLE STORAGE. SOME MESSAGES HAVE BEEN DELETED.)
         RETURN
      CASE (3)
!
!     GET NEXT PSEUDOSTRUCUTRE
!
         ib = i
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (5X,3HDOF,5X,12HSUBSTRUCTURE,5X,8H GRID ID,5X,12HSUBSTRUCTURE,5X,8H GRID ID/)
END SUBROUTINE cmtrce
