
SUBROUTINE cmtrce(Iertab,Iwds,Itomny)
   IMPLICIT NONE
   INTEGER Combo(7,5) , Ij2(2) , Ijunk(6) , Junk , Line , Nlpp , Of , Z(1)
   COMMON /cmb003/ Combo
   COMMON /system/ Junk , Of , Ijunk , Nlpp , Ij2 , Line
   COMMON /zzzzzz/ Z
   INTEGER Itomny , Iwds
   INTEGER Iertab(1)
   INTEGER i , ib , ii , iloc , iout(6) , ip , ips , ipt , ist , itest , j , jj , nam(2) , ncomp , nheqss , nline , nout , nwds
!
!     THIS ROUTINE TRACES BACK IMPROPER CONNECTIONS FINDING
!     GIRD POINT IDS FOR INTERNAL POINT  NUMBERS
!
!
   DATA nheqss/4HEQSS/
!
   CALL sort(0,0,4,2,Iertab(1),Iwds)
   ib = 1
   CALL page1
   WRITE (Of,99001)
99001 FORMAT (/1X,61HTHE FOLLOWING CONNECTIONS HAVE BEEN FOUND TO BE INCONSISTANT.,/1X,                                             &
             &57HATTEMPTS HAVE BEEN MADE TO CONNECT INTERNAL POINTS WITHIN,/1X,                                                     &
             &57HTHE SAME PSEUDOSTRUCTURE DUE TO SPLIT DEGREES OF FREEDOM.,/1X,                                                     &
             &79HTHESE ERRORS MUST BE RESOLVED BY THE USER VIA RELES DATA OR MANUAL CONNECTIONS./)
   WRITE (Of,99004)
   nline = nline + 5
!
 100  ips = Iertab(ib+1)
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
      IF ( Iertab(i+1)/=ips ) GOTO 200
      DO j = 1 , 2
         ip = Iertab(i+1+j)
         DO jj = 1 , ncomp
            ii = Z(ipt+jj)
            nwds = Z(ipt+jj+1) - Z(ipt+jj)
            CALL bisloc(*120,ip,Z(ii+1),3,nwds/3,iloc)
            iout(3*j) = Z(ii+iloc-1)
            iout(3*j-2) = Z(2*jj+3)
            iout(3*j-1) = Z(2*jj+4)
            EXIT
 120     ENDDO
      ENDDO
      Line = Line + 1
      IF ( Line>Nlpp ) THEN
         CALL page1
         WRITE (Of,99004)
         Line = Line + 2
      ENDIF
      WRITE (Of,99002) Iertab(i) , iout
99002 FORMAT (6X,I1,10X,2A4,5X,I8,9X,2A4,5X,I8)
   ENDDO
   IF ( Itomny==0 ) RETURN
   WRITE (Of,99003)
99003 FORMAT (/5X,93HTHE NUMBER OF FATAL MESSAGES EXCEEDED THE AVAILABLE STORAGE. SOME MESSAGES HAVE BEEN DELETED.)
   RETURN
!
!     GET NEXT PSEUDOSTRUCUTRE
!
 200  ib = i
   GOTO 100
99004 FORMAT (5X,3HDOF,5X,12HSUBSTRUCTURE,5X,8H GRID ID,5X,12HSUBSTRUCTURE,5X,8H GRID ID/)
END SUBROUTINE cmtrce