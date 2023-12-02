!*==mbplot.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbplot(Nw1,Nd1,Nwn,Nc21,Nc2n,Nc1,Ncn,Ndn)
   IMPLICIT NONE
   USE C_MBOXC
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Nw1
   INTEGER , DIMENSION(1) :: Nd1
   INTEGER , DIMENSION(1) :: Nwn
   INTEGER , DIMENSION(1) :: Nc21
   INTEGER , DIMENSION(1) :: Nc2n
   INTEGER , DIMENSION(1) :: Nc1
   INTEGER , DIMENSION(1) :: Ncn
   INTEGER , DIMENSION(1) :: Ndn
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: blank , dia , fp , tp , wg , wk
   INTEGER :: i , j , ncbmx , nsbm
   REAL , DIMENSION(50) :: pl
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE TO PRINT A REPRESENTATION OF PLANFORM BEING CONSIDERED
!
   DATA blank , dia , wg , fp , tp , wk/1H  , 1H. , 1HS , 1H1 , 1H2 , 1H,/
!
   nsbm = max0(Nsb,Nsbd)
   ncbmx = max0(Ncb,5)
   WRITE (N6,99001) Mach , Boxw , Boxl
99001 FORMAT (1H1,29X,'GRAPHIC DISPLAY OF REGIONS ON MAIN SEMISPAN',/10X,11HMACH NUMBER,F8.3,11X,9HBOX WIDTH,F11.6,10X,             &
            & 10HBOX LENGTH,F11.6,//)
   DO i = 1 , ncbmx
      DO j = 1 , nsbm
         pl(j) = blank
         IF ( j>Nsb ) THEN
            IF ( (i>=Nd1(j) .AND. i<=Ndn(j)) .OR. (i>=Nc1(j) .AND. i<=Ncn(j)) ) pl(j) = dia
         ELSEIF ( i>=Nw1(j) ) THEN
            IF ( i>Nwn(j) ) THEN
               IF ( i<=Ndn(j) ) pl(j) = wk
            ELSEIF ( i>=Nc21(j) .AND. i<=Nc2n(j) ) THEN
               pl(j) = tp
            ELSEIF ( i>=Nc1(j) .AND. i<=Ncn(j) ) THEN
               pl(j) = fp
            ELSE
               pl(j) = wg
            ENDIF
         ELSEIF ( i>=Nd1(j) ) THEN
            pl(j) = dia
         ENDIF
      ENDDO
!
      WRITE (N6,99002) (pl(j),j=1,nsbm)
99002 FORMAT (30X,50A1)
!
      IF ( i<=5 ) THEN
         IF ( i==2 ) THEN
            WRITE (N6,99003)
99003       FORMAT (1H+,84X,11H1    CNTRL1)
         ELSEIF ( i==3 ) THEN
            WRITE (N6,99004)
99004       FORMAT (1H+,84X,11H2    CNTRL2)
         ELSEIF ( i==4 ) THEN
            WRITE (N6,99005)
99005       FORMAT (1H+,84X,14H.    DIAPHRAGM)
         ELSEIF ( i==5 ) THEN
            WRITE (N6,99006)
99006       FORMAT (1H+,84X,9H,    WAKE)
         ELSE
            WRITE (N6,99007)
99007       FORMAT (1H+,84X,9HS    MAIN)
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE mbplot
