
SUBROUTINE mbplot(Nw1,Nd1,Nwn,Nc21,Nc2n,Nc1,Ncn,Ndn)
   IMPLICIT NONE
   REAL Asym , Beta , Boxa , Boxl , Boxw , Cntrl1 , Cntrl2 , Cr , Crank1 , Crank2 , Ek , Ekbar , Ekm , Gc , Mach , Sys
   INTEGER Kc , Kc1 , Kc1t , Kc2 , Kc2t , Kct , N6 , Nbox , Ncb , Njj , Npts0 , Npts1 , Npts2 , Nsb , Nsbd , Ntote
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2 , Nbox , Npts0 , Npts1 , Npts2 , Asym , Gc , Cr , Mach , Beta , Ek ,     &
                 & Ekbar , Ekm , Boxl , Boxw , Boxa , Ncb , Nsb , Nsbd , Ntote , Kc , Kc1 , Kc2 , Kct , Kc1t , Kc2t
   COMMON /system/ Sys , N6
   INTEGER Nc1(1) , Nc21(1) , Nc2n(1) , Ncn(1) , Nd1(1) , Ndn(1) , Nw1(1) , Nwn(1)
   REAL blank , dia , fp , pl(50) , tp , wg , wk
   INTEGER i , j , ncbmx , nsbm
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