
SUBROUTINE normal
   IMPLICIT NONE
   INTEGER Incrp , Incru , Iopt(2) , Iopt1 , Ip1 , Ip2 , Ipkot1 , Ipkot2 , Iprc(2) , Irc(4) , Isysbf , Iu1 , Iu2 , Iunout , Iz(1) , &
         & Ncol , Nout , Nrow , Nwds(4)
   CHARACTER*23 Ufm
   REAL Xxmax , Z(1)
   DOUBLE PRECISION Zd(1)
   COMMON /blank / Ncol , Nrow , Xxmax , Iopt
   COMMON /packx / Ipkot1 , Ipkot2 , Ip1 , Ip2 , Incrp
   COMMON /system/ Isysbf , Nout
   COMMON /type  / Iprc , Nwds , Irc
   COMMON /unpakx/ Iunout , Iu1 , Iu2 , Incru
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Iz
   REAL dx , x , xmax , xx
   DOUBLE PRECISION dxmax , dzero
   INTEGER i , iblnk , ibuf1 , ibuf2 , icore , icrreq , iprec , isrss , isubnm(2) , itype , ivec , ivec1 , ivec2 , j , k , kwords , &
         & matin , matout , max , mcb(7) , mwords , nrow2 , nrowp , nwords
   INTEGER korsz
!
!     THIS IS THE DRIVER FOR THE NORM MODULE.
!
!     NORM        INMAT/OUTMAT/S,N,NCOL/S,N,NROW/S,N,XNORM/V,Y,IOPT $
!
!     DEPENDING ON THE VALUE OF IOPT, THIS MODULE PERFORMS THE
!     FOLLOWING FUNCTIONS --
!
!     IOPT = 'MAX'
!                 NORM GENERATES A MATRIX.  EACH COLUMN OF THIS OUTPUT
!                 MATRIX REPRESENTS A COLUMN OF THE INPUT MATRIX
!                 NORMALIZED BY ITS LARGEST ROW ELEMENT. (DEFAULT)
!
!     IOPT = 'SRSS'
!                 NORM GENERATES A COLUMN VECTOR.  EACH ELEMENT OF THIS
!                 VECTOR REPRESENTS THE SQUARE ROOT OF THE SUM OF THE
!                 SQUARES (SRSS) OF THE CORRESPONDING ROW OF THE INPUT
!                 MATRIX.
!
!
!
!     INPUT DATA BLOCK --
!
!     INMAT     - ANY MATRIX
!
!     OUTPUT DATA BLOCK --
!
!     OUTMAT    - OUTPUT MATRIX GENERATED AS DESCRIBED BELOW
!
!     PARAMETERS --
!
!     NCOL      - NO. OF COLUMNS OF THE INPUT MATRIX (OUTPUT/INTEGER)
!
!     NROW      - NO. OF ROWS OF THE INPUT MATRIX (OUTPUT/INTEGER)
!
!     XNORM     - MAX. NORMALIZING OR SRSS VALUE, DEPENDING UPON THE
!                 IOPT VALUE SPECIFIED (OUTPUT/REAL)
!     IOPT      - OPTION INDICATING WHETHER EACH COLUMN OF THE INPUT
!                 MATRIX IS TO BE NORMALIZED BY THE MAXIMUM ROW ELEMENT
!                 IN THAT COLUMN OR WHETHER THE SRSS VALUE FOR EACH ROW
!                 OF THE INPUT MATRIX IS TO BE COMPUTED (INPUT/BCD)
!
!     THIS MODULE DEVELOPED BY P. R. PAMIDI OF RPK CORPORATION,
!     MARCH 1988
!
   EQUIVALENCE (Iz(1),Z(1),Zd(1)) , (Iopt1,Iopt(1))
   DATA matin , matout/101 , 201/
   DATA isubnm , max , isrss , iblnk , dzero/4HNORM , 4HAL   , 4HMAX  , 4HSRSS , 4H     , 0.0D+0/
!
   IF ( .NOT.(Iopt(2)==iblnk .AND. (Iopt1==max .OR. Iopt1==isrss)) ) THEN
      WRITE (Nout,99001) Ufm , Iopt
99001 FORMAT (A23,', ILLEGAL BCD VALUE (',2A4,') FOR THE 4TH PARAMATER',' IN MODULE NORM')
      CALL mesage(-61,0,0)
   ENDIF
   Incru = 1
   Incrp = 1
   icore = korsz(Iz)
   ibuf1 = icore - Isysbf + 1
   ibuf2 = ibuf1 - Isysbf
   icore = ibuf2 - 1
   CALL gopen(matin,Iz(ibuf1),0)
   CALL gopen(matout,Iz(ibuf2),1)
   mcb(1) = matin
   CALL rdtrl(mcb)
   Ncol = mcb(2)
   Nrow = mcb(3)
   nrow2 = 2*Nrow
   itype = mcb(5)
   iprec = itype
   IF ( iprec>2 ) iprec = iprec - 2
   Iunout = itype
   Ipkot1 = itype
   Ipkot2 = itype
   nrowp = iprec*Nrow
   nwords = Nwds(itype)
   mwords = Nrow*nwords
   kwords = mwords
   IF ( Iopt1/=max ) kwords = kwords + nrowp
   icrreq = kwords - icore
   IF ( icrreq>0 ) CALL mesage(-8,icrreq,isubnm)
   ivec = mwords
   ivec1 = ivec + 1
   ivec2 = ivec + nrowp
   IF ( Iopt1/=max ) THEN
      mcb(5) = iprec
      Ipkot1 = iprec
      Ipkot2 = iprec
      DO i = ivec1 , ivec2
         Z(i) = 0.0
      ENDDO
   ENDIF
   mcb(1) = matout
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   Iu1 = 1
   Iu2 = Nrow
!
   Xxmax = 0.0
   DO i = 1 , Ncol
      xx = 0.0
      CALL unpack(*50,matin,Z)
      Ip1 = Iu1
      Ip2 = Iu2
      xmax = -1.0
      GOTO 100
 50   Ip1 = 1
      Ip2 = 1
      xmax = 0.0
      DO j = 1 , nwords
         Z(j) = 0.0
      ENDDO
!
 100  IF ( Iopt1/=isrss ) THEN
         IF ( xmax/=0.0 ) THEN
!
!     OPTION IS MAX
!
            IF ( itype==2 ) THEN
!
               dxmax = dzero
               DO j = 1 , Nrow
                  dx = dabs(Zd(j))
                  IF ( dx>dxmax ) dxmax = dx
               ENDDO
               IF ( dxmax/=dzero ) THEN
                  xx = dxmax
                  DO j = 1 , Nrow
                     Zd(j) = Zd(j)/dxmax
                  ENDDO
!
                  IF ( xx>Xxmax ) Xxmax = xx
               ENDIF
            ELSEIF ( itype==3 ) THEN
!
               xmax = 0.0
               DO j = 1 , nrow2 , 2
                  x = sqrt(Z(j)*Z(j)+Z(j+1)**2)
                  IF ( x>xmax ) xmax = x
               ENDDO
               IF ( xmax/=0.0 ) THEN
                  xx = xmax
                  DO j = 1 , nrow2 , 2
                     Z(j) = Z(j)/xmax
                     Z(j+1) = Z(j+1)/xmax
                  ENDDO
                  IF ( xx>Xxmax ) Xxmax = xx
               ENDIF
            ELSEIF ( itype==4 ) THEN
!
               dxmax = dzero
               DO j = 1 , nrow2 , 2
                  dx = dsqrt(Zd(j)*Zd(j)+Zd(j+1)**2)
                  IF ( dx>dxmax ) dxmax = dx
               ENDDO
               IF ( dxmax/=dzero ) THEN
                  xx = dxmax
                  DO j = 1 , nrow2 , 2
                     Zd(j) = Zd(j)/dxmax
                     Zd(j+1) = Zd(j+1)/dxmax
                  ENDDO
                  IF ( xx>Xxmax ) Xxmax = xx
               ENDIF
            ELSE
!
               xmax = 0.0
               DO j = 1 , Nrow
                  x = abs(Z(j))
                  IF ( x>xmax ) xmax = x
               ENDDO
               IF ( xmax/=0.0 ) THEN
                  xx = xmax
                  DO j = 1 , Nrow
                     Z(j) = Z(j)/xmax
                  ENDDO
                  IF ( xx>Xxmax ) Xxmax = xx
               ENDIF
            ENDIF
         ENDIF
         CALL pack(Z,matout,mcb)
!
!     OPTION IS SRSS
!
      ELSEIF ( xmax/=0.0 ) THEN
         IF ( itype==2 ) THEN
!
            DO j = 1 , Nrow
               k = ivec + j
               Zd(k) = Zd(k) + Zd(j)*Zd(j)
            ENDDO
         ELSEIF ( itype==3 ) THEN
!
            k = ivec
            DO j = 1 , nrow2 , 2
               k = k + 1
               Z(k) = Z(k) + Z(j)*Z(j) + Z(j+1)**2
            ENDDO
         ELSEIF ( itype==4 ) THEN
!
            k = ivec
            DO j = 1 , nrow2 , 2
               k = k + 1
               Zd(k) = Zd(k) + Zd(j)*Zd(j) + Zd(j+1)**2
            ENDDO
         ELSE
!
            DO j = 1 , Nrow
               k = ivec + j
               Z(k) = Z(k) + Z(j)*Z(j)
            ENDDO
         ENDIF
      ENDIF
!
   ENDDO
   CALL close(matin,1)
   IF ( Iopt1/=max ) THEN
!
      Ip1 = Iu1
      Ip2 = Iu2
      IF ( iprec==2 ) THEN
!
         DO i = ivec1 , ivec2
            Zd(i) = dsqrt(Zd(i))
            IF ( Zd(i)>Xxmax ) Xxmax = Zd(i)
         ENDDO
      ELSE
!
         DO i = ivec1 , ivec2
            Z(i) = sqrt(Z(i))
            IF ( Z(i)>Xxmax ) Xxmax = Z(i)
         ENDDO
      ENDIF
!
      CALL pack(Z(ivec1),matout,mcb)
   ENDIF
!
   CALL close(matout,1)
   CALL wrttrl(mcb)
END SUBROUTINE normal
