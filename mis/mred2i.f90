
SUBROUTINE mred2i(Kode,Nuf,N2)
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Gs , Idum1 , Idum2 , Idum3(5) , Idum4 , Idum5(4) , Idum6(14) , Incr , Incru , Infile(12) , Iprntr , Irow , &
         & Irowu , Iscr(10) , Korbgn , Korlen , Lamamr , Modlen , Nmodes , Nrow , Nrowu , Otfile(6) , Qsm , Typin , Typinu ,        &
         & Typout , Z(1)
   DOUBLE PRECISION Dz(1)
   REAL Forpi2 , Rz(1)
   CHARACTER*23 Ufm
   COMMON /blank / Idum1 , Dry , Idum2 , Gbuf1 , Idum3 , Infile , Otfile , Iscr , Korlen , Korbgn , Idum6 , Nmodes , Modlen
   COMMON /condas/ Idum5 , Forpi2
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /system/ Idum4 , Iprntr
   COMMON /unpakx/ Typinu , Irowu , Nrowu , Incru
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Kode , N2 , Nuf
   INTEGER dblkor , i , idiag , ifile , imsg , itrlr1(7) , j , k , kore , locqsm , modnam(2) , nwdsrd , qsmcol , qsmrow , qsmtyp ,  &
         & sglkor
!
!     THIS SUBROUTINE COMPUTES THE GS MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Qsm,Infile(12)) , (Gs,Iscr(7)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2I  / , idiag/3/
!
!     TEST OPERATION MODE
!
   IF ( Dry==-2 ) RETURN
!
!     FORM GS MATRIX
!
!                 **     **
!                 *       *        T
!        **  **   * .   0 * **   **
!        *    *   *  .    * *     *                 2
!        * GS * =-*  1/K  * * QSM *    WHERE K = M W
!        *    *   *    .  * *     *               I I
!        **  **   * 0   . * **   **
!                 *       *
!                 **     **
!
   itrlr1(1) = Qsm
   CALL rdtrl(itrlr1)
   IF ( itrlr1(1)<0 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
      WRITE (Iprntr,99001) Ufm
!
99001 FORMAT (A23,' 6638, IN MODULE MREDUCE WITH USERMODE=2, THE ','CONSTRAINT FORCES MATRIX (QSM) CANNOT BE PURGED.')
      GOTO 600
   ELSE
      qsmrow = itrlr1(2)
      qsmcol = itrlr1(3)
!
!                        2
!     FORM K = 1.0 / (M W )
!                      I I
!
      IF ( Korbgn+7+qsmrow>=Korlen ) GOTO 400
      ifile = Lamamr
      CALL gopen(Lamamr,Z(Gbuf1),0)
      CALL fwdrec(*300,Lamamr)
      Nmodes = 0
      DO
         CALL read(*200,*100,Lamamr,Z(Korbgn),7,0,nwdsrd)
         Rz(Korbgn+7+Nmodes) = 1.0/(Forpi2*Rz(Korbgn+5)*(Rz(Korbgn+4)**2))
         Nmodes = Nmodes + 1
         IF ( Korbgn+7+Nmodes>=Korlen ) GOTO 400
      ENDDO
   ENDIF
 100  CALL close(Lamamr,1)
   IF ( Nmodes/=qsmrow ) THEN
      WRITE (Iprntr,99002) Ufm , qsmrow , qsmcol , Nmodes
99002 FORMAT (A23,' 6634, IN MODULE MREDUCE WITH USERMODE=2, THE ','CONSTRAINT FORCES MATRIX (',I3,3H X ,I3,1H),/30X,               &
             &'IS INCOMPATABLE WITH THE NUMBER OF MODES (',I3,2H).)
      GOTO 600
   ELSE
      Modlen = Nmodes
!
!     READ QSM INTO CORE
!
      kore = Korbgn
      Korbgn = Korbgn + 7 + itrlr1(2)
      IF ( Korbgn+qsmrow*(qsmcol+1)>=Korlen ) GOTO 400
      Typinu = itrlr1(5)
      Irowu = 1
      Nrowu = itrlr1(3)
      Incru = 1
      qsmtyp = itrlr1(5)
      dblkor = Korbgn/2 + 1
      sglkor = 2*dblkor - 1
      CALL gopen(Qsm,Z(Gbuf1),0)
      IF ( qsmtyp==2 ) THEN
         locqsm = dblkor
         DO i = 1 , qsmrow
            CALL unpack(*110,Qsm,Dz(dblkor))
            GOTO 120
 110        DO j = 1 , qsmcol
               Dz(dblkor+j-1) = 0.0D0
            ENDDO
 120        dblkor = dblkor + itrlr1(3)
         ENDDO
         Korbgn = dblkor
      ELSE
         locqsm = sglkor
         DO i = 1 , qsmrow
            CALL unpack(*130,Qsm,Rz(sglkor))
            GOTO 140
 130        DO j = 1 , qsmcol
               Rz(sglkor+j-1) = 0.0E0
            ENDDO
 140        sglkor = sglkor + itrlr1(3)
         ENDDO
         Korbgn = sglkor
      ENDIF
      CALL close(Qsm,1)
!
!     FORM GS MATRIX
!
      Typin = itrlr1(5)
      Typout = itrlr1(5)
      Irow = 1
      Nrow = qsmrow
      Incr = 1
      CALL makmcb(itrlr1,Gs,qsmrow,idiag,Typin)
      dblkor = Korbgn/2 + 1
      sglkor = 2*dblkor - 1
      CALL gopen(Gs,Z(Gbuf1),1)
      DO i = 1 , qsmcol
         DO j = 1 , qsmrow
            k = 3*(j-1)
            IF ( qsmtyp==2 ) THEN
               Dz(dblkor+j-1) = Rz(kore+7+j-1)*Dz(locqsm+k)
            ELSE
               Rz(sglkor+j-1) = Rz(kore+7+j-1)*Rz(locqsm+k)
            ENDIF
         ENDDO
         CALL pack(Dz(dblkor),Gs,itrlr1)
      ENDDO
      Korbgn = kore
      CALL close(Gs,1)
      CALL wrttrl(itrlr1)
      RETURN
   ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 200  imsg = -2
   GOTO 500
 300  imsg = -3
   GOTO 500
 400  imsg = -8
   ifile = 0
 500  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   RETURN
 600  Dry = -2
   RETURN
!
END SUBROUTINE mred2i