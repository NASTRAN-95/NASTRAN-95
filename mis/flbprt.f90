
SUBROUTINE flbprt(Iuset,Ieqex,Ibuf)
   IMPLICIT NONE
   REAL Dum1(8) , Dum2(6) , Dum3 , U21 , U22 , U23 , Ud , Ue , Ufe , Uk , Une , Up , Upa , Ups , Usa
   INTEGER Eqexin , Line , Nlpp , Nout , Npage , Sysbuf , Two(32) , Ua , Uab , Uf , Ufr , Ug , Ui , Ul , Um , Un , Uo , Ur , Us ,   &
         & Usb , Usg , Ux , Uy , Uz , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud , Ups , Usa , Uk , Upa , U21 , &
                 & U22 , U23 , Ux , Uy , Ufr , Uz , Uab , Ui
   COMMON /flbfil/ Dum1 , Eqexin
   COMMON /system/ Sysbuf , Nout , Dum2 , Nlpp , Dum3 , Npage , Line
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   INTEGER Ibuf , Ieqex , Iuset
   INTEGER andf
   INTEGER astric , blank , d32 , d33 , dash , expnt , file , i , ibit , icol , idof , imk , inpnt , inum , itype , iu , juset , k ,&
         & kk , msk(17) , n , name(2) , ndof , neqex , nz , sbit(17) , title(3,9) , upbit(17) , zdof(10) , zgrd(10)
   EXTERNAL andf
!
!     HYDROELEASTIC USET OUTPUT
!
!     PRINTS DOF VS. DISP SETS IF DIAG 32 IS ON.
!     PRINTS DISP SETS VS. DOF IF DIAG 33 IS ON.
!
   DATA name/4HFLBP , 4HRT  /
   DATA title/4H     , 4H     , 4H MPC , 4H     , 4H     , 4H SPC , 4H     , 4H     , 4HOMIT , 4H     , 4HANAL , 4HYSIS , 4H     ,  &
       &4HPERM , 4H SPC , 4H     , 4HBDRY , 4H SPC , 4H   S , 4HTRUC , 4HTURE , 4H     , 4H   F , 4HLUID , 4HFREE , 4H SUR , 4HFACE/
   DATA blank/1H / , dash/1H-/ , astric/1H*/
!
!
!     DETERMINE IF ANY OUTPUT IS REQUESTED
!
   CALL sswtch(32,d32)
   CALL sswtch(33,d33)
   IF ( d32==0 .AND. d33==0 ) RETURN
!
!     READ EQEXIN INTO CORE
!
   file = Eqexin
   CALL open(*200,Eqexin,Z(Ibuf),0)
   CALL fwdrec(*300,Eqexin)
   CALL fwdrec(*300,Eqexin)
   nz = Ibuf - Ieqex
   CALL read(*300,*100,Eqexin,Z(Ieqex),nz,1,neqex)
   WRITE (Nout,99001) Uwm
99001 FORMAT (A25,' 8011, INSUFFICIENT CORE TO HOLD CONTENTS OF EQEXIN',' DATA BLOCK',/31X,'HYDROELASTIC USET PRINTOUT TERMINATED.')
   RETURN
 100  CALL close(Eqexin,1)
!
!     SORT ON INTERNAL ID
!
   CALL sort(0,0,2,2,Z(Ieqex),neqex)
!
!     SET UP USET MASKS FOR DOF VS. DISP SET PRINTOUT
!
   IF ( d32/=0 ) THEN
      msk(1) = Two(Usb)
      msk(2) = Two(Usg)
      msk(3) = Two(Ul)
      msk(4) = Two(Ua)
      msk(5) = Two(Uf)
      msk(6) = Two(Un)
      msk(7) = Two(Ug)
      msk(8) = Two(Ur)
      msk(9) = Two(Uo)
      msk(10) = Two(Us)
      msk(11) = Two(Um)
      msk(12) = Two(Ux)
      msk(13) = Two(Uy)
      msk(14) = Two(Ufr)
      msk(15) = Two(Uz)
      msk(16) = Two(Uab)
      msk(17) = Two(Ui)
!
      DO i = 1 , 17
         sbit(i) = 0
      ENDDO
!
!     PASS THROUGH EQEXIN TABLE AND DETERMINE NUMBER OF DOF FOR EACH
!     POINT
!
      juset = Iuset - 1
      Line = Nlpp
      inpnt = 0
      DO k = 1 , neqex , 2
         itype = mod(Z(Ieqex+k),10)
         ndof = 6
         IF ( itype==2 ) ndof = 1
!
!     FOR EACH DOF - GET USET ENTRY AND TEST VARIOUS MACK BITS
!
         DO kk = 1 , ndof
            juset = juset + 1
            iu = Z(juset)
            inpnt = inpnt + 1
            expnt = Z(Ieqex+k-1)
            idof = kk
            IF ( ndof==1 ) idof = 0
            DO ibit = 1 , 17
               IF ( andf(msk(ibit),iu)/=0 ) THEN
                  upbit(ibit) = astric
                  sbit(ibit) = sbit(ibit) + 1
               ELSE
                  upbit(ibit) = blank
               ENDIF
            ENDDO
!
!     PRINT LINE OF OUTPUT
!
            Line = Line + 1
            IF ( Line>Nlpp ) THEN
               CALL page1
               WRITE (Nout,99002)
!
!     FORMAT STATEMENTS
!
99002          FORMAT (//12X,'INT DOF  EXT GP. DOF   SB   SG    L    A    F   ',                                                    &
                      &'N    G    R    O    S    M    X    Y   FR    Z   AB    I',/1X,131(1H-))
               Line = 1
            ENDIF
            WRITE (Nout,99003) inpnt , expnt , dash , idof , upbit
99003       FORMAT (10X,I8,1X,I8,1X,A1,I2,1X,17(4X,A1))
         ENDDO
      ENDDO
!
!     PRINT COLUMN TOTALS
!
      WRITE (Nout,99004) sbit
99004 FORMAT (1H0,31H-- C O L U M N   T O T A L S --,17I5)
   ENDIF
!
!     SET UP MASKS FOR DISP SET VS. DOF PRINTOUT
!
   IF ( d33==0 ) RETURN
   msk(1) = Two(Um)
   msk(2) = Two(Us)
   msk(3) = Two(Uo)
   msk(4) = Two(Ua)
   msk(5) = Two(Usg)
   msk(6) = Two(Usb)
   msk(7) = Two(Ux)
   msk(8) = Two(Uy)
   msk(9) = Two(Ufr)
!
!     PASS THROUGH EQEXIN TABLE ONCE FOR EACH DISP SET TO BE PRINTED
!
   DO imk = 1 , 9
      inum = -9
      icol = 0
      Line = Nlpp
      juset = Iuset - 1
      DO k = 1 , neqex , 2
         itype = mod(Z(Ieqex+k),10)
         ndof = 6
         IF ( itype==2 ) ndof = 1
!
!     FOR EACH DOF - TEST IF IT IS IN DESIRED SET FOR THIS PASS
!
         expnt = Z(Ieqex+k-1)
         DO kk = 1 , ndof
            juset = juset + 1
            IF ( andf(Z(juset),msk(imk))/=0 ) THEN
               idof = kk
               IF ( ndof==1 ) idof = 0
               icol = icol + 1
               zgrd(icol) = expnt
               zdof(icol) = idof
               IF ( icol>=10 ) THEN
!
!     WE HAVE ACUMULATED 10 POINTS - PRINT THEM
!
                  icol = 0
                  Line = Line + 1
                  IF ( Line>Nlpp ) THEN
                     CALL page1
                     WRITE (Nout,99005) (title(i,imk),i=1,3)
                     Line = 1
                  ENDIF
                  inum = inum + 10
                  WRITE (Nout,99006) inum , (zgrd(i),zdof(i),i=1,10)
               ENDIF
            ENDIF
!
         ENDDO
      ENDDO
!
!     PRINT ANY REMAINING ENTRIES
!
      IF ( icol/=0 ) THEN
         Line = Line + 1
         IF ( Line>Nlpp ) THEN
            CALL page1
            WRITE (Nout,99005) (title(i,imk),i=1,3)
            Line = 1
         ENDIF
         inum = inum + 10
         WRITE (Nout,99006) inum , (zgrd(i),zdof(i),i=1,icol)
      ENDIF
!
   ENDDO
!
!     PRINT OUT COMPLETE
!
   RETURN
!
!     ERROR CONDITIONS - PRINT NON-FATAL MESSAGE
!
 200  n = 1
   GOTO 400
 300  n = 2
 400  CALL mesage(n,file,name)
   RETURN
99005 FORMAT (45X,3A4,17H DISPLACEMENT SET,//16X,3H-1-,8X,3H-2-,8X,3H-3-,8X,3H-4-,8X,3H-5-,8X,3H-6-,8X,3H-7-,8X,3H-8-,8X,3H-9-,7X,  &
             &4H-10-,/1H )
99006 FORMAT (1H ,I6,1H=,10(1X,I8,1H-,I1))
END SUBROUTINE flbprt