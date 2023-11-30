
SUBROUTINE outpt3
   IMPLICIT NONE
   INTEGER Eol , Eor , Erno , Ic , Icard1 , Icol , Ifo , Ir , Iro , Itin , Itout , Iz , Jo , Junk(6) , Kpp , Nam , Namex(2) , Nb ,  &
         & Nlp , Nlpp , No , Noutpt , Param(2,5)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   REAL X(1) , Xx , Z(4)
   COMMON /blank / Jo , Param
   COMMON /phdmix/ Namex , Nam , Ifo , Itin , Itout , Ir , Ic , Noutpt , Kpp , Nlp , Erno , Icol , Iro , Xx , Icard1
   COMMON /system/ Nb , No , Junk , Nlpp
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zntpkx/ Z , Iz , Eol , Eor
   COMMON /zzzzzz/ X
   LOGICAL first
   INTEGER i , ibuf , ii , in(5) , ityp , j , jono , lcor , name(2) , ncards , subnam(2) , trl(7) , trl1 , trl2 , trl3 , trl4 ,     &
         & trl5 , trl6 , trl7
   INTEGER korsz
!
!     PUNCH UP TO 5 MATRIX DATA BLOCK ONTO DMI CARDS
!
!     CALL TO THIS MODULE IS
!
!     OUTPUT3   M1,M2,M3,M4,M5//C,N,PO/C,Y,N1=AB/C,Y,N2=CD/C,Y,N3=EF/
!                                      C,Y,N4=GH/C,Y,N5=IJ   $
!
!               PO = FORTRAN OUTPUT FILE UNIT NO. (DEFAULT = 0)
!                    .GE.0 MEANS NO LISTING OF  CARD IMAGES WILL BE MADE
!                    .LT.0 MEANS LISTING OF DMI CARD IMAGES WILL BE MADE
!                          ON FORTRAN UNIT = IABS(PO).
!
!
!
   EQUIVALENCE (trl(1),trl1) , (trl(2),trl2) , (trl(3),trl3) , (trl(4),trl4) , (trl(5),trl5) , (trl(6),trl6) , (trl(7),trl7)
   DATA subnam/4HOUTP , 4HUT3 / , in/101 , 102 , 103 , 104 , 105/
   DATA ityp/1/
!
!
   lcor = korsz(X) - Nb
   IF ( lcor<=0 ) CALL mesage(-8,lcor,subnam)
   ibuf = lcor + 1
   jono = 0
   IF ( Jo<0 ) jono = iabs(Jo)
   Noutpt = jono
   Itin = 1
   Kpp = 2
   Nlp = Nlpp
!
   DO ii = 1 , 5
      trl1 = in(ii)
      CALL rdtrl(trl)
      IF ( trl1>0 ) THEN
         CALL fname(in(ii),name)
         CALL gopen(in(ii),X(ibuf),0)
         Namex(1) = name(1)
         Namex(2) = name(2)
         Nam = Param(1,ii)
         Ifo = trl4
         Itout = 0
         Ir = trl3
         Ic = trl2
         CALL phdmia
         IF ( Erno/=0 ) GOTO 100
!
         DO j = 1 , trl2
            CALL intpk(*20,in(ii),0,ityp,0)
            first = .FALSE.
            Icol = j
!
            DO i = 1 , trl3
               IF ( Eol/=0 ) EXIT
               CALL zntpki
               Iro = Iz
               Xx = Z(1)
!
!     VAX MAY HAVE A FEW IMBEDED ZEROS
!
               IF ( Xx/=0.0 ) THEN
                  IF ( first ) THEN
                     CALL phdmic
                     IF ( Erno/=0 ) GOTO 100
                  ELSE
                     first = .TRUE.
                     CALL phdmib
                     IF ( Erno/=0 ) GOTO 100
                  ENDIF
               ENDIF
            ENDDO
!
            CALL phdmid
            IF ( Erno/=0 ) GOTO 100
 20      ENDDO
!
         ncards = Icard1 + 1
         CALL page2(-2)
         WRITE (No,99001) Uim , name , ncards
99001    FORMAT (A29,' 4103, OUTPUT3 HAS PUNCHED MATRIX DATA BLOCK ',2A4,' ONTO ',I5,' DMI CARDS.')
         CALL close(in(ii),1)
      ENDIF
   ENDDO
   RETURN
!
!     ERROR MESSAGE
!
 100  CALL page2(-2)
   WRITE (No,99002) Ufm
99002 FORMAT (A23,' 4104, ATTEMPT TO PUNCH MORE THAN 99999 DMI CARDS ','FOR A SINGLE MATRIX.')
   CALL mesage(-61,0,0)
!
END SUBROUTINE outpt3
