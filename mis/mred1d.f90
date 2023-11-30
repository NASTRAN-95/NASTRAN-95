
SUBROUTINE mred1d
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Gbuf2 , Idum1(3) , Idum2(3) , Idum3(6) , Idum4 , Idum5 , Idum6(12) , Idum7(4) , Ieig , Iprntr , Itwo(32) , &
         & Korbgn , Korlen , Oldnam(2) , Type(2) , Z(1)
   CHARACTER*23 Ufm
   LOGICAL Usrmod
   COMMON /blank / Oldnam , Dry , Idum1 , Type , Idum5 , Gbuf1 , Gbuf2 , Idum2 , Korlen , Idum7 , Ieig , Idum3 , Korbgn , Idum6 ,   &
                 & Usrmod
   COMMON /system/ Idum4 , Iprntr
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER dnamic , eedx , eig(3,3) , eigcp , eigcpr(3) , eigtrl(7) , eigtyp , i , imsg , itest , komplx , kreal , letr(3) ,        &
         & modnam(2) , nowdsr , nwds2r
   INTEGER orf
   EXTERNAL orf
!
!     THIS SUBROUTINE GENERATES THE EEDX DATA BLOCK USING THE EED DATA
!     BLOCK FORMAT FROM THE EIGR OR EIGC AND EIGP BULK DATA FOR THE
!     MRED1 MODULE.
!
!     INPUT  DATA
!     GINO - DYNAMICS - EIGC DATA
!                       EIGP DATA
!                       EIGR DATA
!
!     OUTPUT DATA
!     GINO - EEDX     - EIGC DATA
!                       EIGP DATA
!                       EIGR DATA
!
!     PARAMETERS
!     INPUT  - DNAMIC - DYNAMICS DATA BLOCK INPUT FILE NUMBER
!              GBUF1  - GINO BUFFER
!              EEDX   - EEDX DATA BLOCK OUTPUT FILE NUMBER
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              IEIG   - EIGENVALUE EXTRACTION SET IDENTIFICATION NUMBER
!     OUTPUT - DRY    - MODULE OPERATION FLAG
!     OTHERS - EIGTYP - EIG CARD TYPE PROCESSING FLAG
!                     = 1, PROCESS EIGC DATA
!                     = 2, PROCESS EIGP DATA
!                     = 3, PROCESS EIGR DATA
!              EIGCP  - EIGC AND EIGP DATA ERROR FLAG
!                     = 0, NO EIGC, EIGP DATA - NO ERROR
!                     = 1, EIGC DATA ONLY - NO ERROR
!                     = 2, EIGP DATA ONLY - ERROR
!                     = 3, EIGC AND EIGP DATA - NO ERROR
!              EIGTRL - EEDX TRAILER
!              EIGCPR - DUMMY EIG(C,P,R) ARRAY
!              EIG    - ARRAY OF EIG(C,P,R) CARD TYPES AND HEADER
!                       INFORMATION
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              NWDS2R - NUMBER OF EIG(C,P,R) WORDS TO READ ON DYNAMIC
!                       DATA FILE
!
   DATA dnamic , eig , eedx/103 , 207 , 2 , 0 , 257 , 4 , 0 , 307 , 3 , 0 , 202/
   DATA modnam , letr/4HMRED , 4H1D   , 1HC , 1HP , 1HR/
   DATA komplx , kreal/4HCOMP , 4HREAL/
!
!     OPEN DYNAMICS, EEDX DATA BLOCKS
!
   IF ( Dry==-2 ) RETURN
   IF ( Usrmod ) GOTO 200
   CALL preloc(*300,Z(Gbuf1),dnamic)
   CALL gopen(eedx,Z(Gbuf2),1)
!
!     SET PROCESSING FLAGS
!
   eigtyp = 0
   eigcp = 0
   eigtrl(1) = eedx
   DO i = 2 , 7
      eigtrl(i) = 0
   ENDDO
 100  DO
!
!     INCREMENT EIG PROCESSING FLAG
!     EIGTYP .EQ. 1, PROCESS EIGC DATA
!     EIGTYP .EQ. 2, PROCESS EIGP DATA
!     EIGTYP .EQ. 3, PROCESS EIGR DATA
!
      eigtyp = eigtyp + 1
      IF ( eigtyp==4 ) THEN
!
!     CLOSE DYNAMICS, EEDX DATA BLOCKS
!
         CALL close(dnamic,1)
         CALL close(eedx,1)
!
!     TEST FOR EIG CARD ERRORS
!
         IF ( eigtrl(2)==0 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
            WRITE (Iprntr,99001) Ufm , Ieig , Oldnam
99001       FORMAT (A23,' 6628, NO EIGC OR EIGR CARD SPECIFIED FOR SET ID',I9,', SUBSTRUCTURE ',2A4,1H.)
            GOTO 700
         ELSEIF ( eigcp==2 ) THEN
            WRITE (Iprntr,99002) Ufm , Ieig , Oldnam
99002       FORMAT (A23,' 6629, NO EIGC DATA CARD SPECIFHIED WITH EIGP DATA ','CARD SET ID',I9,', SUBSTRUCTURE ',2A4,1H.)
            GOTO 700
         ELSE
!
!     WRITE EEDX DATA BLOCK TRAILER
!
            CALL wrttrl(eigtrl)
            EXIT
         ENDIF
!
!     SELECT EIG MODE
!
      ELSEIF ( Type(1)/=kreal .OR. eigtyp>=3 ) THEN
         IF ( Type(1)/=komplx .OR. eigtyp/=3 ) THEN
            DO i = 1 , 3
               eigcpr(i) = eig(i,eigtyp)
            ENDDO
!
!     LOCATE EIG(C,P,R) DATA CARD
!
            CALL locate(*100,Z(Gbuf1),eigcpr,itest)
!
!     SET UP EEDX DATA RECORD
!
            DO i = 1 , 3
               Z(Korbgn+i-1) = eigcpr(i)
            ENDDO
!
!     FIND CORRECT EIG(C,P,R) DATA CARD
!
            IF ( eigtyp==2 ) THEN
               nwds2r = 4
            ELSEIF ( eigtyp==3 ) THEN
               nwds2r = 18
            ELSE
               nwds2r = 10
            ENDIF
            DO
               CALL read(*400,*500,dnamic,Z(Korbgn+3),nwds2r,0,nowdsr)
               IF ( Z(Korbgn+3)==Ieig ) THEN
!
!     SELECT EIG PROCESSING MODE
!
                  IF ( eigtyp==2 ) THEN
!
!     WRITE EIGP DATA ONTO EEDX DATA BLOCK
!
                     CALL write(eedx,Z(Korbgn),7,1)
                     eigcp = eigcp + 2
                     eigtrl(2) = orf(eigtrl(2),4096)
                  ELSEIF ( eigtyp==3 ) THEN
!
!     WRITE EIGR DATA ONTO EEDX DATA BLOCK
!
                     CALL write(eedx,Z(Korbgn),21,1)
                     eigtrl(2) = orf(eigtrl(2),8192)
                  ELSE
!
!     WRITE EIGC DATA ONTO EEDX DATA BLOCK
!
                     CALL write(eedx,Z(Korbgn),13,0)
                     eigtrl(2) = orf(eigtrl(2),16384)
                     eigcp = eigcp + 1
                     DO
                        CALL read(*400,*500,dnamic,Z(Korbgn),7,0,nowdsr)
                        IF ( Z(Korbgn)==-1 ) THEN
                           CALL write(eedx,Z(Korbgn),7,1)
                           EXIT
                        ELSE
                           CALL write(eedx,Z(Korbgn),7,0)
                        ENDIF
                     ENDDO
                  ENDIF
                  EXIT
               ELSEIF ( eigtyp/=2 .AND. eigtyp/=3 ) THEN
                  DO
!
!     READ REST OF EIGC DATA
!
                     CALL read(*400,*500,dnamic,Z(Korbgn+3),7,0,nowdsr)
                     IF ( Z(Korbgn+3)==-1 ) EXIT
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDDO
 200  RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 300  imsg = -1
   GOTO 600
 400  imsg = -2
   IF ( eigtyp==2 ) GOTO 100
   WRITE (Iprntr,99003) Ufm , letr(eigtyp) , Ieig , Oldnam
!
99003 FORMAT (A23,' 6627, NO EIG',A1,' DATA CARD ','SPECIFIED FOR SET ID',I9,', SUBSTRUCTURE ',2A4,1H.)
   GOTO 600
 500  imsg = -3
   IF ( eigtyp==2 ) GOTO 100
   WRITE (Iprntr,99003) Ufm , letr(eigtyp) , Ieig , Oldnam
 600  CALL sofcls
   CALL mesage(imsg,dnamic,modnam)
   RETURN
 700  Dry = -2
   RETURN
!
END SUBROUTINE mred1d