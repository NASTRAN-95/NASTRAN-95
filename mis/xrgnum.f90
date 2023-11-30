
SUBROUTINE xrgnum
   IMPLICIT NONE
   REAL Dum(98)
   INTEGER Ichar(80) , Icol , Icount , Idmap , Ierror , Ignore , Ind , Iphase , Irestr , Iscr , Istate , Isysbf , Itype , Limit(2) ,&
         & Member(2) , Name(2) , Nsubst , Num(2) , Number , Nument , Optape , Record(20)
   CHARACTER*23 Ufm
   COMMON /system/ Isysbf , Optape , Dum
   COMMON /xmssg / Ufm
   COMMON /xrgdxx/ Irestr , Nsubst , Iphase , Icol , Number , Itype , Istate , Ierror , Num , Ind , Nument , Record , Ichar ,       &
                 & Limit , Icount , Idmap , Iscr , Name , Member , Ignore
   INTEGER blank , i , ifrcol , j , k , newnum , nums(10)
!
!     XRGNUM PROCESSES THE NUMBER ON A CARD OR FILE NAME TABLE ENTRY
!     THIS ROUTINE IS CALLED ONLY BY XRGDTB
!
!     WRITTEN BY  RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       /SYSTEM/
!         OPTAPE       UNIT NUMBER FOR THE OUTPUT PRINT FILE
!       /XRGDXX/
!         ICHAR        CONTAINS THE CARD IMAGE IN 80A1 FORMAT
!         ICOL         CURRENT COLUMN BEING PROCESSED
!         RECORD       CONTAINS THE CARD IMAGE IN 20A4 FORMAT
!
!     OUTPUT
!       /XRGDXX/
!         ICOL         CURRENT COLUMN BEING PROCESSED
!         NUMBER       VALUE OF THE NUMBER IN INTEGER FORMAT
!
!     LOCAL VARIABLES
!         BLANK          CONTAINS THE VALUE 1H
!         IFRCOL         FIRST COLUMN TO BE EXAMINED BY XRGNUM
!         NEWNUM         INTEGER VALUE OF THE CHARACTER IN THE CURRENT
!                        COLUMN
!         NUMS           CONTAINS THE ALPHA VALUES 1,2,...0
!
!     THE CARD IS SCANED TO FIND THE VALUE OF THE NUMBER IN THE FIRST
!     FIELD OF THE CARD
!
!     MESSAGE 8030 MAY BE ISSUED
!
   DATA nums/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   DATA blank/1H /
!
   ifrcol = Icol
   Number = 0
   DO WHILE ( Icol<80 )
      IF ( Ichar(Icol)==blank ) THEN
         Icol = Icol + 1
         IF ( Number/=0 ) EXIT
         CYCLE
      ELSE
         DO k = 1 , 10
            IF ( Ichar(Icol)==nums(k) ) THEN
               newnum = mod(k,10)
               Number = Number*10 + newnum
               GOTO 50
            ENDIF
         ENDDO
         Number = 0
         j = 0
         k = 1
         WRITE (Optape,99001) Ufm , ifrcol , Record , j , (i,i=1,8) , k , (j,i=1,8)
99001    FORMAT (A23,' 8030, EXPECTED AN INTEGER NEAR COLUMN',I3,' IN THE FOLLOWING CARD',//20X,20A4,/,(20X,I1,I9,7I10))
         EXIT
      ENDIF
 50   Icol = Icol + 1
   ENDDO
END SUBROUTINE xrgnum
