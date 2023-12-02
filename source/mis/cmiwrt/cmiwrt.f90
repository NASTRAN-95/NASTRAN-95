!*==cmiwrt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmiwrt(Icode,Name1,Name2,Loc,Nw,A,Iz)
   USE c_output
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Icode
   INTEGER , DIMENSION(2) :: Name1
   INTEGER , DIMENSION(2) :: Name2
   INTEGER :: Loc
   INTEGER :: Nw
   REAL , DIMENSION(1) :: A
   INTEGER , DIMENSION(1) :: Iz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , i1 , i2 , icomp , ifin , ip , ist , ist1 , j , kcode , kk , nl
   INTEGER , DIMENSION(32) :: ibits
   INTEGER , DIMENSION(96) , SAVE :: ih1 , ih2 , ih3 , ih4 , ih5 , ih6
   INTEGER , DIMENSION(6) :: ipl
   INTEGER , SAVE :: loap
   EXTERNAL andf , bitpat , page
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE WRITES FORMATTED SOF ITEMS.
!     ICODE = 1 FOR EQSS    ICODE = 2 FOR BGSS    ICODE = 3 FOR CSTM
!     ICODE = 4 FOR PLTS    ICODE = 5 FOR LODS    ICODE = 7 FOR LOAP
!     NAME1 IS PSEUDOSTRUCTURE NAME, NAME2 IS COMPONENT NAME
!
   DATA ih1/9*4H     , 4H EQS , 4HS IT , 4HEM F , 4HOR S , 4HUBST , 4HRUCT , 4HURE  , 2*4H     , 4H COM , 4HPONE , 4HNT   ,         &
       &11*4H     , 4HGRID , 4H POI , 4HNT   , 4H INT , 4HERNA , 4HL    , 4H  CO , 4HMPON , 4HENT  , 2*4H     , 4H GRI , 4HD PO ,   &
       &4HINT  , 4H  IN , 4HTERN , 4HAL   , 4H   C , 4HOMPO , 4HNENT , 2*4H     , 4H  GR , 4HID P , 4HOINT , 4H   I , 4HNTER ,      &
       &4HNAL  , 4H     , 4HCOMP , 4HONEN , 4HT    , 4H     , 4HID   , 4H     , 4H POI , 4HNT I , 4HD    , 4H     , 4H DOF ,        &
       &4*4H     , 4H ID  , 4H     , 4H  PO , 4HINT  , 4HID   , 4H     , 4H  DO , 4HF    , 3*4H     , 4H  ID , 4H     , 4H   P ,    &
       &4HOINT , 4H ID  , 4H     , 4H   D , 4HOF   , 4H    /
   DATA ih2/11*4H     , 4HBGSS , 4H ITE , 4HM FO , 4HR SU , 4HBSTR , 4HUCTU , 4HRE   , 21*4H     , 4HINTE , 4HRNAL , 4H     ,       &
      & 4H CST , 4HM ID , 4*4H     , 4H  C  , 4HO O  , 4HR D  , 4HI N  , 4HA T  , 4HE S  , 17*4H     , 4HPOIN , 4HT ID , 4H     ,   &
       &4H   N , 4HO.   , 3*4H     , 4HX1   , 3*4H     , 4HX2   , 3*4H     , 4HX3   , 8*4H    /
   DATA ih3/12*4H     , 4HCSTM , 4H ITE , 4HM FO , 4HR SU , 4HBSTR , 4HUCTU , 4HRE   , 13*4H     , 2*4H     , 4H CST , 4HM    ,     &
       &4HTYPE , 2*4H     , 4HC O  , 4HO R  , 4HD I  , 4HN A  , 4HT E  , 4HS    , 4HO F  , 4H  O  , 4HR I  , 4HG I  , 4HN    ,      &
       &3*4H     , 4H   T , 4H R A , 4H N S , 4H F O , 4H R M , 4H A T , 4H I O , 4H N   , 5*4H     , 4H  ID , 5*4H     , 4HX1   ,  &
      & 3*4H     , 4HX2   , 3*4H     , 4HX3   , 6*4H     , 4H   M , 4H A T , 4H R I , 4H X   , 5*4H    /
   DATA ih4/12*4H     , 4HPLTS , 4H ITE , 4HM FO , 4HR SU , 4HBSTR , 4HUCTU , 4HRE   , 13*4H     , 2*4H     , 4HCOMP , 4HONEN ,     &
       &4HT    , 4H     , 4H C O , 4H O R , 4H D I , 4H N A , 4H T E , 4H S   , 4H O F , 4H   O , 4HR I  , 4HG I  , 4HN    ,        &
       &3*4H     , 4H   T , 4H R A , 4H N S , 4H F O , 4H R M , 4H A T , 4H I O , 4H N   , 6*4H     , 4H  NA , 4HME   , 3*4H     ,  &
       &4H X1  , 3*4H     , 4H X2  , 3*4H     , 4H X3  , 6*4H     , 4H   M , 4H A T , 4H R I , 4H X   , 6*4H    /
   DATA ih5/12*4H     , 4HLODS , 4H ITE , 4HM FO , 4HR SU , 4HBSTR , 4HUCTU , 4HRE   , 18*4H     , 4H COM , 4HPONE , 4HNT   ,       &
      & 4H  NU , 4HMBER , 4H OF  , 21*4H     , 5*4H     , 4H   N , 4HAME  , 4H     , 4H  LO , 4HAD S , 4HETS  , 4H  L  , 4HO A  ,   &
       &4HD    , 4HS E  , 4HT    , 4HI D  , 4HE N  , 4HT I  , 4HF I  , 4HC A  , 4HT I  , 4HO N  , 4H  N  , 4HU M  , 4HB E  ,        &
      & 4HR S  , 5*4H    /
   DATA ih6/9*4H     , 4HEQSS , 4H ITE , 4HM -  , 4HSCAL , 4HAR I , 4HNDEX , 4H LIS , 4HT FO , 4HR SU , 4HBSTR , 4HUCTU , 4HRE   ,  &
      & 11*4H     , 4H INT , 4HERNA , 4HL    , 4H INT , 4HERNA , 4HL    , 4H  CO , 4HMPON , 4HENT  , 2*4H     , 4H  IN , 4HTERN ,   &
       &4HAL   , 4H  IN , 4HTERN , 4HAL   , 4H   C , 4HOMPO , 4HNENT , 2*4H     , 4H   I , 4HNTER , 4HNAL  , 4H   I , 4HNTER ,      &
       &4HNAL  , 4H     , 4HCOMP , 4HONEN , 4HT    , 4H POI , 4HNT I , 4HD    , 4H  SI , 4HL ID , 2*4H     , 4H DOF , 3*4H     ,    &
       &4H  PO , 4HINT  , 4HID   , 4H   S , 4HIL I , 4HD    , 4H     , 4H  DO , 4HF    , 2*4H     , 4H   P , 4HOINT , 4H ID  ,      &
       &4H     , 4HSIL  , 4HID   , 4H     , 4H   D , 4HOF   , 4H    /
   DATA loap/4HLOAP/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ist = Loc
         ifin = Loc + Nw - 1
         IF ( Icode==2 ) THEN
!
!     BGSS ITEM
!
            DO i = 1 , 96
               ihead(i) = ih2(i)
            ENDDO
            ihead(20) = Name1(1)
            ihead(21) = Name1(2)
            CALL page
            j = 0
            DO i = ist , ifin , 4
               j = j + 1
               nline = nline + 1
               IF ( nline>nlpp ) THEN
                  CALL page
                  nline = nline + 1
               ENDIF
               WRITE (outt,99001) j , Iz(i) , A(i+1) , A(i+2) , A(i+3)
99001          FORMAT (33X,I8,4X,I8,3X,3(3X,E13.6))
            ENDDO
         ELSEIF ( Icode==3 ) THEN
!
!     CSTM ITEM
!
            DO i = 1 , 96
               ihead(i) = ih3(i)
            ENDDO
            ihead(20) = Name1(1)
            ihead(21) = Name1(2)
            CALL page
            DO i = ist , ifin , 14
               nline = nline + 4
               IF ( nline>nlpp ) THEN
                  CALL page
                  nline = nline + 4
               ENDIF
               i1 = i + 2
               i2 = i + 13
               WRITE (outt,99002) Iz(i) , Iz(i+1) , (A(kk),kk=i1,i2)
99002          FORMAT (/10X,I8,3X,I4,3X,3(3X,E13.6),4X,3(3X,E13.6),/80X,3(3X,E13.6),/80X,3(3X,E13.6))
            ENDDO
         ELSEIF ( Icode==4 ) THEN
!
            DO i = 1 , 96
!
!     PLTS ITEM
!
               ihead(i) = ih4(i)
            ENDDO
            ihead(20) = Name1(1)
            ihead(21) = Name1(2)
            CALL page
            DO i = ist , ifin , 14
               nline = nline + 4
               IF ( nline>nlpp ) THEN
                  CALL page
                  nline = nline + 4
               ENDIF
               i1 = i + 2
               i2 = i + 13
               WRITE (outt,99003) Iz(i) , Iz(i+1) , (A(j),j=i1,i2)
99003          FORMAT (/14X,2A4,3X,3(3X,E13.6),4X,3(3X,E13.6)/77X,3(3X,E13.6),/77X,3(3X,E13.6))
            ENDDO
         ELSEIF ( Icode==5 .OR. Icode==7 ) THEN
!
!     LODS AND LOAP ITEMS
!
            DO i = 1 , 96
               ihead(i) = ih5(i)
            ENDDO
            ihead(20) = Name1(1)
            ihead(21) = Name1(2)
            IF ( Icode==7 ) ihead(13) = loap
            CALL page
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Icode==6 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Icode==8 ) THEN
!
!     EQSS - SCALER INDEX LIST
!
            DO i = 1 , 96
               ihead(i) = ih6(i)
            ENDDO
            ihead(22) = Name1(1)
            ihead(23) = Name1(2)
            CALL page
!
            ip = 0
            DO i = ist , ifin , 6
               nline = nline + 1
               IF ( nline>nlpp ) THEN
                  CALL page
                  nline = nline + 1
               ENDIF
               kcode = Iz(i+1)
               CALL bitpat(kcode,ibits(1))
               i2 = 2
               ipl(1) = ip + 1
               IF ( i+3<=ifin ) THEN
                  kcode = Iz(i+3)
                  CALL bitpat(kcode,ibits(3))
                  i2 = 4
                  ipl(3) = ip + 2
                  IF ( i+5<=ifin ) THEN
                     kcode = Iz(i+5)
                     CALL bitpat(kcode,ibits(5))
                     i2 = 6
                     ipl(5) = ip + 3
                  ENDIF
               ENDIF
               WRITE (outt,99007) (ipl(j),Iz(i+j-1),ibits(j),ibits(j+1),j=1,i2,2)
               ip = ip + 3
            ENDDO
         ELSE
!
!     EQSS ITEM
!
            DO i = 1 , 96
               ihead(i) = ih1(i)
            ENDDO
!
!     INSERT NAMES INTO HEADING
!
            ihead(17) = Name1(1)
            ihead(18) = Name1(2)
            ihead(22) = Name2(1)
            ihead(23) = Name2(2)
            CALL page
            IF ( Nw/=0 ) THEN
!
               DO i = ist , ifin , 9
                  nline = nline + 1
                  IF ( nline>nlpp ) THEN
                     CALL page
                     nline = nline + 1
                  ENDIF
                  icomp = andf(Iz(i+2),63)
                  CALL bitpat(icomp,ibits(1))
                  i2 = 3
                  IF ( i+5<=ifin ) THEN
                     icomp = andf(Iz(i+5),63)
                     CALL bitpat(icomp,ibits(4))
                     i2 = 6
                     IF ( i+8<=ifin ) THEN
                        icomp = andf(Iz(i+8),63)
                        CALL bitpat(icomp,ibits(7))
                        i2 = 9
                     ENDIF
                  ENDIF
                  WRITE (outt,99007) (Iz(i+j-1),Iz(i+j),ibits(j),ibits(j+1),j=1,i2,3)
               ENDDO
            ELSE
               WRITE (outt,99004)
99004          FORMAT (/30X,64HALL DEGREES OF FREEDOM FOR THIS COMPONENT HAVE BEEN REDUCED OUT.)
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (2)
         IF ( Nw==0 .OR. Nw==1 ) THEN
!
            nline = nline + 2
            IF ( nline>nlpp ) THEN
               CALL page
               nline = nline + 2
            ENDIF
            WRITE (outt,99005) Name2(1) , Name2(2)
99005       FORMAT (/26X,2A4,17X,32HNO LOAD SETS FOR THIS COMPONENT.)
         ELSE
            nl = Nw/5 + 3
            nline = nline + nl
            IF ( nline>nlpp ) THEN
               CALL page
               nline = nline + nl
            ENDIF
            ist1 = ist + 1
            WRITE (outt,99006) Name2(1) , Name2(2) , Iz(ist) , (Iz(j),j=ist1,ifin)
99006       FORMAT (/26X,2A4,3X,I8,5X,6(2X,I8)/(50X,2X,I8,2X,I8,2X,I8,2X,I8,2X,I8,2X,I8,/))
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99007 FORMAT (6X,I8,4X,I8,6X,A4,A2,2(13X,I8,4X,I8,6X,A4,A2))
END SUBROUTINE cmiwrt
