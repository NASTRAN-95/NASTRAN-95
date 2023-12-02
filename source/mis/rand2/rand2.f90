!*==rand2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rand2(File,Ilist,Load,If,Len,Llist)
   USE C_SYSTEM
   USE C_XMSSG
   IMPLICIT NONE
   INTEGER Ibuf , Nout
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm
   INTEGER File , If , Len , Llist , Load
   INTEGER Data(1) , Ilist(2)
   INTEGER data1(100) , filex , i , ichk , idata(50) , idr(10) , ielem , ieltp , iflag , ifmt(2,84) , ifmtp , ifmtt(11) , ioldld ,  &
         & ip1 , itb(180) , itb1(137) , itb2(145) , itemp(5) , j , k , l , len1 , lenx , ll , m , mid(2,10) , name(2)
   INTEGER :: spag_nextblock_1
!
!     READS FILE UNTIL IT FINDS DATA RECORD IN LIST - RETURNS LOAD,
!     IF, AND LEN
!
   !>>>>EQUIVALENCE (itb1(1),itb(1)) , (itb2(1),itb(138))
   DATA ifmtt/1 , 11 , 41 , 55 , 61 , 99 , 121 , 155 , 181 , 199 , 237/
   DATA ioldld/0/
   DATA ifmt/1 , 1 , -1 , -1 , 1 , 1 , 1 , 1 , 1 , 1 , 6 , 2 , 6 , 2 , 6 , 2 , 0 , 3 , 1 , 1 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 0 , 6 ,  &
      & 2 , 0 , 3 , 6 , 2 , 6 , 2 , 6 , 2 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,    &
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 7 , 5 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 0 , 8 , 0 , 8 , 0 ,  &
      & 8 , 0 , 8 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 0 , 9 , 0 , 10 , 0 , 11 , 0 , 6 , 0 , 8 , -1 , -1 , -1 , -1 , -1 , -1 , &
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 0 , 3 , 0 , 3 , 7 , 2 , -1 , -1 , -1 , -1 , -1 , -1 , &
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , 7 , 2 , -1 , -1/
!
!     IFMT TABLE (ELEM ID IN GPTABD ORDER) HAS 2 WORDS PER ENTRY
!        WORD1  FORCE  FORMAT POINTER INTO IFMTT TABLE
!        WORD2  STRESS FORMAT POINTER INTO IFMTT TABLE
!
!     IFMTT TABLE  HAS ONE ENTRY PER FORMAT TYPE
!        THE ENTRY IS THE BEGINNING OF THE FORMAT IN THE ITB TABLE
!
   DATA itb1/6 , 3 , 5 , 4 , 6 , 0 , 1 , 1 , 2 , 2 , 16 , 3 , 4 , 12 , 5 , 13 , 6 , 14 , 7 , 8 , 16 , 9 , 17 , 10 , 18 , 0 , 1 , 2 ,&
      & 2 , 3 , 3 , 4 , 4 , 5 , 6 , 6 , 7 , 7 , 8 , 8 , 8 , 3 , 6 , 4 , 7 , 5 , 8 , 0 , 1 , 1 , 2 , 2 , 3 , 3 , 4 , 3 , 4 , 0 , 1 , &
      & 1 , 20 , 3 , 4 , 5 , 6 , 7 , 12 , 13 , 14 , 15 , 16 , 8 , 9 , 10 , 11 , 17 , 18 , 19 , 20 , 0 , 1 , 2 , 3 , 4 , 5 , 1 , 2 , &
      & 3 , 4 , 5 , 6 , 7 , 8 , 9 , 6 , 7 , 8 , 9 , 12 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 0 , 1 , 2 , 3 , 4 , 5 , 1 , 2 , &
      & 3 , 4 , 5 , 18 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18/
   DATA itb2/0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 14 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 ,    &
      & 13 , 14 , 0 , 1 , 2 , 3 , 4 , 5 , 6 , 1 , 2 , 3 , 4 , 5 , 6 , 10 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 0 , 1 , 2 , 3 , 4 , 1 , &
      & 2 , 3 , 4 , 20 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 20 , 0 , 1 , 2 , 3 , 4 , 5 , &
      & 6 , 7 , 8 , 9 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 24 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 ,&
      & 18 , 19 , 20 , 21 , 22 , 23 , 24 , 0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 ,    &
      & 10 , 11/
   DATA name/4HRAND , 4H2   /
   DATA mid/3001 , 4HDISP , 3010 , 4HVELO , 3011 , 4HACCE , 3002 , 4HLOAD , 3003 , 4HSPCF , 3004 , 4HELFO , 3005 , 4HSTRE , 3015 ,  &
       &4HDISP , 3016 , 4HVELO , 3017 , 4HACCE/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         filex = File
         spag_nextblock_1 = 2
      CASE (2)
!
!     POSITION TO + READ ID RECORD
!
         CALL fwdrec(*60,File)
         CALL read(*60,*40,File,idr,10,1,i)
         idr(5) = idr(5)/10
!
!     IDR(5) = 10*ELEM.ID + DEV.CODE
!     IDR(2) = GINO FILE 3004, 3005 ETC.
!     CONVERT MAJOR ID TO MNEMONIC
!
         DO i = 1 , 10
            IF ( idr(2)==mid(1,i) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         ip1 = -7
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     CHECK FOR  MID
!
         IF ( Ilist(1)==mid(2,i) ) THEN
            ielem = i
!
!     LOOK FOR ID IN LIST
!
            SPAG_Loop_1_1: DO i = 1 , Llist , 5
               IF ( idr(5)<Ilist(i+1) ) EXIT SPAG_Loop_1_1
               IF ( idr(5)==Ilist(i+1) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     ID IS IN  LIST
!
         i = i - 1
         IF ( i/=0 ) THEN
!
!     FLIP LIST ORDER
!
            m = 0
            ll = i
            SPAG_Loop_1_2: DO
!
!     SAVE CURRENT STUFF AT END OF LIST
!
               DO j = 1 , 5
                  l = i + j
                  itemp(j) = Ilist(l)
               ENDDO
!
!     PUSH DOWN LIST
!
               DO j = 1 , ll
                  k = i - j + 1
                  Ilist(k+5) = Ilist(k)
               ENDDO
!
!     RESTORE CURRENT STUFF IN FRONT OF LIST, IN FLIPPED ORDER
!
               DO j = 1 , 5
                  k = m + j
                  Ilist(k) = itemp(j)
               ENDDO
!
!     AGAIN
!
               IF ( Ilist(i+7)/=itemp(2) ) EXIT SPAG_Loop_1_2
               m = m + 5
               i = i + 5
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     FOUND IT
!
!     IDR( 3) = ELEMENT TYPE
!     IDR( 4) = SUBCASE NO.
!     IDR( 9) = FORMAT CODE, 1=REAL, 2=REAL/IMAG, 3=MAG/PHASE
!     IDR(10) = NO. OF WORDS PER ENTRY
!     IELEM   = 6 OR 7 FOR ELFORCE (OEFC2) OR STRESS (OESC2)
!
         Load = idr(4)
         If = 0
         IF ( idr(9)==3 ) If = 1
         len1 = idr(10)
         Len = len1
         ieltp = idr(3)
         IF ( ielem>=6 .AND. ielem<=7 ) THEN
!
!     EXECUTE THIS PORTION FOR STRESSES AND FORCES
!
!     FIND FORMAT TYPE
!
            IF ( ifmt(1,ieltp)==-1 ) THEN
               WRITE (Nout,99001) Uwm , ieltp
99001          FORMAT (A25,' 2185, CURRENTLY RAND2 ROUTINE DOES NOT PROCESS ','ELEMENT TYPE',I5)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     PICK UP FORMAT POINTER
!
               ifmtp = ifmt(ielem-5,ieltp)
               IF ( ifmtp==0 ) THEN
                  ip1 = -7
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  j = ifmtt(ifmtp)
!
!     SAVE EXTERNAL DATA LENGTH
!
                  Len = itb(j)
!
!     SAVE MAP OF ITB
!
                  DO i = 1 , len1
                     k = j + i - 1
                     idata(i) = itb(k)
                  ENDDO
!
!     CONVERT POINTERS TO NEW DATA VALUES
!
                  IF ( ioldld/=0 ) THEN
                     IF ( ioldld/=Load ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  ioldld = Load
                  SPAG_Loop_1_3: DO i = 1 , Llist , 5
                     IF ( Ilist(i)/=mid(2,ielem) .OR. Ilist(i+1)/=Ilist(2) ) EXIT SPAG_Loop_1_3
                     k = Ilist(i+2)
                     IF ( k>len1 ) THEN
!
!     POINTER OUT OF RANGE
!
                        CALL mesage(52,Ilist(i),Ilist(i+1))
                        k = len1
                     ENDIF
                     k = j + k - 1 + len1
                     Ilist(i+2) = itb(k)
                  ENDDO SPAG_Loop_1_3
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         ichk = 1234321
         lenx = Len
!
!     FILE AND LEN WERE SAVED LOCALLY IN FILEX AND LENX, SO THAT THEY
!     CAN BE USED IN RAND2A
!
         RETURN
!
!
         ENTRY rand2a(Data)
!     ===================
!
!     WILL OBTAIN DATA AND REFORMAT IF NECESSARY
!
!     READ DATA
!
         IF ( ichk/=1234321 ) CALL mesage(-37,0,name)
         CALL read(*20,*40,filex,Data(1),len1,0,iflag)
         IF ( ielem<6 ) RETURN
!
!     APPLY DATA MAP  I.E. REARRANGE DATA ACCORDING TO DATA MAP
!
         DO i = 1 , lenx
            data1(i) = 0
         ENDDO
         data1(1) = Data(1)
         DO i = 2 , len1
            j = idata(i)
            data1(j) = Data(i)
         ENDDO
!WKBR 9/93 DO 190 I = 1,LEN
         DO i = 1 , lenx
            Data(i) = data1(i)
         ENDDO
         RETURN
!
!     FILE ERRORS
!
 20      ip1 = -2
         spag_nextblock_1 = 6
      CASE (6)
         CALL mesage(ip1,filex,name)
 40      ip1 = -3
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
! 950 LOAD = 0
 60      CALL rewind(filex)
!WKBI 9/93
         WRITE (Nout,99002)
99002    FORMAT (' THE FOLLOWING I/O ERROR OCCURRED MOST LIKELY BECAUSE',/,                                                         &
                &' THERE WAS A PLOT REQUEST FOR A POINT THAT DOES NOT EXIST.')
         CALL mesage(-2,File,name)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rand2
