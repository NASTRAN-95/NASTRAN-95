!*==apd12.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE apd12
   IMPLICIT NONE
   USE C_APD12C
   USE C_APD1C
   USE C_BITPOS
   USE C_SYSTEM
   USE C_TWO
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: dlb , lc , ls
   INTEGER :: i , i17 , i18 , i19 , i20 , ichord , igid2 , ispan , j , jchord , jspan , lca , n1 , nc , nextc , nigid , nigid1 ,    &
            & nigid2 , nx , pspa
   INTEGER , DIMENSION(20) :: iax
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Eid,Iax(1))
   DATA nam/4HAPD1 , 4H2   /
!
   i17 = Ibit(17)
   i18 = Ibit(18)
   i19 = Ibit(19)
   i20 = Ibit(20)
   pspa = orf(Itwo(i17),Itwo(i20))
   Usa = orf(pspa,Itwo(i18))
   Uk = orf(Itwo(i19),Itwo(i20))
   DO j = 1 , 2
      DO i = 1 , 6
         Auset(i,j) = Usa
      ENDDO
   ENDDO
   Auset(3,2) = Uk
   Auset(5,2) = Uk
   Ncam = ((Nca2-Nca1)+1)/16
   IF ( Nca1==0 ) Ncam = 0
   Ncam2 = ((Ca2e-Ca2s)+1)/16
   IF ( Ca2s==0 ) Ncam2 = 0
   lca = 16
!
!     CREATE IGID SEQUENCE ARRAY
!
   nigid1 = Next
   igid2 = Next
   nigid = Next
   nx = Nca1
   j = nigid1
   IF ( Ncam/=0 ) THEN
      DO i = 1 , Ncam
         iz(j) = iz(nx+7)
         j = j + 1
         iz(j) = nx
         nx = nx + lca
         j = j + 1
      ENDDO
!
!     SORT IGID ARRAY ON IGID
!
      CALL sort(0,0,2,1,iz(nigid1),2*Ncam)
   ENDIF
   IF ( Ncam2/=0 ) THEN
      nx = Ca2s
      nigid2 = j
      DO i = 1 , Ncam2
         iz(j) = iz(nx+7)
         j = j + 1
         iz(j) = nx
         nx = nx + lca
         j = j + 1
      ENDDO
      CALL sort(0,0,2,1,iz(nigid2),2*Ncam2)
      igid2 = nigid2
   ENDIF
   nextc = j
   IF ( Ncam/=0 ) THEN
      nigid = nigid1
!
!     OUTTER LOOP PROCESSES CAERO1 CARDS
!
      DO i = 1 , Ncam
!
!     SET APD1 INPUT COMMON BLOCK
!
         nc = iz(nigid+1) - 1
!
!     MOVE CAERO TO COMMON
!
         DO j = 1 , 16
            n1 = j + nc
            iax(j) = iz(n1)
         ENDDO
         Mcstm = Mcstm + 1
         iz(nc+2) = Mcstm
!
!     FIND PAERO1 CARD
!
         IF ( Npa1/=0 ) THEN
            DO j = Npa1 , Npa2 , 8
               Ippc = j
               IF ( Pid==iz(j) ) GOTO 20
            ENDDO
         ENDIF
         GOTO 300
 20      Xop = .25
         X1p = .75
         Alzo = 0.0
!
!     FIND AEFACT ARRAYS IF PRESENT
!
         jspan = Nspan
         jchord = Nchord
         IF ( Lspan/=0 ) THEN
            CALL apdoe(Lspan,iz,Naef1,Naef2,ispan,jspan)
            IF ( ispan==0 ) GOTO 100
            ispan = ispan + 1
            jspan = jspan - 1
         ENDIF
         IF ( Lchord/=0 ) THEN
            CALL apdoe(Lchord,iz,Naef1,Naef2,ichord,jchord)
            IF ( ichord==0 ) GOTO 200
            ichord = ichord + 1
            jchord = jchord - 1
         ENDIF
!
!     CHECK IF FIRST OR LAST ENTRY IN IGID SET
!
         ls = .FALSE.
         IF ( i==1 ) THEN
            ls = .TRUE.
         ELSEIF ( iz(nigid)/=iz(nigid-2) ) THEN
            ls = .TRUE.
         ENDIF
         lc = .FALSE.
         dlb = .FALSE.
         IF ( i/=Ncam ) THEN
            IF ( iz(nigid)==iz(nigid+2) ) GOTO 40
         ENDIF
         lc = .TRUE.
!
!     CHECK FOR CAERO2 ELEMENT
!
         IF ( Ncam2/=0 ) THEN
            IF ( nigid2>nextc ) THEN
               IF ( dlb ) lc = .FALSE.
            ELSE
               DO WHILE ( iz(nigid2)<=iz(nigid) )
                  IF ( iz(nigid)==iz(nigid2) ) dlb = .TRUE.
                  IF ( dlb ) THEN
                     IF ( dlb ) lc = .FALSE.
                     GOTO 40
                  ELSE
                     nigid2 = nigid2 + 2
                     IF ( nigid2>nextc ) THEN
                        IF ( dlb ) lc = .FALSE.
                        GOTO 40
                     ENDIF
                  ENDIF
               ENDDO
               IF ( dlb ) lc = .FALSE.
            ENDIF
         ENDIF
!
!     CALL APD1 TO MANUFACTURE BOXES
!
 40      CALL apd1(Z(ispan),jspan,Z(ichord),jchord,ls,lc)
         Nchord = jchord
         Nspan = jspan
         iz(nc+4) = Nspan
         iz(nc+5) = Nchord
         iz(nc+8) = 1
!
!     PROCESS CAERO2 WITH CAERO1
!
         IF ( dlb ) CALL apd2(1,iz(Next),iz(igid2),nextc,iz(nigid))
         nigid = nigid + 2
      ENDDO
   ENDIF
!
!     PROCESS CAERO2 CARDS NOT PROCESSED YET
!
   IF ( Ncam2/=0 ) CALL apd2(0,iz(Next),iz(igid2),nextc,iz(nigid))
   RETURN
 100  DO
      CALL emsg(0,2326,1,2,0)
      WRITE (Iut,99002) Eid , Lspan
!
!     ERROR MESSAGES
!
      CALL mesage(-61,0,nam)
   ENDDO
 200  CALL emsg(0,2327,1,2,0)
   WRITE (Iut,99002) Eid , Lchord
   CALL mesage(-61,0,nam)
   GOTO 100
 300  CALL emsg(0,2323,1,2,0)
   WRITE (Iut,99001) Pid , Eid
99001 FORMAT (10X,16HPAERO1 CARD NO. ,I8,31H REFERENCED BY CAERO1 CARD NO. ,I8,20H BUT DOES NOT EXIST.)
   CALL mesage(-61,0,nam)
   GOTO 100
99002 FORMAT (10X,19HCAERO1 ELEMENT NO. ,I8,28H REFERENCES AEFACT CARD NO. ,I8,22H WHICH DOES NOT EXIST.)
END SUBROUTINE apd12
