!*==apd12.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE apd12
   IMPLICIT NONE
   USE c_apd12c
   USE c_apd1c
   USE c_bitpos
   USE c_system
   USE c_two
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Eid,Iax(1))
   DATA nam/4HAPD1 , 4H2   /
!
   i17 = ibit(17)
   i18 = ibit(18)
   i19 = ibit(19)
   i20 = ibit(20)
   pspa = orf(itwo(i17),itwo(i20))
   usa = orf(pspa,itwo(i18))
   uk = orf(itwo(i19),itwo(i20))
   DO j = 1 , 2
      DO i = 1 , 6
         auset(i,j) = usa
      ENDDO
   ENDDO
   auset(3,2) = uk
   auset(5,2) = uk
   ncam = ((nca2-nca1)+1)/16
   IF ( nca1==0 ) ncam = 0
   ncam2 = ((ca2e-ca2s)+1)/16
   IF ( ca2s==0 ) ncam2 = 0
   lca = 16
!
!     CREATE IGID SEQUENCE ARRAY
!
   nigid1 = next
   igid2 = next
   nigid = next
   nx = nca1
   j = nigid1
   IF ( ncam/=0 ) THEN
      DO i = 1 , ncam
         iz(j) = iz(nx+7)
         j = j + 1
         iz(j) = nx
         nx = nx + lca
         j = j + 1
      ENDDO
!
!     SORT IGID ARRAY ON IGID
!
      CALL sort(0,0,2,1,iz(nigid1),2*ncam)
   ENDIF
   IF ( ncam2/=0 ) THEN
      nx = ca2s
      nigid2 = j
      DO i = 1 , ncam2
         iz(j) = iz(nx+7)
         j = j + 1
         iz(j) = nx
         nx = nx + lca
         j = j + 1
      ENDDO
      CALL sort(0,0,2,1,iz(nigid2),2*ncam2)
      igid2 = nigid2
   ENDIF
   nextc = j
   IF ( ncam/=0 ) THEN
      nigid = nigid1
!
!     OUTTER LOOP PROCESSES CAERO1 CARDS
!
      DO i = 1 , ncam
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
         mcstm = mcstm + 1
         iz(nc+2) = mcstm
!
!     FIND PAERO1 CARD
!
         IF ( npa1/=0 ) THEN
            DO j = npa1 , npa2 , 8
               ippc = j
               IF ( pid==iz(j) ) GOTO 20
            ENDDO
         ENDIF
         GOTO 300
 20      xop = .25
         x1p = .75
         alzo = 0.0
!
!     FIND AEFACT ARRAYS IF PRESENT
!
         jspan = nspan
         jchord = nchord
         IF ( lspan/=0 ) THEN
            CALL apdoe(lspan,iz,naef1,naef2,ispan,jspan)
            IF ( ispan==0 ) GOTO 100
            ispan = ispan + 1
            jspan = jspan - 1
         ENDIF
         IF ( lchord/=0 ) THEN
            CALL apdoe(lchord,iz,naef1,naef2,ichord,jchord)
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
         IF ( i/=ncam ) THEN
            IF ( iz(nigid)==iz(nigid+2) ) GOTO 40
         ENDIF
         lc = .TRUE.
!
!     CHECK FOR CAERO2 ELEMENT
!
         IF ( ncam2/=0 ) THEN
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
 40      CALL apd1(z(ispan),jspan,z(ichord),jchord,ls,lc)
         nchord = jchord
         nspan = jspan
         iz(nc+4) = nspan
         iz(nc+5) = nchord
         iz(nc+8) = 1
!
!     PROCESS CAERO2 WITH CAERO1
!
         IF ( dlb ) CALL apd2(1,iz(next),iz(igid2),nextc,iz(nigid))
         nigid = nigid + 2
      ENDDO
   ENDIF
!
!     PROCESS CAERO2 CARDS NOT PROCESSED YET
!
   IF ( ncam2/=0 ) CALL apd2(0,iz(next),iz(igid2),nextc,iz(nigid))
   RETURN
 100  DO
      CALL emsg(0,2326,1,2,0)
      WRITE (iut,99002) eid , lspan
!
!     ERROR MESSAGES
!
      CALL mesage(-61,0,nam)
   ENDDO
 200  CALL emsg(0,2327,1,2,0)
   WRITE (iut,99002) eid , lchord
   CALL mesage(-61,0,nam)
   GOTO 100
 300  CALL emsg(0,2323,1,2,0)
   WRITE (iut,99001) pid , eid
99001 FORMAT (10X,16HPAERO1 CARD NO. ,I8,31H REFERENCED BY CAERO1 CARD NO. ,I8,20H BUT DOES NOT EXIST.)
   CALL mesage(-61,0,nam)
   GOTO 100
99002 FORMAT (10X,19HCAERO1 ELEMENT NO. ,I8,28H REFERENCES AEFACT CARD NO. ,I8,22H WHICH DOES NOT EXIST.)
END SUBROUTINE apd12
