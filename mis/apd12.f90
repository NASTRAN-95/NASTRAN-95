
SUBROUTINE apd12
   IMPLICIT NONE
   INTEGER Acpt , Acsid , Auset(6,2) , Bgpa , Buf10 , Buf11 , Buf12 , Ca2e , Ca2s , Ca3e , Ca3s , Ca4e , Ca4s , Cidbx , Cp , Cstma ,&
         & Ecta , Eid , Gpla , Iacs , Iax(20) , Ibit(64) , Igid , Ippc , Isiln , Itwo(32) , Iut , Iz(1) , Key(5) , Lchord , Left ,  &
         & Lspan , Mcstm , Naef1 , Naef2 , Nasb , Nca1 , Nca2 , Ncam , Ncam2 , Nchord , Ncrd , Ncst1 , Ncst2 , Next , Npa1 , Npa2 , &
         & Nspan , Pa2e , Pa2s , Pa3e , Pa3s , Pa4e , Pa4s , Pid , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sila , Silb , Uk , Usa , Useta
   REAL Alzo , Sysbuf , X1 , X12 , X1p , X4 , X43 , Xop , Y1 , Y4 , Z(1) , Z1 , Z4
   COMMON /apd12c/ Key , Auset , Usa , Uk , Ncam2 , Nasb , Ippc
   COMMON /apd1c / Eid , Pid , Cp , Nspan , Nchord , Lspan , Lchord , Igid , X1 , Y1 , Z1 , X12 , X4 , Y4 , Z4 , X43 , Xop , X1p ,  &
                 & Alzo , Mcstm , Ncst1 , Ncst2 , Cidbx , Acsid , Iacs , Silb , Ncrd , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Ecta ,    &
                 & Bgpa , Gpla , Useta , Sila , Cstma , Acpt , Buf10 , Buf11 , Buf12 , Next , Left , Isiln , Ncam , Naef1 , Naef2 , &
                 & Nca1 , Nca2 , Ca2s , Ca2e , Ca3s , Ca3e , Ca4s , Ca4e , Npa1 , Npa2 , Pa2s , Pa2e , Pa3s , Pa3e , Pa4s , Pa4e
   COMMON /bitpos/ Ibit
   COMMON /system/ Sysbuf , Iut
   COMMON /two   / Itwo
   COMMON /zzzzzz/ Z
   LOGICAL dlb , lc , ls
   INTEGER i , i17 , i18 , i19 , i20 , ichord , igid2 , ispan , j , jchord , jspan , lca , n1 , nam(2) , nc , nextc , nigid ,       &
         & nigid1 , nigid2 , nx , pspa
   INTEGER orf
   EXTERNAL orf
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
         Iz(j) = Iz(nx+7)
         j = j + 1
         Iz(j) = nx
         nx = nx + lca
         j = j + 1
      ENDDO
!
!     SORT IGID ARRAY ON IGID
!
      CALL sort(0,0,2,1,Iz(nigid1),2*Ncam)
   ENDIF
   IF ( Ncam2/=0 ) THEN
      nx = Ca2s
      nigid2 = j
      DO i = 1 , Ncam2
         Iz(j) = Iz(nx+7)
         j = j + 1
         Iz(j) = nx
         nx = nx + lca
         j = j + 1
      ENDDO
      CALL sort(0,0,2,1,Iz(nigid2),2*Ncam2)
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
         nc = Iz(nigid+1) - 1
!
!     MOVE CAERO TO COMMON
!
         DO j = 1 , 16
            n1 = j + nc
            Iax(j) = Iz(n1)
         ENDDO
         Mcstm = Mcstm + 1
         Iz(nc+2) = Mcstm
!
!     FIND PAERO1 CARD
!
         IF ( Npa1/=0 ) THEN
            DO j = Npa1 , Npa2 , 8
               Ippc = j
               IF ( Pid==Iz(j) ) GOTO 20
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
            CALL apdoe(Lspan,Iz,Naef1,Naef2,ispan,jspan)
            IF ( ispan==0 ) GOTO 100
            ispan = ispan + 1
            jspan = jspan - 1
         ENDIF
         IF ( Lchord/=0 ) THEN
            CALL apdoe(Lchord,Iz,Naef1,Naef2,ichord,jchord)
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
         ELSEIF ( Iz(nigid)/=Iz(nigid-2) ) THEN
            ls = .TRUE.
         ENDIF
         lc = .FALSE.
         dlb = .FALSE.
         IF ( i/=Ncam ) THEN
            IF ( Iz(nigid)==Iz(nigid+2) ) GOTO 40
         ENDIF
         lc = .TRUE.
!
!     CHECK FOR CAERO2 ELEMENT
!
         IF ( Ncam2/=0 ) THEN
            IF ( nigid2>nextc ) THEN
               IF ( dlb ) lc = .FALSE.
            ELSE
               DO WHILE ( Iz(nigid2)<=Iz(nigid) )
                  IF ( Iz(nigid)==Iz(nigid2) ) dlb = .TRUE.
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
         Iz(nc+4) = Nspan
         Iz(nc+5) = Nchord
         Iz(nc+8) = 1
!
!     PROCESS CAERO2 WITH CAERO1
!
         IF ( dlb ) CALL apd2(1,Iz(Next),Iz(igid2),nextc,Iz(nigid))
         nigid = nigid + 2
      ENDDO
   ENDIF
!
!     PROCESS CAERO2 CARDS NOT PROCESSED YET
!
   IF ( Ncam2/=0 ) CALL apd2(0,Iz(Next),Iz(igid2),nextc,Iz(nigid))
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