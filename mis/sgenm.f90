
SUBROUTINE sgenm(Ntype,Ifile,Sfile,Ofile,Icode,Ocode,Ctypes,Ctypeo)
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Idry , Iptr , Name(2) , Nono , Nss , Nz , Outt , Sysbuf , Z(1)
   CHARACTER*23 Ufm
   COMMON /blank / Idry , Name
   COMMON /sgencm/ Nono , Nss , Iptr , Buf1 , Buf2 , Buf3 , Nz
   COMMON /system/ Sysbuf , Outt
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Ifile , Ntype , Ofile , Sfile
   INTEGER Ctypeo(2,8) , Ctypes(2,8) , Icode(4,1) , Ocode(4,1)
   INTEGER card(3) , i , i1 , idx , j , k , m , nlimit(3) , nwds , subnam(2)
   REAL file
!
!     THIS SUBROUTINE MERGES CONVERTED SUBSTRUCTURING DATA WITH EXISTING
!     NASTRAN DATA
!
!     INPUTS
!     NTYPE  - NUMBER OF DIFFERENT SUBSTRUCTURING CARDS
!     IFILE  - INPUT FILE NAME
!     SFILE  - SCRATCH FILE NAME
!     OFILE  - OUTPUT FILE NAME
!     ICODE  - LOCATE CODES FOR INPUT CARD TYPES
!     OCODE  - LOCATE CODES FOR OUTPUT CARD TYPES
!     CTYPES - BCD NAMES OF SUBSTRUCTURING CARDS
!     CTYPEO - BCD NAMES OF CORRESPONDING NASTRAN CARDS
!
   DATA nlimit/3*2147483647/
   DATA subnam/4HSGEN , 4HM   /
!
!     OPEN FILES
!
   CALL gopen(Ifile,Z(Buf1),0)
   CALL gopen(Sfile,Z(Buf2),0)
   CALL gopen(Ofile,Z(Buf3),1)
!
!     READ HEADER FROM IFILE - DETERMINE IF SUBSTRUCTURING OR NASTRAN
!     CARD
!
   file = Ifile
 100  DO
      CALL read(*800,*900,Ifile,card,3,0,idx)
      IF ( card(1)==nlimit(1) ) GOTO 400
      DO i = 1 , Ntype
         IF ( Icode(1,i)==card(1) .AND. Icode(2,i)==card(2) ) THEN
!
!     SKIP RECORD IF SUBSTRUCTURING CARD
!
            CALL fwdrec(*400,Ifile)
            GOTO 200
         ENDIF
      ENDDO
      DO i = 1 , Ntype
         IF ( Ocode(1,i)==card(1) .AND. Ocode(2,i)==card(2) ) THEN
!
!     FATAL ERROR IF BOTH SUBSTRUCTURING AND NASTRAN CARDS
!
            IF ( Icode(4,i)==0 ) EXIT
            Nono = 1
            j = Ocode(4,i)
            WRITE (Outt,99001) Ufm , Name , (Ctypes(k,j),k=1,2) , (Ctypeo(k,j),k=1,2)
!
99001       FORMAT (A23,' 6330, SOLUTION SUBSTRUCTURE ',2A4,3H - ,2A4,' AND ',2A4,' CARDS CANNOT BE USED TOGETHER.',/30X,           &
                   &'USE EITHER ONE, BUT NOT BOTH.')
            CALL fwdrec(*400,Ifile)
            GOTO 200
         ENDIF
      ENDDO
!
!     COPY RECORD FROM IFILE TO OUTPUT
!
      CALL write(Ofile,card,3,0)
      DO
         CALL read(*800,*300,Ifile,Z,Nz,0,nwds)
         CALL write(Ofile,Z,Nz,0)
      ENDDO
 200  ENDDO
 300  CALL write(Ofile,Z,nwds,1)
   GOTO 100
!
!     COPY RECORD FROM SFILE TO OUTPUT
!
 400  i1 = 1
 500  DO i = i1 , Ntype
      IF ( Icode(4,i)==1 ) GOTO 600
   ENDDO
!
!     CLOSE FILES
!
   CALL write(Ofile,nlimit,3,1)
   CALL close(Ifile,1)
   CALL close(Sfile,1)
   CALL close(Ofile,1)
   RETURN
 600  CALL fread(Sfile,card,3,0)
   CALL write(Ofile,Ocode(1,i),3,0)
   file = Sfile
   DO
      CALL read(*800,*700,Sfile,Z,Nz,0,nwds)
      CALL write(Ofile,Z,Nz,0)
   ENDDO
 700  CALL write(Ofile,Z,nwds,1)
   i1 = i + 1
   GOTO 500
!
!     ERRORS
!
 800  m = -2
   GOTO 1000
 900  m = -3
 1000 CALL mesage(m,file,subnam)
   RETURN
END SUBROUTINE sgenm
