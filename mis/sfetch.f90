
SUBROUTINE sfetch(Name,Item,Irw,Itest)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Dirsiz , Io , Ioblk , Ioitcd , Iolbn , Iomode , Iopbn , Ioptr , Iosind , Mdi , Mdibl , Mdilbn ,        &
         & Mdipbn , Nbuff , Nout , Nxtdum(15)
   REAL Ditdum(6) , Ditup
   LOGICAL Mdiup
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /sof   / Ditdum , Io , Iopbn , Iolbn , Iomode , Ioptr , Iosind , Ioitcd , Ioblk , Mdi , Mdipbn , Mdilbn , Mdibl , Nxtdum ,&
                 & Ditup , Mdiup
   COMMON /sys   / Blksiz , Dirsiz
   COMMON /system/ Nbuff , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Irw , Item , Itest
   INTEGER Name(2)
!
! Local variable declarations
!
   INTEGER andf , itcode , ittype
   INTEGER ibl , idle , imdi , ird , irdwrt , itest1 , itm , iwrt , nmsbr(2)
   EXTERNAL andf
!
! End of declarations
!
!
!     POSITIONS THE SOF TO READ OR WRITE DATA ASSOCIATED WITH ITEM OF
!     SUBSTRUCTURE NAME.
!
   DATA idle , ird , iwrt/0 , 1 , 2/ , nmsbr/4HSFET , 4HCH  /
!
   CALL chkopn(nmsbr(1))
   CALL fdsub(Name(1),Iosind)
   IF ( Iosind==-1 ) THEN
!
!     NAME DOES NOT EXIST.
!
      Itest = 4
   ELSE
      Ioitcd = itcode(Item)
      IF ( Ioitcd==-1 ) THEN
!
!     ITEM IS AN ILLEGAL ITEM NAME.
!
         Itest = 5
      ELSE
!
!     CHECK IF ITEM IS A TABLE ITEM UNLESS SPECIAL CALL FROM MTRXO OR
!     MTRXI
!
         IF ( Irw>=0 ) THEN
            itm = ittype(Item)
            IF ( itm/=0 ) THEN
!
!     ATTEMPT TO OPERATE ON A MATRIX ITEM
!
               WRITE (Nout,99001) Sfm , Item , Name
99001          FORMAT (A25,' 6227, AN ATTEMPT HAS BEEN MADE TO OPERATE ON THE ','MATRIX ITEM ',A4,' OF SUBSTRUCTURE ',2A4,          &
                      &' USING SFETCH.')
               GOTO 100
            ENDIF
         ENDIF
         CALL fmdi(Iosind,imdi)
         Iolbn = 1
         Ioptr = Io + 1
         ibl = andf(Buf(imdi+Ioitcd),65535)
         irdwrt = iabs(Irw)
         IF ( irdwrt==2 ) THEN
!
!     WRITE OPERATION.
!
            IF ( ibl==0 .OR. ibl==65535 ) THEN
               itest1 = Itest - 1
               IF ( itest1==2 ) THEN
!
!     ITEM IS TO BE WRITTEN.  GET A FREE BLOCK AND UPDATE THE COMMON
!     BLOCK SOF.
!
                  CALL getblk(0,Ioblk)
                  IF ( Ioblk==-1 ) THEN
!
!     NO MORE BLOCKS ON SOF
!
                     WRITE (Nout,99002) Ufm
99002                FORMAT (A23,' 6223, SUBROUTINE SFETCH - THERE ARE NO MORE FREE ','BLOCKS AVAILABLE ON THE SOF.')
                     GOTO 100
                  ELSE
                     Iopbn = Ioblk
                     Iomode = iwrt
                     RETURN
                  ENDIF
               ELSE
!
!     ITEM IS TO BE PSEUDO-WRITTEN.
!
                  Buf(imdi+Ioitcd) = 65535
                  Mdiup = .TRUE.
                  RETURN
               ENDIF
            ELSE
!
!     ITEM HAS ALREADY BEEN WRITTEN.
!
               Itest = 1
            ENDIF
!
!     READ OPERATION.
!
         ELSEIF ( ibl==0 ) THEN
!
!     ITEM HAS NOT BEEN WRITTEN.
!
            Itest = 3
         ELSEIF ( ibl/=65535 ) THEN
!
!     UPDATE THE COMMON BLOCK SOF, AND BRING INTO CORE THE DESIRED BLOCK
!
            Itest = 1
            IF ( irdwrt/=3 ) THEN
               Iopbn = ibl
               Iomode = ird
               CALL sofio(ird,Iopbn,Buf(Io-2))
               RETURN
            ENDIF
         ELSE
!
!     ITEM WAS PSEUDO-WRITTEN.
!
            Itest = 2
         ENDIF
      ENDIF
   ENDIF
   Iomode = idle
   RETURN
 100  CALL sofcls
   CALL mesage(-61,0,0)
END SUBROUTINE sfetch