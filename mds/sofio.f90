
SUBROUTINE sofio(Isop,Iblknm,Buf)
   IMPLICIT NONE
   INCLUDE 'GINOX.COM'
   INCLUDE 'DSIOF.COM'
!
! COMMON variable declarations
!
   REAL Avblks , Blksiz , Dirsiz , Supsiz
   CHARACTER*4 Filnam(10)
   INTEGER Filsiz(10) , Hiblk , Isysbf , Iwr , Nfiles
   CHARACTER*80 Sofdsn(10)
   COMMON /sofcom/ Nfiles , Filnam , Filsiz
   COMMON /sofdsn/ Sofdsn
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks , Hiblk
   COMMON /system/ Isysbf , Iwr
!
! Dummy argument declarations
!
   INTEGER Iblknm , Isop
   INTEGER Buf(10)
!
! Local variable declarations
!
   CHARACTER*80 dsname
   INTEGER ialloc , iblk , icerr , ifile , iop , isize , istat , k , lasfil , num , numblk
!
! End of declarations
!
   IF ( Lensof(1)==0 ) THEN
      numblk = 1
      IF ( Lenwpb/=0 ) numblk = Isysbf/Lenwpb
      DO k = 1 , Nfiles
         Lensof(k) = 0
         dsname = Sofdsn(k)
         CALL dsinqr(dsname,istat,isize)
         IF ( istat/=0 ) Lensof(k) = Filsiz(k)
      ENDDO
      lasfil = 0
   ENDIF
   IF ( Isop==7 ) THEN
      CALL dsclos(90)
      lasfil = 0
   ELSE
      iblk = Iblknm
      IF ( iblk>0 ) THEN
         ifile = 0
         DO k = 1 , Nfiles
            IF ( iblk<=Filsiz(k) ) THEN
               ifile = k
               GOTO 20
            ELSE
               iblk = iblk - Filsiz(k)
            ENDIF
         ENDDO
         WRITE (Iwr,99001) Iblknm
99001    FORMAT (' *** SUBSTRUCTURING ERROR - BLOCK NUMBER OUT OF RANGE ',' -  BLOCK NUMBER IS ',I10)
         WRITE (Iwr,99002)
99002    FORMAT (//,' THE FOLLOWING SOF FILES WERE AVAILABLE',//)
         DO k = 1 , Nfiles
            WRITE (Iwr,99003) Sofdsn(k) , Filsiz(k) , Lensof(k)
99003       FORMAT (' FILE ',A72' HAS ',I10,' BLOCKS - BLOCKS USED ',I10)
         ENDDO
         CALL mesage(-61,0,0)
 20      IF ( lasfil/=ifile ) THEN
            IF ( lasfil/=0 ) CALL dsclos(90)
            ialloc = numblk*Filsiz(ifile)
            dsname = Sofdsn(ifile)
            iop = 0
            CALL dsopen(dsname,90,iop)
            lasfil = ifile
         ENDIF
         IF ( Isop==1 ) THEN
            CALL dsread(90,Buf(4),Nbuff,iblk)
         ELSE
            IF ( (iblk-Lensof(ifile))>1 ) THEN
               num = iblk - Lensof(ifile) - 1
               IF ( num/=0 ) THEN
                  DO k = 1 , num
                     Lensof(ifile) = Lensof(ifile) + 1
                     CALL dswrit(90,Buf(4),Nbuff,Lensof(ifile),icerr)
                     IF ( icerr/=0 ) GOTO 30
                  ENDDO
               ENDIF
            ENDIF
            CALL dswrit(90,Buf(4),Nbuff,iblk,icerr)
            IF ( icerr==0 ) THEN
               IF ( iblk>Lensof(ifile) ) Lensof(ifile) = iblk
               IF ( Iblknm>Hiblk ) Hiblk = Iblknm
               GOTO 99999
            ENDIF
 30         IF ( icerr==28 ) WRITE (Iwr,99004)
99004       FORMAT (///,' INSUFFICIENT SPACE FOR SOF FILE ON DEFAULT',' DEVICE---JOB ABORTED.')
            IF ( icerr/=28 ) WRITE (Iwr,99005)
99005       FORMAT (///,' I/O ERROR OCCURRED ON SOF FILE, JOB ABORTED')
            CALL mesage(-61,0,0)
         ENDIF
      ENDIF
   ENDIF
99999 END SUBROUTINE sofio
