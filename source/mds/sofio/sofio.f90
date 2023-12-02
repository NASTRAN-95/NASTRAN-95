!*==sofio.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sofio(Isop,Iblknm,Buf)
   IMPLICIT NONE
   USE I_GINOX
   USE I_DSIOF
   USE C_SOFCOM
   USE C_SOFDSN
   USE C_SYS
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Isop
   INTEGER :: Iblknm
   INTEGER , DIMENSION(10) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(80) :: dsname
   INTEGER :: ialloc , iblk , icerr , ifile , iop , isize , istat , k , lasfil , num , numblk
!
! End of declarations rewritten by SPAG
!
   IF ( lensof(1)==0 ) THEN
      numblk = 1
      IF ( lenwpb/=0 ) numblk = Isysbf/lenwpb
      DO k = 1 , Nfiles
         lensof(k) = 0
         dsname = Sofdsn(k)
         CALL dsinqr(dsname,istat,isize)
         IF ( istat/=0 ) lensof(k) = Filsiz(k)
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
            WRITE (Iwr,99003) Sofdsn(k) , Filsiz(k) , lensof(k)
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
            CALL dsread(90,Buf(4),nbuff,iblk)
         ELSE
            IF ( (iblk-lensof(ifile))>1 ) THEN
               num = iblk - lensof(ifile) - 1
               IF ( num/=0 ) THEN
                  DO k = 1 , num
                     lensof(ifile) = lensof(ifile) + 1
                     CALL dswrit(90,Buf(4),nbuff,lensof(ifile),icerr)
                     IF ( icerr/=0 ) GOTO 30
                  ENDDO
               ENDIF
            ENDIF
            CALL dswrit(90,Buf(4),nbuff,iblk,icerr)
            IF ( icerr==0 ) THEN
               IF ( iblk>lensof(ifile) ) lensof(ifile) = iblk
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
