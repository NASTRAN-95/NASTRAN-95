
SUBROUTINE fbsrdm(Mcb,Icore,Rcore,Dcore,Memtot,Buff,Lasind,Ipos)
   IMPLICIT NONE
   DOUBLE PRECISION Dxl(1)
   INTEGER Ksystm(65) , Nout , Rd , Rdrew , Rew , Wrt , Wrtrew
   REAL Rxl(1)
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Dxl
   INTEGER Lasind , Memtot
   INTEGER Buff(2) , Icore(1) , Ipos(7) , Mcb(7)
   DOUBLE PRECISION Dcore(1)
   REAL Rcore(1)
   INTEGER i , iblk(20) , iblock , icbp , iclr , ii , incr , index , jcol , jrow , mem , mindex , ncol , ntms , ntms2 , ntwds ,     &
         & ntype
!
!  FBSRDM - This routine will store an entire matrix in memory
!           if sufficient memory exists.  The matrix
!           is stored in memory according to the following scheme:
!           (Subroutine FERRDM is very similiar to this subroutine)
!
!  1st word = current column number
!  2nd word = number of terms in string (ntms)
!  3rd word           }
!     |               }
!     |               } = actual
!     |               }   matrix
!     |               }   string
!     |               }   data
!     |               }
!     |               }
!  3+(ntms*prec)      } (where prec=1 for s.p.;  =2 for d.p. )
!  3+(ntms*prec)+1 = row position of first element in above string
!  3+(ntms*prec)+2 = number of terms in ABOVE string (ntms)
!
!  The above data repeats for all strings within a column and then
!  for all columns in the matrix.
!
!  Argument list :
!     MCB    - Matrix control block for input matrix
!     ICORE  - Memory for storage of data (integer)
!     RCORE  - Same location as ICORE but real single reference
!     DCORE  - Same location as ICORE but real double reference
!     MEMTOT - Total amount of memory available for this data
!     BUFF   - Buffer allocation for input matrix
!     LASIND - Memory index of last string stored in memory
!     IPOS   - 6 word array with the following information
!              (1) = last column read into memory
!              (2) = block number of following column not read into memory
!              (3) = current logical record pointer for following column
!                    not read into memory
!              (4) = current buffer pointer for following record not read
!                    into memory
!              (5) = last block number in file
!              (6) = current logical record pointer for last record in file
!              (7) = current buffer pointer for last record in file
!
   EQUIVALENCE (Ksystm(2),Nout)
   EQUIVALENCE (Dxl,Rxl)
   mem = 1
   ncol = Mcb(2)
   ntype = Mcb(5)
   incr = 1
   IF ( ntype==2 .OR. ntype==3 ) incr = 2
   IF ( ntype==4 ) incr = 4
   ntwds = 0
   Ipos(1) = ncol
   DO i = 2 , 7
      Ipos(i) = 0
   ENDDO
   DO i = 1 , 20
      iblk(i) = 0
   ENDDO
   iblk(1) = Mcb(1)
   iblk(9) = 1
   iblk(10) = 1
   CALL gopen(Mcb,Buff,Rdrew)
   CALL rewind(Mcb)
   CALL skprec(Mcb,1)
   DO jcol = 1 , ncol
      iblk(8) = -1
      Lasind = mem - 1
      CALL dscpos(Mcb,iblock,iclr,icbp)
      DO
         CALL getstr(*100,iblk(1))
         index = iblk(5)
         ntms = iblk(6)
         jrow = iblk(4)
         ntwds = ntwds + 4 + ntms*incr
         IF ( ntwds>Memtot ) GOTO 200
         Icore(mem) = jcol
         Icore(mem+1) = ntms
         IF ( ntype==2 ) THEN
            mindex = mem/2 + 1
            DO ii = 1 , ntms
               Dcore(mindex+ii) = Dxl(index+ii-1)
            ENDDO
            mem = mem + 2 + ntms*2
         ELSEIF ( ntype==3 ) THEN
            mindex = mem + 1
            ntms2 = ntms*2
            DO ii = 1 , ntms2
               Rcore(mindex+ii) = Rxl(index+ii-1)
            ENDDO
            mem = mem + 2 + ntms2
         ELSEIF ( ntype==4 ) THEN
            mindex = mem/2 + 1
            ntms2 = ntms*2
            DO ii = 1 , ntms2
               Dcore(mindex+ii) = Dxl(index+ii-1)
            ENDDO
            mem = mem + 2 + ntms*4
         ELSE
            mindex = mem + 1
            DO ii = 1 , ntms
               Rcore(mindex+ii) = Rxl(index+ii-1)
            ENDDO
            mem = mem + 2 + ntms
         ENDIF
         Icore(mem) = jrow
         Icore(mem+1) = ntms
         mem = mem + 2
         CALL endget(iblk(1))
      ENDDO
 100  ENDDO
   Lasind = mem - 1
   GOTO 300
 200  Ipos(1) = jcol - 1
   Ipos(2) = iblock
   Ipos(3) = iclr
   Ipos(4) = icbp
   CALL skprec(Mcb,ncol-jcol+1)
   CALL dscpos(Mcb,iblock,iclr,icbp)
   Ipos(5) = iblock
   Ipos(6) = iclr
   Ipos(7) = icbp
 300  CALL close(Mcb,Rew)
END SUBROUTINE fbsrdm
