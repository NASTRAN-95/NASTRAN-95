
SUBROUTINE fbs(Zs,Zd)
   IMPLICIT NONE
   INTEGER Dbb(7) , Dbl(7) , Lcore , Nout , Rlcmpx(4) , Sysbuf , Words(4)
   REAL Dbu(7) , Dbx(7) , Prc(2) , Prec , Scrx , Sign
   COMMON /fbsx  / Dbl , Dbu , Dbb , Dbx , Lcore , Prec , Sign , Scrx
   COMMON /system/ Sysbuf , Nout
   COMMON /type  / Prc , Words , Rlcmpx
   DOUBLE PRECISION Zd(1)
   REAL Zs(1)
   INTEGER iremain , l46 , memavl , ncol , need , nrhv , nrhvwd , rc , typeb , typel
   REAL subnam
!
!     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND
!     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM
!     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR
!     SYSTEM OF EQUATIONS.
!
!     THE ARE TWO METHODS AVAILABLE FOR THIS PROCESS.
!     METHOD 1 - THIS METHOD READS AS MANY RIGHT HAND VECTORS INTO MEMORY
!                AS POSSIBLE, AND THEN READS THE LOWER TRIANGULAR MATRIX
!                USING GETSTR AND GETSTB TO SOLVE FOR THE SOLUTION VECTORS.
!                MORE THAN ONE PASS MAY BE REQUIRED IF INSUFFICIENT MEMORY
!                EXISTS FOR LOADING ALL RIGHT HAND VECTORS AT ONE TIME.
!                THIS METHOD IS THE OLDER OF THE TWO METHODS.
!                (SEE SUBROUTINES FBSF, FBSF1, FBSF2, FBSF3 AND FBSF4)
!     METHOD 2 - THIS METHOD IS THE SAME AS METHOD 1 WITH THE EXCEPTION
!                THAT MEMORY EXISTS FOR LOADING PART OR ALL OF THE LOWER
!                TRIANGULAR MATRIX INTO OPEN CORE AFTER LOADING ALL OF
!                THE RIGHT HAND VECTORS INTO OPEN CORE.  THIS METHOD
!                WILL ELIMINATE THE NEED TO READ THE LOWER TRIANGULAR
!                MATRIX TWICE (ONCE FORWARD AND ONCE BACKWARD).
!                (SEE SUBROUTINES FBSI, FBSI1, FBSI2, FBSI3 AND FBSI4)
!
!     THE SELECTION OF METHOD 1 OR 2 IS DEPENDENT UPON WHETHER
!     MEMORY EXISTS FOR READING THE LOWER TRIANGULAR MATRIX INTO MEMORY
!
!     SEE SUBROUTINES FBSF AND FBSI FOR OPEN CORE LAYOUTS
!
!
!     GENERAL INITIALIZATION
!
!   DIAG 46 FORCES METHOD ONE
!
   CALL sswtch(46,l46)
   IF ( l46==0 ) THEN
      ncol = Dbl(2)
      typel = Dbl(5)
      typeb = Dbb(5)
      rc = Rlcmpx(typeb)
!
!     NRHVWD = NUMBER OF WORDS REQUIRED FOR EACH RIGHT HAND VECTOR
!     NRHV   = NUMBER OF RIGHT HAND VECTORS
!
      nrhvwd = Words(typel)*ncol
      nrhv = Dbb(2)
!
! CHECK FOR RIGHT HAND VECTORS BEING THE IDENTITY MATRIX
!
      IF ( Dbb(4)==8 ) nrhv = ncol
!
! COMPUTE THE MEMORY TO READ ALL OF THE RIGHT HAND VECTORS INTO MEMORY
!
      need = nrhv*nrhvwd
      iremain = Lcore - 2*Sysbuf - need
!
! IF LESS THAN ONE COLUMN WORTH OF MEMORY AVAILABLE, USE METHOD ONE
!
      IF ( iremain>=nrhvwd ) THEN
!
!  METHOD TWO
!
         CALL fbsi(Zs,Zd)
         GOTO 99999
      ENDIF
   ENDIF
!
!  METHOD ONE - FIRST, CHECK FOR SUFFICIENT MEMORY FOR PROCESS
!
   memavl = Lcore - 2*Sysbuf - nrhvwd
   IF ( memavl<=0 ) CALL mesage(-8,-memavl,subnam)
   CALL fbsf(Zs,Zd)
99999 RETURN
END SUBROUTINE fbs