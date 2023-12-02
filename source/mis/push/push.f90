!*==push.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE push(In,Bcd,Icol,Nchar,Flag)
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: In
   INTEGER , DIMENSION(1) :: Bcd
   INTEGER :: Icol
   INTEGER :: Nchar
   INTEGER :: Flag
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , cperwd , minus
   INTEGER :: digit , i , iadd , iadd1 , ib1 , ibl , ic , icl , ij , isave , ish , iwrd , ix , ixtra , k , lcol , lwrd , m , m1 ,   &
            & m2 , m3 , nin , nx
   LOGICAL , SAVE :: first
   INTEGER , DIMENSION(18) :: ii
   INTEGER , DIMENSION(10) , SAVE :: numbs
   EXTERNAL klshft , krshft , orf
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE IS USED TO PLACE BCD CHARACTERS OR INTEGERS FROM II
!     ARRAY INTO THE BCD STRING . IF FLAG = 1 AN INTEGER IS INPUT
!
   DATA numbs/1H0 , 1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9/
   DATA cperwd/4/ , first/.TRUE./ , blank/1H / , minus/4H-   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( first ) THEN
            first = .FALSE.
!
!     REMOVE BLANKS FROM NUMBERS, AND ZERO FILL
!
            ish = ncpw - 1
            DO i = 1 , 10
               isave = krshft(numbs(i),ish)
               numbs(i) = klshft(isave,ish)
            ENDDO
            isave = krshft(minus,ish)
            minus = klshft(isave,ish)
            nx = ncpw - cperwd
            ixtra = nx*nbpc
            ibl = 0
            IF ( nx/=0 ) THEN
               ib1 = krshft(blank,ish)
               DO i = 1 , nx
                  ibl = orf(ibl,klshft(ib1,i-1))
               ENDDO
            ENDIF
         ENDIF
!
         IF ( Nchar<=0 ) RETURN
         IF ( Nchar+Icol>128 ) THEN
!
            WRITE (iout,99001) ufm , Nchar , In
99001       FORMAT (A23,' 6015. TOO MANY CHARACTERS TO BE INSERTED IN A DMAP',' LINE',/6X,4H N =,I8,6X,6HWORD =,A4)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nin = (Nchar-1)/cperwd + 1
            DO i = 1 , nin
               ii(i) = In(i)
            ENDDO
            IF ( Flag/=1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     INTEGER HAS BEEN INPUT - 1 WORD ONLY
!
!     FIND POWER OF 10 = NUMBER OF CHARACTERS
!
            ix = iabs(In(1))
            DO i = 1 , 8
               ix = ix/10
               IF ( ix==0 ) GOTO 10
            ENDDO
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
 10         ic = i
            IF ( In(1)<0 ) ic = ic + 1
            IF ( ic>Nchar ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ii(2) = blank
            ix = iabs(In(1))
            IF ( ic>cperwd ) THEN
!
!     NUMBER TAKES TWO WORDS
!
               m = ic - cperwd
               ii(2) = krshft(blank,m)
               DO i = 1 , m
                  ij = ix/10
                  digit = iabs(ix-10*ij) + 1
                  ix = ij
                  iadd = numbs(digit)
                  ii(2) = orf(ii(2),krshft(iadd,m-i))
               ENDDO
!
               ic = cperwd
            ENDIF
!
!     FIRST WORD SET HERE FOR BOTH CASES
!
            ii(1) = krshft(blank,ic)
            DO i = 1 , ic
               IF ( i/=ic .OR. In(1)>=0 ) THEN
                  ij = ix/10
                  digit = iabs(ix-10*ij) + 1
                  ix = ij
                  iadd = numbs(digit)
                  ii(1) = orf(ii(1),krshft(iadd,ic-i))
               ENDIF
            ENDDO
            IF ( In(1)<0 ) ii(1) = orf(ii(1),minus)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         iwrd = (Icol-1)/cperwd + 1
         icl = Icol - (iwrd-1)*cperwd
         lwrd = (Icol+Nchar-2)/cperwd + 1
         lcol = Icol + Nchar - (lwrd-1)*cperwd - 1
         m1 = (icl-1)*nbpc
         m2 = cperwd*nbpc - m1
         m3 = m2 + (ncpw-cperwd)*nbpc
!
!     M1 IS THE NUMBER OF BITS FOR THE  FIRST SET OF CHARACTERS
!     M2 IS THE NUMBER OF BITS FOR THE SECOND SET OF CHARACTERS
!     M3 IS THE NUMBER OF BITS FOR THE RIGHT HALF OF THE WORD
!
!     IADD IS THE CURRENT WORKING WORD, IADD1 IS THE SPILL
!
         isave = krshft(Bcd(iwrd),m3/nbpc)
         iadd1 = klshft(isave,m3/nbpc)
         k = 0
         DO i = iwrd , lwrd
            k = k + 1
!
!     SPLIT INPUT WORD INTO TWO SETS
!
!     MOVE LEFT HALF TO RIGHT SIDE OF IADD AND ADD IADD1
!
            isave = krshft(ii(k),(m1+ixtra)/nbpc)
            iadd = orf(klshft(isave,ixtra/nbpc),iadd1)
!
!     IF THIS ISNT THE LAST WORD MOVE THE RIGHT HALF TO IADD1 AND INSERT
!
            IF ( i<lwrd ) THEN
               isave = krshft(ii(k),ixtra/nbpc)
               iadd1 = klshft(isave,m3/nbpc)
!
               Bcd(i) = orf(iadd,ibl)
            ENDIF
!
         ENDDO
!
!     LAST WORD PROCESSED HERE, REMOVE EXTRA CHARACTERS
!
         ish = ncpw - lcol
         isave = krshft(iadd,ish)
         iadd = klshft(isave,ish)
         isave = klshft(Bcd(lwrd),lcol)
         Bcd(lwrd) = orf(iadd,krshft(isave,lcol))
         RETURN
      CASE (3)
         WRITE (iout,99002) ufm , In
99002    FORMAT (A23,' 6016. TOO MANY DIGITS TO BE INSERTED IN DMAP.',2X,'VALUE =',I12)
         spag_nextblock_1 = 4
      CASE (4)
!
         nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE push
