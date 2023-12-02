!*==delete.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE delete(Name,Itemx,Itest)
   USE c_itemdt
   USE c_sof
   USE c_sys
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Itemx
   INTEGER :: Itest
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibl , ii , imdi , isvps , isvss , itm , k
   INTEGER , SAVE :: is , ps , ss
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , fdsub , fmdi , itcode , retblk , rshift
!
! End of declarations rewritten by SPAG
!
!
!     DELETES ITEM WHICH BELONGS TO THE SUBSTRUCTURE NAME.  THE MDI IS
!     UPDATED ACCORDINGLY AND THE BLOCKS ON WHICH ITEM WAS WRITTEN ARE
!     RETURNED TO THE LIST OF FREE BLOCKS.  ITEST IS AN OUTPUT PARAMETER
!     WHICH TAKES ON ONE OF THE FOLLOWING VALUES
!
!              1  IF ITEM DOES EXIST
!              2  IF ITEM PSEUDO-EXISTS
!              3  IF ITEM DOES NOT EXIST
!              4  IF NAME DOES NOT EXIST
!              5  IF ITEM IS AN ILLEGAL ITEM NAME
!
!     THE BLOCKS OCCUPIED BY THE ITEM ARE RETURNED TO THE LIST OF FREE
!     BLOCKS IF THEY BELONG TO THE SPECIFIED SUBSTRUCTURE
!
!
   DATA is , ps , ss/1 , 1 , 1/
   DATA nmsbr/4HDELE , 4HTE  /
!
   CALL chkopn(nmsbr(1))
   CALL fdsub(Name(1),k)
   IF ( k==-1 ) THEN
!
!     NAME DOES NOT EXIST.
!
      Itest = 4
      RETURN
   ELSE
      CALL fmdi(k,imdi)
      ii = itcode(Itemx)
      IF ( ii==-1 ) THEN
!
!     ITEM IS AN ILLEGAL ITEM NAME.
!
         Itest = 5
      ELSE
         itm = ii - ifrst + 1
         ibl = andf(buf(imdi+ii),65535)
!                             55535 = 2**16 - 1
         IF ( ibl/=0 ) THEN
!
            buf(imdi+ii) = 0
            mdiup = .TRUE.
            IF ( ibl/=65535 ) THEN
!
!     ITEM DOES EXIST.
!
               Itest = 1
            ELSE
!
!     ITEM PSEUDO-EXISTS.
!
               Itest = 2
            ENDIF
            IF ( andf(buf(imdi+is),1073741824)==0 ) THEN
!
!     NAME IS A SECONDARY OR A PRIMARY SUBSTRUCTURE
!
               isvps = andf(buf(imdi+ps),1023)
!                               1023 = 2**10 - 1
               IF ( isvps==0 ) THEN
!
!     PRIMARY SUBSTRUCTURE
!
                  IF ( Itest==1 ) CALL retblk(ibl)
                  DO
                     isvss = rshift(andf(buf(imdi+ss),1048575),10)
!                                      1048575 = 2*20 - 1
                     IF ( isvss==0 ) RETURN
                     CALL fmdi(isvss,imdi)
                     IF ( andf(buf(imdi+ii),65535)==ibl ) THEN
                        buf(imdi+ii) = 0
                        mdiup = .TRUE.
                     ENDIF
                  ENDDO
               ELSE
!
!     SECONDARY SUBSTRUCTURE
!
                  IF ( Itest/=1 ) RETURN
                  IF ( item(5,itm)/=0 ) CALL retblk(ibl)
                  RETURN
               ENDIF
            ELSE
!                           1073741824 = 2**30
!
!     IMAGE SUBSTRUCTURE
!
               IF ( Itest/=1 ) RETURN
               IF ( item(4,itm)/=0 ) CALL retblk(ibl)
               RETURN
            ENDIF
         ELSE
!
!     ITEM DOES NOT EXIST.
!
            Itest = 3
            RETURN
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE delete
