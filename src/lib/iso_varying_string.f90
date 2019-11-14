! $Id: aniso_varying_string.f90 2415 2016-09-02 04:37:11Z ian $
!> @file
!! Defines the aniso_varying_string module.

! ******************************************************************************
! *                                                                            *
! * iso_varying_string.f90                                                     *
! *                                                                            *
! * Copyright (c) 2003, Rich Townsend <rhdt@bartol.udel.edu>                   *
! * All rights reserved.                                                       *
! *                                                                            *
! * Redistribution and use in source and binary forms, with or without         *
! * modification, are permitted provided that the following conditions are     *
! * met:                                                                       *
! *                                                                            *
! *  * Redistributions of source code must retain the above copyright notice,  *
! *    this list of conditions and the following disclaimer.                   *
! *  * Redistributions in binary form must reproduce the above copyright       *
! *    notice, this list of conditions and the following disclaimer in the     *
! *    documentation and/or other materials provided with the distribution.    *
! *                                                                            *
! * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS    *
! * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  *
! * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR     *
! * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR           *
! * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,      *
! * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,        *
! * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR         *
! * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF     *
! * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING       *
! * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         *
! * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               *
! *                                                                            *
! ******************************************************************************
!
! Author    : Rich Townsend <rhdt@bartol.udel.edu>
! Synopsis  : Definition of iso_varying_string module, conformant to the API 
!             specified in ISO/IEC 1539-2:2000 (varying-length strings for 
!             Fortran 95). 
! Version   : 1.3-F
! Thanks    : Lawrie Schonfelder (bugfixes and design pointers), Walt Brainerd
!             (conversion to F).

! Subsequently modified by Ian Harvey to include F2003 language capabilities.

!*******************************************************************************
!!
!> Extension of ISO 1539-2:2000 - varying length character strings, that uses
!! the F2003 deferred length character feature to extend varying_string 
!! support.
!!
!! Note that operation on a varying_string object that has not been defined
!! will result in access to an unallocated value.

MODULE iso_varying_string

  IMPLICIT NONE

  PRIVATE

  !-----------------------------------------------------------------------------
  ! Parameter definitions

  !> Our one and only supported character kind.
  INTEGER, PARAMETER :: ck = KIND('a')

  !> Incremental length of buffer extracted with each IO read.
  !!
  !! This is only used in the specific procedures for get, when the 
  !! file connection is not stream access and a terminating character 
  !! set is not provided.
  INTEGER, PARAMETER, PRIVATE :: GET_BUFFER_LEN = 256

  !> Debugging mode - setting this to true enables some run time assertions.
  LOGICAL, PARAMETER :: dbg_flag = .TRUE.

  !-----------------------------------------------------------------------------
  ! Type definitions

  !> Extension of ISO 1539-2:2000 variable length string type.
  !
  ! We don't anticipate use of this type as a parent type, hence no operations 
  ! are implemented though bindings and nearly all supporting procedures take 
  ! non-polymorphic dummy arguments (the defined io procedures take a 
  ! polymorphic argument due to F2008 C936, we've had to move them into this 
  ! type due to compiler bugs).
  !
  ! Whether this is reasonable is perhaps up for debate.
  !
  ! The type is not parameterised on the kind value of the stored characters, 
  ! despite this apparently being an obvious parameterisation.  The reason 
  ! for this is that we would need to duplicate all the procedures below 
  ! for each character kind that we wanted to support, and we have no idea 
  ! ahead of time what character kind values need to be supported.  Instead, 
  ! the suggestion is that users simply copy the source of this module, 
  ! rename the module and edit the `ck` parameter definition above.
  TYPE, PUBLIC :: varying_string
     !> Actual storage for the string.
     CHARACTER(:,KIND=ck), ALLOCATABLE, PUBLIC :: chars
   CONTAINS
     PROCEDURE, PRIVATE :: write_formatted
     GENERIC :: WRITE(FORMATTED) => write_formatted

     PROCEDURE, PRIVATE :: read_formatted
     GENERIC :: READ(FORMATTED) => read_formatted

     PROCEDURE, PRIVATE :: write_unformatted
     GENERIC :: WRITE(UNFORMATTED) => write_unformatted

     PROCEDURE, PRIVATE :: read_unformatted
     GENERIC :: READ(UNFORMATTED) => read_unformatted
  END TYPE varying_string

  !-----------------------------------------------------------------------------
  ! Interface blocks

  !> Assignment to a varying_string.
  !!
  !! Forms are available for assignment from a varying_string to a character 
  !! string and from a character string to a varying_string.  
  !!
  !! In the first case, if the length of the varying_string is greater than 
  !! the length of the character string then only the left most characters of 
  !! the varying_string are assigned to the character string.  If the length 
  !! of the varying_string is less than the character string being assigned 
  !! to, then the character string is padded on the right with blanks.
  !!
  !! In the second case the length of the varying_string will be that of the 
  !! character string.
  !!
  !! All forms of the interface are elemental.
  INTERFACE ASSIGNMENT(=)
     !> @details Assign a varying_string to a varying_string.  See 
     !! aniso_varying_string::op_assign_CH_VS.    
     MODULE PROCEDURE op_assign_CH_VS
     !> @details Assign a character variable to a varying_string.  See 
     !! aniso_varying_string::op_assign_VS_CH.
     MODULE PROCEDURE op_assign_VS_CH
  END INTERFACE ASSIGNMENT(=)

  !> Concatenate two strings to form a varying_string.
  !!
  !! Forms are available to concatenate a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is always a varying_string.  All forms of the interface are 
  !! elemental.
  INTERFACE OPERATOR(//)
     !> @details Concatenate two varying_strings. See 
     !! aniso_varying_string::op_concat_VS_VS.
     MODULE PROCEDURE op_concat_VS_VS
     !> @details Concatenate a CHARACTER string and a varying_string.  See 
     !! aniso_varying_string::op_concat_CH_VS.
     MODULE PROCEDURE op_concat_CH_VS
     !> @details Concatenate a varying_string and a CHARACTER string.  See 
     !! aniso_varying_string::op_concat_VS_CH.
     MODULE PROCEDURE op_concat_VS_CH
  END INTERFACE OPERATOR(//)

  !> Equality comparison of a varying_string to other strings.
  !!
  !! Forms are available to compare a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is LOGICAL.  All forms of the interface are elemental.
  !!
  !! If the arguments do not match in length then the shorter argument is 
  !! padded on the right with blanks.  The collation sequence is processor 
  !! defined but consistent with the other comparison operators.
  INTERFACE OPERATOR(==)
     !> @details Equality comparison of two varying_strings.  See 
     !! aniso_varying_string::op_eq_VS_VS.
     MODULE PROCEDURE op_eq_VS_VS
     !> @details Equality comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::op_eq_CH_VS.
     MODULE PROCEDURE op_eq_CH_VS
     !> @details Equality comparison of a varying_string with a 
     !! CHARACTER string.  See aniso_varying_string::op_eq_VS_CH.
     MODULE PROCEDURE op_eq_VS_CH
  END INTERFACE OPERATOR(==)

  !> Inequality comparison of a varying_string to other strings.
  !!
  !! Forms are available to compare a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is LOGICAL.  All forms of the interface are elemental.
  !!
  !! If the arguments do not match in length then the shorter argument is 
  !! padded on the right with blanks.  The collation sequence is processor 
  !! defined but consistent with the other comparison operators.
  INTERFACE OPERATOR(/=)
     !> @details Inequality comparison of two varying_strings.  See 
     !! aniso_varying_string::op_ne_VS_VS.
     MODULE PROCEDURE op_ne_VS_VS
     !> @details Inequality comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::op_ne_CH_VS.
     MODULE PROCEDURE op_ne_CH_VS
     !> @details Inequality comparison of a varying_string with a CHARACTER 
     !! string.  See aniso_varying_string::op_ne_VS_CH.
     MODULE PROCEDURE op_ne_VS_CH
  END INTERFACE OPERATOR(/=)

  !> Less than comparison of a varying_string to other strings.
  !!
  !! Forms are available to compare a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is LOGICAL.  All forms of the interface are elemental.
  !!
  !! If the arguments do not match in length then the shorter argument is 
  !! padded on the right with blanks.  The collation sequence is processor 
  !! defined but consistent with the other comparison operators.
  INTERFACE OPERATOR(<)
     !> @details Less than comparison of two varying_strings.  See 
     !! aniso_varying_string::op_ne_VS_VS.
     MODULE PROCEDURE op_lt_VS_VS
     !> @details Inequality comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::op_ne_CH_VS.
     MODULE PROCEDURE op_lt_CH_VS
     !> @details Inequality comparison of a varying_string with a CHARACTER 
     !! string.  See aniso_varying_string::op_ne_VS_CH.
     MODULE PROCEDURE op_lt_VS_CH
  END INTERFACE OPERATOR(<)

  !> Less than or equal comparison of a varying_string to other strings.
  !!
  !! Forms are available to compare a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is LOGICAL.  All forms of the interface are elemental.
  !!
  !! If the arguments do not match in length then the shorter argument is 
  !! padded on the right with blanks.  The collation sequence is processor 
  !! defined but consistent with the other comparison operators.
  INTERFACE OPERATOR(<=)
     !> @details Less than or equal comparison of two varying_strings.  See 
     !! aniso_varying_string::op_ne_VS_VS.
     MODULE PROCEDURE op_le_VS_VS
     !> @details Less than or equal comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::op_ne_CH_VS.
     MODULE PROCEDURE op_le_CH_VS
     !> @details Less than or equal comparison of a varying_string with a 
     !! CHARACTER string.  See aniso_varying_string::op_ne_VS_CH.
     MODULE PROCEDURE op_le_VS_CH
  END INTERFACE OPERATOR(<=)

  !> Greater than or equal comparison of a varying_string to other strings.
  !!
  !! Forms are available to compare a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is LOGICAL.  All forms are elemental.
  !!
  !! If the arguments do not match in length then the shorter argument is 
  !! padded on the right with blanks.  The collation sequence is processor 
  !! defined but consistent with the other comparison operators.
  INTERFACE OPERATOR(>=)
     !> @details Greater than or equal comparison of two varying_strings.  See 
     !! aniso_varying_string::op_ne_VS_VS.
     MODULE PROCEDURE op_ge_VS_VS
     !> @details Greater than or equal comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::op_ne_CH_VS.
     MODULE PROCEDURE op_ge_CH_VS
     !> @details Greater than or equal comparison of a varying_string with a 
     !! CHARACTER string.  See aniso_varying_string::op_ne_VS_CH.
     MODULE PROCEDURE op_ge_VS_CH
  END INTERFACE OPERATOR(>=)

  !> Greater than comparison of a varying_string to other strings.
  !!
  !! Forms are available to compare a varying_string with a varying_string 
  !! and a varying_string with a character string, or vice versa.  The return 
  !! type is LOGICAL.  All forms are elemental.
  !!
  !! If the arguments do not match in length then the shorter argument is 
  !! padded on the right with blanks.  The collation sequence is processor 
  !! defined but consistent with the other comparison operators.
  INTERFACE OPERATOR(>)
     !> @details Greater than comparison of two varying_strings.  See 
     !! aniso_varying_string::op_ne_VS_VS.
     MODULE PROCEDURE op_gt_VS_VS
     !> @details Greater than comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::op_ne_CH_VS.
     MODULE PROCEDURE op_gt_CH_VS
     !> @details Greater than comparison of a varying_string with a 
     !! CHARACTER string.  See aniso_varying_string::op_ne_VS_CH.
     MODULE PROCEDURE op_gt_VS_CH
  END INTERFACE OPERATOR(>)

  ! ifort 16.0.2 appears to munge the accessibility of the following 
  ! for namelist operations.

  !> Implementation of UDDTIO (formatted output) for a varying_string.
  !INTERFACE WRITE(FORMATTED)
  !  MODULE PROCEDURE write_formatted
  !END INTERFACE WRITE(FORMATTED)

  !> Implementation of UDDTIO (formatted input) for a varying_string.
  !INTERFACE READ(FORMATTED)
  !  MODULE PROCEDURE read_formatted
  !END INTERFACE READ(FORMATTED)

  !> Implementation of UDDTIO (unformatted output) for a varying_string.
  !INTERFACE WRITE(UNFORMATTED)
  !  MODULE PROCEDURE write_unformatted
  !END INTERFACE WRITE(UNFORMATTED)

  !> Implementation of UDDTIO (unformatted input) for a varying_string.
  !INTERFACE READ(UNFORMATTED)
  !  MODULE PROCEDURE read_unformatted
  !END INTERFACE READ(UNFORMATTED)

  !> Adjusts a varying_string to the left, removing any leading blanks and 
  !! inserting the same number of trailing blanks.
  !!
  !! The interface has the form of an elemental function adjustl(@a string) 
  !! returning a varying_string.  This is an extension of the ADJUSTL 
  !! intrinsic.
  INTERFACE adjustl
     !> @details See aniso_varying_string::adjustl_.
     MODULE PROCEDURE adjustl_
  END INTERFACE adjustl

  !> Adjusts a varying_string to the right, removing any trailing blanks and
  !! inserting the same number of leading blanks.
  !!
  !! The interface has the form of an elemental function adjustr(@a string) 
  !! returning a varying_string.  This is an extension of the ADJUSTR 
  !! intrinsic.
  INTERFACE adjustr
     !> @details See aniso_varying_string::adjustr_.
     MODULE PROCEDURE adjustr_
  END INTERFACE adjustr

  !> Converts a varying_string to default CHARACTER. 
  !!
  !! Interface has the form of a pure function CHAR(@a string [, @a length]) 
  !! returning a character string.
  INTERFACE char
     !> @details Conversion to a string of given length.  See
     !! aniso_varying_string::char_fixed.
     MODULE PROCEDURE char_fixed
  END INTERFACE char

  !> Returns the position of a character in the ASCII collation sequence.
  !!
  !! The interface has the form of a function with the form IACHAR(@a c) 
  !! returning an INTEGER.  The varying_string argument @a c shall have a 
  !! length of exactly one. This is an extension of the IACHAR intrinsic.
  !!
  !! This is the varying_string inverse of the %ACHAR intrinsic.
  INTERFACE iachar
     !> @details See aniso_varying_string::iachar_.
     MODULE PROCEDURE iachar_
  END INTERFACE iachar

  !> Returns the position of a character in the processor defined collation 
  !! sequence.
  !!
  !! The interface has the form of an elemental function with the form 
  !! ICHAR(@a c) returning an INTEGER. The varying_string argument @a c shall 
  !! have a length of exactly one. This is an extension of the ICHAR intrinsic.
  !!
  !! This is the varying_string inverse of the %CHAR intrinsic.
  INTERFACE ichar
     !> @details See aniso_varying_string::ichar_.
     MODULE PROCEDURE ichar_
  END INTERFACE ichar

  !> Returns an INTEGER that is the starting position of a substring within a 
  !! string.
  !!
  !! The interface has the form of an elemental function INDEX(@a string, 
  !! @a substring [, @a back]) returning an INTEGER.  Forms are available for 
  !! both @a string and @a substring to be varying_strings, or a varying_string 
  !! and a character string, or vice versa.  @a back shall be of type LOGICAL.  
  !!
  !! If @ a back is not present or has the value false, the result is the 
  !! minimum positive value of I such that:
  !! @code
  !! EXTRACT(string, I, I+LEN(substring)-1) == substring
  !! @endcode
  !! or zero if there is no such value.
  !!
  !! If @a back is true, then the result is the maximum value of I less than
  !! or equal to LEN(@a string) - LEN(@a substring) + 1, such that:
  !! @code
  !! EXTRACT(string, I, I+LEN(substring)-1) == substring
  !! @endcode
  !! or zero if there is no such value.
  !!
  !! This is an extension of the %INDEX intrinsic.
  INTERFACE index
     !> @details Case for when @a string and @a substring are both 
     !! varying_strings.  See aniso_varying_string::index_VS_VS.
     MODULE PROCEDURE index_VS_VS
     !> @details Case for when @a string is a CHARACTER string and 
     !! @a substring is a varying_string.  See aniso_varying_string::index_CH_VS.
     MODULE PROCEDURE index_CH_VS
     !> @details Case for when @a string is a varying_string and @a substring 
     !! is a CHARACTER string.  See aniso_varying_string::index_VS_CH.
     MODULE PROCEDURE index_VS_CH
  END INTERFACE index

  !> Returns the length of a varying_string.
  !!
  !! The interface has the form of an elemental function LEN(@a string)
  !! returning an INTEGER.  This is an extension of the %LEN intrinsic.
  !
  ! In the module subprograms below, make sure that the intrinsic LEN is only 
  ! used in specifications after the len_ subprogram!
  INTERFACE len
     !> @details See aniso_varying_string::len_ 
     MODULE PROCEDURE len_
  END INTERFACE len

  !> Returns the length of a varying_string not counting any trailing blanks.
  !!
  !! The interface has the form of a elemental function LEN_TRIM(@a string) 
  !! returning an INTEGER.  This is an extension of the LEN_TRIM intrinsic.
  INTERFACE len_trim
     !> @details See aniso_varying_string::len_trim_
     MODULE PROCEDURE len_trim_
  END INTERFACE len_trim

  !> Compares the lexical ordering of two strings based on the ISO 646:1991 
  !! collating sequence (ASCII).  The comparison is @a string_a greater than
  !! or equal to @a string_b.
  !!
  !! The interface has the form of an elemental function LGE(@a string_a, 
  !! @a string_b) returning a LOGICAL.  Forms are provided for @a string_a
  !! and @a string_b to be both varying_strings, a varying_string and a 
  !! character string or vice versa.
  !!
  !! If the arguments do not match in length then the shorter argument is
  !! padded on the right with blanks.
  INTERFACE lge
     !> @details Form for comparison of two varying_strings.  See
     !! aniso_varying_string::lge_VS_VS.
     MODULE PROCEDURE lge_VS_VS
     !> @details Form for comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::lge_CH_VS.
     MODULE PROCEDURE lge_CH_VS
     !> @details Form for comparison of a varying_string with a CHARACTER 
     !! string.  See aniso_varying_string::lge_VS_CH.
     MODULE PROCEDURE lge_VS_CH
  END INTERFACE lge

  !> Compares the lexical ordering of two strings based on the ISO 646:1991 
  !! collating sequence (ASCII).  The comparison is @a string_a greater than 
  !! @a string_b. 
  !!
  !! The interface has the form of an elemental function LGT(@a string_a, 
  !! @a string_b) returning a LOGICAL.  Forms are provided for @a string_a
  !! and @a string_b to be both varying_strings, a varying_string and a 
  !! character string or vice versa.
  !!
  !! If the arguments do not match in length then the shorter argument is
  !! padded on the right with blanks.
  INTERFACE lgt
     !> @details Form for comparison of two varying_strings.  See
     !! aniso_varying_string::lgt_VS_VS.
     MODULE PROCEDURE lgt_VS_VS
     !> @details Form for comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::lgt_CH_VS.
     MODULE PROCEDURE lgt_CH_VS
     !> @details Form for comparison of a varying_string with a CHARACTER 
     !! string.  See aniso_varying_string::lgt_VS_CH.
     MODULE PROCEDURE lgt_VS_CH
  END INTERFACE lgt

  !> Compares (less than or equal to) the lexical ordering of two strings 
  !! based on the ISO 646:1991 collating sequence (ASCII).
  !!
  !! The interface has the form of an elemental function LGE(@a string_a, 
  !! @a string_b) returning a LOGICAL.  Forms are provided for @a string_a
  !! and @a string_b to be both varying_strings, a varying_string and a 
  !! character string or vice versa.
  !!
  !! @return The result of the comparison is @a string_a less than or equal 
  !! to @a string_b. If the arguments do not match in length then the shorter 
  !! argument is padded on the right with blanks.
  INTERFACE lle
     !> @details Form for comparison of two varying_strings.  See
     !! aniso_varying_string::lle_VS_VS.
     MODULE PROCEDURE lle_VS_VS
     !> @details Form for comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::lle_CH_VS.
     MODULE PROCEDURE lle_CH_VS
     !> @details Form for comparison of a varying_string with a CHARACTER 
     !! string.  See aniso_varying_string::lle_VS_CH.
     MODULE PROCEDURE lle_VS_CH
  END INTERFACE lle

  !> Compares the lexical ordering of two strings based on the ISO 646:1991 
  !! collating sequence (ASCII).  The comparison is @a string_a less than 
  !! @a string_b. 
  !!
  !! The interface has the form of an elemental function LGE(@a string_a, 
  !! @a string_b) returning a LOGICAL.  Forms are provided for @a string_a
  !! and @a string_b to be both varying_strings, a varying_string and a 
  !! character string or vice versa.
  !!
  !! If the arguments do not match in length then the shorter argument is
  !! padded on the right with blanks.
  INTERFACE llt
     !> @details Form for comparison of two varying_strings.  See
     !! aniso_varying_string::llt_VS_VS.
     MODULE PROCEDURE llt_VS_VS
     !> @details Form for comparison of a character string with a 
     !! varying_string.  See aniso_varying_string::llt_CH_VS.
     MODULE PROCEDURE llt_CH_VS
     !> @details Form for comparison of a varying_string with a CHARACTER 
     !! string.  See aniso_varying_string::llt_VS_CH.
     MODULE PROCEDURE llt_VS_CH
  END INTERFACE llt

  !> Concatenates several copies of a string.
  !!
  !! The interface is of the form of an elemental function REPEAT(@a string, 
  !! @a ncopies), returning a varying_string.  This is an extension of the
  !! REPEAT intrinsic.  @a string is a varying_string.
  !!
  !! @return The result value is the string produced by repeated concatenation 
  !! of the argument @a string, producing a string containing @a ncopies 
  !! copies of @a string.  If the value of @a ncopies is not positive then the 
  !! returned string is of zero length.
  INTERFACE repeat
     !> @details See aniso_varying_string::repeat_
     MODULE PROCEDURE repeat_
  END INTERFACE repeat

  !> Scans a string for any one of the characters in a set of characters.
  !!
  !! The interface has the form of an elemental function SCAN(@a string,
  !! @a set, [, @a back]), returning an INTEGER.  Forms are available
  !! for @a string and @a set to be both varying_strings or a varying_string
  !! and a CHARACTER string or vice versa.  @a back is a LOGICAL.  This is
  !! an extension of the SCAN intrinsic.
  !!
  !! @return If @a back is absent or .FALSE. and if @a string contains at 
  !! least one character that is in @a set, then the value of the result is 
  !! the position of the left most character in @a string that is in @a set.
  !!
  !! @return If @a back is .TRUE. and if @a string contains at least one 
  !! character that is in @a set, then the value of the result is the position
  !! of the right most character in @a string that is in @a set.
  !!
  !! @return The value of the result is zero if no character of @a string is
  !! in @a set or if the length of either @a string or @a set is zero.
  INTERFACE scan
     !> @details Form for scanning in a varying_string for characters from a 
     !! varying_string. See aniso_varying_string::scan_VS_VS.
     MODULE PROCEDURE scan_VS_VS
     !> @details Form for scanning in a CHARACTER string for characters from a 
     !! varying_string.  See aniso_varying_string::scan_CH_VS.
     MODULE PROCEDURE scan_CH_VS
     !> @details Form for scanning in a varying_string for characters from a 
     !! CHARACTER string.  See aniso_varying_string::scan_VS_CH.
     MODULE PROCEDURE scan_VS_CH
  END INTERFACE scan

  !> Removes trailing blanks from a string.
  !!
  !! The interface has the form of an elemental function TRIM(@a string), 
  !! returning a varying_string.  @a string is a varying_string.  This is 
  !! an extension of the TRIM intrinsic.
  !!
  !! @return The result value is the same as @a string except that any 
  !! trailing blanks have been deleted.  If the argument @a string contains 
  !! only blank characters or is of zero length, the result is a 
  !! zero-length string.
  INTERFACE trim
     !> @details See aniso_varying_string::trim_
     MODULE PROCEDURE trim_
  END INTERFACE trim

  !> Verifies that a string contains only characters from a given set by 
  !! scanning for any character not in the set.
  !!
  !! This generic interface is for an elemental function that is an extension
  !! of the VERIFY intrinsic.
  !!
  !! @param[in]     string      The varying_string or CHARACTER string to 
  !! verify.
  !!
  !! @param[in]     set         The varying_string or CHARACTER with the set
  !! of characters to use for verifying.  If @a string is a CHARACTER string
  !! then this argument must be a varying_string.
  !! 
  !! @param[in]     bacl        An optional LOGICAL to indicate the search
  !! direction.
  !!
  !! @return An INTEGER.
  !!
  !! @return - If @a back is not present or is .FALSE. and if @a string 
  !! contains at least one character that is not in @a set, the value of 
  !! the result is the position of the left-most character of @a string 
  !! that is not in @a set.
  !!
  !! @return - If @a back is .TRUE. and if @a string contains at least one 
  !! character that is not in @a set, the value of the result is the position 
  !! of the right-most character of @a string that is not in @a set.
  !!
  !! @return - The value of the result is zero if each character of 
  !! @a string is in @a set or if the length of @a string is zero.
  INTERFACE verify
     !> @details Form for verifying a varying_string for characters from a 
     !! varying_string. See aniso_varying_string::scan_VS_VS.
     MODULE PROCEDURE verify_VS_VS
     !> @details Form for verifying a CHARACTER string for characters from a 
     !! varying_string. See aniso_varying_string::scan_CH_VS.
     MODULE PROCEDURE verify_CH_VS
     !> @details Form for verifying a varying_string for characters from a 
     !! CHARACTER string. See aniso_varying_string::scan_VS_CH.
     MODULE PROCEDURE verify_VS_CH
  END INTERFACE verify

  !> Converts an intrinsic fixed-length character value into the equivalent
  !! varying-length string value.
  !!
  !! The interface is of the form of an elemental function VAR_STR(@a char). 
  !!
  !! @param[in]     char    The CHARACTER string, of any length, to convert.
  !!
  !! @return A varying_string with the same string of characters as @a char.
  INTERFACE var_str
     !> @details See aniso_varying_string::var_str_.
     MODULE PROCEDURE var_str_
  END INTERFACE var_str

  !> Reads characters from an external file into a string.
  !!
  !! This generic interface is for a subroutine.
  !!
  !! @param[out]    string      A scalar varying_string or deferred length 
  !! allocatable CHARACTER to receive the characters read from the file.
  !!
  !! @param[in]     maxlen      A optional scalar INTEGER with the maximum 
  !! number of characters to read from the file.  If less than or equal to zero 
  !! then no characters will be read and @a string will be set to zero length.
  !! If absent then an maximum of HUGE(1) will be used.
  !!
  !! @param[in]     unit        A scalar INTEGER with the number of the input
  !! unit.  The unit shall be connected to a formatted file for sequential
  !! read access.  If the argument is omitted then the default input unit is
  !! used.
  !!
  !! @param[in]     set         A scalar varying_string or CHARACTER that 
  !! specifies the set of characters, the occurrence of any of which will 
  !! terminate the input.  The terminal character will be read from the input
  !! file but will not be included in @a string.
  !!
  !! @param[out]    separator   An optional scalar varying_string or 
  !! deferred length allocatable CHARACTER (the type must match that of 
  !! @a string) that receives the actual character (from @a set) 
  !! that terminates the transfer.  If the transfer is terminated other 
  !! than by the occurrence of a character in @a set then a zero length 
  !! string will be returned.
  !!
  !! @param[out]    iostat      An optional scalar INTEGER to receive the 
  !! status resulting from the transfer.  A zero value is returned if a valid
  !! read operation occurs and the end-of-record is not reached, a positive
  !! value if an error occurs and a negative value if an end-of-file or
  !! end-of-record condition occurs.  If @a iostat is absent and an error or
  !! end-of-file condition occurs, the program execution is terminated.
  !!
  !! @param[in,out] iomsg       An optional scalar assumed length CHARACTER 
  !! to receive an explanatory message in the event of error, end of file 
  !! or end of record.  Only defined if @a iostat is present and not zero.
  !!
  !! The procedures cause characters from the connected file, starting with 
  !! the next character in the current record if there is a current record
  !! or the first character of the enxt record if not, to be read and stored
  !! in @a string.  The end of record always terminates the input but input 
  !! may be terminated before this.  
  !!
  !! The file position after the data transfer is complete is after the last
  !! character that was read.  If the transfer is terminated by the end of 
  !! record being reached then the file is positioned after the record just 
  !! read.
  INTERFACE get
     !> @details Form for operations on the default input unit, with no 
     !! terminal character set and the result as a varying_string.  
     !! See aniso_varying_string::get_VS.
     MODULE PROCEDURE get_VS
     !> @details Form for operations on the default input unit, with no 
     !! terminal character set and the result as deferred length 
     !! allocatable CHARACTER.  See aniso_varying_string::get_CH.
     MODULE PROCEDURE get_CH
     !> @details Form for operations on a specified unit, with no terminal 
     !! character set and the result as a varying_string.  See 
     !! aniso_varying_string::get_unit.
     MODULE PROCEDURE get_unit_VS
     !> @details Form for operations on a specified unit, with no terminal 
     !! character set and the result as a deferred length allocatable 
     !! CHARACTER.  See aniso_varying_string::get_unit_CH.
     MODULE PROCEDURE get_unit_CH

     ! The get_set* forms all have an optional separator argument 
     ! that is a varying_string that gets defined with the character 
     ! that terminated the input, if that character was from the provided 
     ! set of characters that terminate input.  Unfortunately, because 
     ! that argument is specified as being OPTIONAL, we cannot then easily 
     ! provide specific versions that return the separator as deferred 
     ! length allocatable CHARACTER.

     !> @details Form for operations on the default input unit, with the 
     !! result @a string as varying_string and a terminal character @a set 
     !! specified using a varying_string.  See 
     !! aniso_varying_string::get_set_VS_VS.
     MODULE PROCEDURE get_set_VS_VS
     !> @details Form for operations on the default input unit, with the 
     !! result @a string as a deferred length allocatable CHARACTER and 
     !! a terminal character @a set specified using a varying_string.  
     !! See aniso_varying_string::get_set_CH_VS.
     MODULE PROCEDURE get_set_CH_VS
     !> @details Form for operations on the default input unit, with the 
     !! @a string read as a varying_string and the @a set of terminal 
     !! characters as a CHARACTER string.  See 
     !! aniso_varying_string::get_set_VS_CH.
     MODULE PROCEDURE get_set_VS_CH
     !> @details Form for operations on the default input unit, with the 
     !! @a string read as deferred length CHARACTER and the @a set of 
     !! terminal characters as a CHARACTER string.  See 
     !! aniso_varying_string::get_set_CH_CH.
     MODULE PROCEDURE get_set_CH_CH
     !> @details Form for operations on a specified unit, with the @a string 
     !! read as a varying_string and the a terminal character @a set 
     !! as a varying_string. See
     !! aniso_varying_string::get_unit_set_VS_VS.
     MODULE PROCEDURE get_unit_set_VS_VS
     !> @details Form for operations on a specified unit, with the @a string 
     !! read as a varying_string and the a terminal character @a set 
     !! as a varying_string. See
     !! aniso_varying_string::get_unit_set_VS_VS.
     MODULE PROCEDURE get_unit_set_CH_VS
     !> @details Form for operations on a specified unit, with the @a string 
     !! read as a varying_string and the terminal character @a set as 
     !! a CHARACTER string.  See aniso_varying_string::get_unit_set_VS_CH.
     MODULE PROCEDURE get_unit_set_VS_CH
     !> @details Form for operation on a specified unit, with the @a string 
     !! read as a deferred length allocatable CHARACTER and the terminal 
     !! character @a set as a CHARACTER string.  See 
     !> aniso_varying_string::get_unit_set_CH_CH.
     MODULE PROCEDURE get_unit_set_CH_CH
  END INTERFACE get

  !> Writes a string to an external file.
  !!
  !! This generic interface is for a subroutine.
  !!
  !! @param[in]     string      A scalar of type varying_string or CHARACTER
  !! with the string to be transferred.
  !!
  !! @param[in]     unit        An optional scalar INTEGER that specifies the 
  !! output unit to use.  The unit shall be connected to a formatted file for
  !! sequential write access.  If not present then the default output unit is
  !! used.
  !!
  !! @param[out]    iostat      An optional scalar INTEGER to receive the 
  !! status resulting from the transfer.  A zero value is returned if a valid
  !! write operation occurs and a positive value if an error occurs.  If 
  !! absent and anything other than a valid write operation occurs then the
  !! program execution is terminated.
  !!
  !! The characters of @a string are appended to the current record, if there
  !! is a current record, or to the start of the next record if there is no
  !! current record.  The last character transferred becomes the last character
  !! of the current record, which is the last record of the file.
  INTERFACE put
     !> @details Form for operations on the default output unit, with @a string 
     !! specified using a varying_string.  See aniso_varying_string::put_VS.
     MODULE PROCEDURE put_VS
     !> @details Form for operations on the default output unit, with @a string 
     !! specified using a CHARACTER.  See aniso_varying_string::put_CH.
     MODULE PROCEDURE put_CH
     !> @details Form for operations on a specified unit, with @a string 
     !! specified using a varying_string.  See 
     !! aniso_varying_string::put_unit_VS.
     MODULE PROCEDURE put_unit_VS
     !> @details Form for operations on a specified unit, with @a string 
     !! specified using a CHARACTER string.  See 
     !! aniso_varying_string::put_unit_CH.
     MODULE PROCEDURE put_unit_CH
  END INTERFACE put

  !> Writes a string to an external file and ends the record.
  !!
  !! This generic interface is for a subroutine.
  !!  
  !! @param[in]     string      A scalar of type varying_string or CHARACTER
  !! with the string to be transferred.
  !!
  !! @param[in]     unit        An optional scalar INTEGER that specifies the 
  !! output unit to use.  The unit shall be connected to a formatted file for
  !! sequential write access.  If not present then the default output unit is
  !! used.
  !!
  !! @param[out]    iostat      An optional scalar INTEGER to receive the 
  !! status resulting from the transfer.  A zero value is returned if a valid
  !! write operation occurs and a positive value if an error occurs.  If 
  !! absent and anything other than a valid write operation occurs then the
  !! program execution is terminated.
  !!
  !! The characters of @a string are appended to the current record, if there
  !! is a current record, or to the start of the next record if there is no
  !! current record.
  INTERFACE put_line
     !> @details Form for operations on the default output unit, with @a string
     !! specified using a varying_string.  See aniso_varying_string::put_line_VS.
     MODULE PROCEDURE put_line_VS
     !> @details Form for operations on the default output unit, with @a string 
     !! specified using a CHARACTER string.  See 
     !! aniso_varying_string::put_line_CH.
     MODULE PROCEDURE put_line_CH
     !> @details Form for operations on a specified output unit, with @a string 
     !! specified using a varying_string.  See 
     !! aniso_varying_string::put_line_unit_VS.
     MODULE PROCEDURE put_line_unit_VS
     !> @details Form for operations on a specified output unit, with @a string 
     !! specified using a CHARACTER string.  
     !! See aniso_varying_string::put_line_unit_CH.
     MODULE PROCEDURE put_line_unit_CH
  END INTERFACE put_line

  !> Extracts a specified substring from a string.
  !!
  !! This generic interface is for a function, all forms of which are 
  !! elemental.
  !!
  !! @param[in]     string      The varying_string or CHARACTER string from
  !! which to extract characters.
  !!
  !! @param[in]     start       An optional INTEGER with the start index.  If
  !! not present or less than one then a start index of one is used.
  !!
  !! @param[in]     finish      An optional INTEGER with the finish index.  If
  !! not present or greater than LEN(@a string) then a finish index of 
  !! LEN(@a string) is used.
  !!
  !! @return A varying_string with a copy of the characters of @a string 
  !! between the start index and the finish index, inclusive.  If @a finish is 
  !! less than @a start, the result is a zero length string.
  INTERFACE extract
     !> @details Form for extracting characters from a varying_string.  See
     !! aniso_varying_string::extract_VS.
     MODULE PROCEDURE extract_VS
     !> @details Form for extracting characters from a CHARACTER string.  See
     !! aniso_varying_string::extract_CH.
     MODULE PROCEDURE extract_CH
  END INTERFACE extract

  !> Insert a substring into a string at a specified position.
  !!
  !! This generic interface is for a function, all forms of which are 
  !! elemental.
  !!
  !! @param[in]     string      A varying_string or CHARACTER string with the
  !! string to insert into.
  !!
  !! @param[in]     start       An INTEGER with the start index for the
  !! insertion.
  !!
  !! @param[in]     substring   A varying_string or CHARACTER string with
  !! the string to be inserted.
  !!
  !! @return A varying_string with a copy of the characters of @a string with 
  !! the characters of @a substring inserted into the copy of @a string 
  !! before the character at the character position @a start.  If @a start is 
  !! greater than LEN(@a string) then the value LEN(@a string) + 1 is used for 
  !! start and @a substring is appended to the copy of @a string.  If @a start 
  !! is less than one, the value one is used for @a start and @a substring is 
  !! inserted before the first character of the copy of @a string.
  INTERFACE insert
     !> @details Form for insertion of a varying_string into a varying_string.
     !! See aniso_varying_string::insert_VS_VS.
     MODULE PROCEDURE insert_VS_VS
     !> @details Form for insertion of a CHARACTER string into a varying_string.
     !! See aniso_varying_string::insert_CH_VS.
     MODULE PROCEDURE insert_CH_VS
     !> @details Form for insertion of a varying_string into a CHARACTER string.
     !! See aniso_varying_string::insert_VS_CH.
     MODULE PROCEDURE insert_VS_CH
     !> @details Form for insertion of a CHARACTER string into a CHARACTER 
     !! string. See aniso_varying_string::insert_CH_CH.
     MODULE PROCEDURE insert_CH_CH
  END INTERFACE insert

  !> Remove a specified substring from a string.
  !!
  !! This generic interface is for a function, all forms of which are 
  !! elemental.
  !!
  !! @param[in]     string      A varying_string or CHARACTER string with the
  !! string to %remove the substring from.
  !!
  !! @param[in]     start       An optional INTEGER with the start %index of 
  !! the characters to be removed.  If not present or less than one, then the 
  !! value one is used for the start %index.
  !!
  !! @param[in]     finish      An optional INTEGER with the finish %index of
  !! the characters to be removed.  If not present or greater than 
  !! LEN(@a string) then the value LEN(@a string) is used for the finish 
  !! %index.
  !!
  !! @return A varying_string with a copy of the characters of @a string with 
  !! the characters between the start %index and the finish %index, inclusive, 
  !! removed.  If the finish %index is less than the start %index the 
  !! characters of @a string are returned unchanged.
  INTERFACE remove
     !> @details Form for operation on a varying_string.  See 
     !! aniso_varying_string::remove_VS.
     MODULE PROCEDURE remove_VS
     !> @details Form for operation on a CHARACTER string.  See 
     !! aniso_varying_string::remove_CH.
     MODULE PROCEDURE remove_CH
  END INTERFACE remove

  !> Replace a subset of the characters in a string by a given substring.
  !!
  !! The subset may be specified either by position or by content.
  !!
  !! This generic interface is for a function, all forms of which are 
  !! elemental.
  !!
  !! @param[in]     string      A varying_string or CHARACTER string with
  !! the string to operate with.
  !!
  !! @param[in]     start       An INTEGER with the start index for the 
  !! replacement
  !!
  !! @param[in]     finish      An INTEGER with the finish index for the
  !! replacement.
  !!
  !! @param[in]     substring   A varying_string or CHARACTER string with
  !! the substring to replaced removed characters.
  !!
  !! @param[in]     target      A varying_string or CHARACTER string with
  !! the characters to be replaced.
  !!
  !! @param[in]     every       An optional LOGICAL to specify that all
  !! occurences of @a target should be replaced.
  !!
  !! @param[in]     back        An optional LOGICAL to specify that the
  !! search direction should be from the back of the string.
  !!
  !! @return A varying_string with a copy of the characters in @a string
  !! modified as per one of the following:
  !!
  !! @return - For a reference of the form 
  !! REPLACE(@a string, @a start, @a substring)
  !! the characters of the argument @a substring are inserted into the copy 
  !! of @a string beginning with the character at the character position
  !! @a start.  The characters in positions from @a start to 
  !! MIN(@a start + LEN(@a substring) - 1, LEN(@a string)) are deleted.
  !! If @a start is greater than LEN(@a string), the value LEN(@a string) + 1
  !! is used for @a start and @a substring is appended to the copy of 
  !! @a string.  If @a start is less than one, the value one is used for 
  !! @a start.  
  !!
  !! @return - For a reference of the form 
  !! REPLACE(@a string, @a start, @a finish, @a substring)
  !! the characters in the copy of @a string between positions @a start and 
  !! @a finish, including those at @a start and @a finish, are deleted and 
  !! replaced by the characters of @a substring. If @a start is less than
  !! one, the value one is used for @a start. If @a finish is greater than 
  !! LEN(@a string), the value LEN(@a string) is used for @a finish. If 
  !! @a finish is less than @a start, the characters of @a substring are
  !! inserted before the character at @a start and no characters are deleted.
  !!
  !! @return - For a reference of the form 
  !! REPLACE(@a string, @a target, @a substring, @a every, @a back)
  !! the copy of @a string is searched for occurrences of @a target. The search 
  !! is done in the backward direction if the argument @a back is present with 
  !! the value .TRUE., and in the forward direction otherwise. If @a target is 
  !! found, it is replaced by @a substring. If @a every is present with the 
  !! value .TRUE., the search and replace is continued from the character 
  !! following @a target in the search direction specified until all 
  !! occurrences of @a target in the copy string are replaced; otherwise only 
  !! the first occurrence of @a target is replaced. 
  INTERFACE replace
     !> @details Form for replacing a varying_string with a varying_string, 
     !! with the replacement position specified by starting position only.  See
     !! aniso_varying_string::replace_VS_VS_auto.
     MODULE PROCEDURE replace_VS_VS_auto
     !> @details Form for replacing a CHARACTER string with a varying_string,
     !! with the replacement position specified by starting position only.  See 
     !! aniso_varying_string::replace_CH_VS_auto.
     MODULE PROCEDURE replace_CH_VS_auto
     !> @details Form for replacing a varying_string with a CHARACTER string,
     !! with the replacement position specified by starting position only.  See 
     !! aniso_varying_string::replace_VS_CH_auto.
     MODULE PROCEDURE replace_VS_CH_auto
     !> @details Form for replacing a CHARACTER string with a CHARACTER string,
     !! with the replacement position specified by starting position only.  See 
     !! aniso_varying_string::replace_CH_CH_auto.
     MODULE PROCEDURE replace_CH_CH_auto
     !> @details Form for replacing a varying_string with a varying_string,
     !! with the replacement position specified by starting and finishing 
     !! position.  See aniso_varying_string::replace_VS_VS_fixed.
     MODULE PROCEDURE replace_VS_VS_fixed
     !> @details Form for replacing a CHARACTER string with a varying_string,
     !! with the replacement position.specified by starting and finishing 
     !! position.  See aniso_varying_string::replace_CH_VS_fixed.
     MODULE PROCEDURE replace_CH_VS_fixed
     !> @details Form for replacing a varying_string with a CHARACTER string,
     !! with the replacement position.specified by starting and finishing 
     !! position.  See aniso_varying_string::replace_VS_CH_fixed.
     MODULE PROCEDURE replace_VS_CH_fixed
     !> @details Form for replacing a CHARACTER string with a CHARACTER string,
     !! with the replacement position.specified by starting and finishing 
     !! position.  See aniso_varying_string::replace_CH_CH_fixed.
     MODULE PROCEDURE replace_CH_CH_fixed
     !> @details Form for replacing a varying_string with a varying_string,
     !! with the replacement position specified by a varying_string.  
     !! See aniso_varying_string::replace_VS_VS_VS_target.
     MODULE PROCEDURE replace_VS_VS_VS_target
     !> @details Form for replacing a CHARACTER string with a varying_string,
     !! with the replacement position specified by a varying_string.  See 
     !! aniso_varying_string::replace_CH_VS_VS_target.
     MODULE PROCEDURE replace_CH_VS_VS_target
     !> @details Form for replacing a varying_string with a CHARACTER string,
     !! with the replacement position specified by a varying_string.  See 
     !! aniso_varying_string::replace_VS_CH_VS_target.
     MODULE PROCEDURE replace_VS_CH_VS_target
     !> @details Form for replacing a CHARACTER string with a CHARACTER string,
     !! with the replacement position specified by a varying_string.  See 
     !! aniso_varying_string::replace_CH_CH_VS_target.
     MODULE PROCEDURE replace_CH_CH_VS_target
     !> @details Form for replacing a varying_string with a varying_string,
     !! with the replacement position specified by a CHARACTER.  See 
     !! aniso_varying_string::replace_VS_VS_CH_target.
     MODULE PROCEDURE replace_VS_VS_CH_target
     !> @details Form for replacing a CHARACTER string with a varying_string,
     !! with the replacement position specified by a varying_string.  See 
     !! aniso_varying_string::replace_CH_VS_CH_target.
     MODULE PROCEDURE replace_CH_VS_CH_target
     !> @details Form for replacing a varying_string with a CHARACTER string,
     !! with the replacement position specified by a CHARACTER string.  See 
     !! aniso_varying_string::replace_VS_CH_CH_target.
     MODULE PROCEDURE replace_VS_CH_CH_target
     !> @details Form for replacing a CHARACTER string with a CHARACTER string,
     !! with the replacement position specified by a CHARACTER string.  See 
     !! aniso_varying_string::replace_CH_CH_CH_target.
     MODULE PROCEDURE replace_CH_CH_CH_target
  END INTERFACE replace

  !> Splits a string into two substrings with the substrings separated by the 
  !! occurrence of a character from a specified separator set.
  !!
  !! This generic interface is for a subroutine.  Forms that have 
  !! INTENT(INOUT) or INTENT(OUT) deferred CHARACTER are PURE, forms without 
  !! deferred CHARACTER are ELEMENTAL.
  !!
  !! @param[in,out] string      A varying_string containing the string to 
  !! be %split and the remainder of the string after splitting, not including
  !! the separator character.
  !!
  !! @param[out]    word        A varying_string to receive characters passed
  !! over in the search for a separator character from @a set.
  !!
  !! @param[in]     set         A varying_string or CHARACTER string with
  !! the set of characters to use as a separator character for the splitting.
  !!
  !! @param[out]    separator   An optional varying_string or deferred length 
  !! allocatable CHARACTER (the type must match that of @a string) to receive 
  !! the character that actually separates @a word from the remainder of 
  !! @a string.
  !!
  !! @param[in]     back        An optional LOGICAL to indicate a backward
  !! search if present with the value .TRUE.  A forward search is conducted
  !! if not present.
  !!
  !! The effect of the procedure is to divide @a string at the first occurrence
  !! of a character that is in @a set.  If no character from @a set if found
  !! or if @a string is of zero length, the whole string is returned in 
  !! @a word, @a string is returned as zero length and @a separator (if present)
  !! is returned as zero length.  The effect of the procedure is such that,
  !! on return, either @a word // @a separator // @a string is the same as the 
  !! initial string for a forward search, of @a string // @a separator 
  !! // @a word is the same as the initial string for a backward search.
  INTERFACE split
     !> @details Form for the set of separator characters to be specified as a
     !! varying_string.  See aniso_varying_string::split_VS. 
     MODULE PROCEDURE split_VS_VS_VS
     !> @details Form for the set of separator characters to be specified as a
     !! CHARACTER string.  See aniso_varying_string::split_CH. 
     MODULE PROCEDURE split_VS_VS_CH
     !> @details Form for the @a string to split, the @a word split off, 
     !! the @a set of separator characters and the optional @a separator to 
     !! all be CHARACTER.
     MODULE PROCEDURE split_CH_CH_CH
  END INTERFACE split

  !> Get the number of file storage units required for unformatted output 
  !! of a varying_string value.
  INTERFACE IOLength
     !> @details See aniso_varying_string::IOLength_.
     MODULE PROCEDURE IOLength_
  END INTERFACE IOLength

  !-----------------------------------------------------------------------------
  ! Access specifiers

  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(//)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: OPERATOR(<)
  PUBLIC :: OPERATOR(<=)
  PUBLIC :: OPERATOR(>=)
  PUBLIC :: OPERATOR(>)
  ! See comments for the interface blocks above.
  !  PUBLIC :: READ(FORMATTED)
  !  PUBLIC :: WRITE(FORMATTED)
  !  PUBLIC :: READ(UNFORMATTED)
  !  PUBLIC :: WRITE(UNFORMATTED)
  PUBLIC :: adjustl
  PUBLIC :: adjustr
  PUBLIC :: char
  PUBLIC :: iachar
  PUBLIC :: ichar
  PUBLIC :: index
  PUBLIC :: len
  PUBLIC :: len_trim
  PUBLIC :: lge
  PUBLIC :: lgt
  PUBLIC :: lle
  PUBLIC :: llt
  PUBLIC :: repeat
  PUBLIC :: scan
  PUBLIC :: trim
  PUBLIC :: verify
  PUBLIC :: var_str
  PUBLIC :: get
  PUBLIC :: put
  PUBLIC :: put_line
  PUBLIC :: extract
  PUBLIC :: insert
  PUBLIC :: remove
  PUBLIC :: replace
  PUBLIC :: split
  PUBLIC :: iolength

  !-----------------------------------------------------------------------------
  ! Internal types

  !> Stores the results of parsing the character literal after a DT edit 
  !! descriptor.
  TYPE :: literal_parse_results
     !> Status of the SKIPBLANK/NOSKIPBLANK modifiers.
     !! - 0: Neither modifier seen.
     !! - 1: SKIPBLANK seen.
     !! - 2: NOSKIPBLANK seen.
     INTEGER :: skipblank = 0

     LOGICAL :: seen_eor = .FALSE.         !< Seen the EOR keyword.
     LOGICAL :: seen_blank = .FALSE.       !< Seen the BLANK keyword.
     LOGICAL :: seen_slash = .FALSE.       !< Seen the SLASH keyword.
     LOGICAL :: seen_nodelimited = .FALSE. !< Seen the NODELIMITED keyword.
     LOGICAL :: seen_comma = .FALSE.       !< Seen the COMMA keyword.
     LOGICAL :: seen_semicolon = .FALSE.   !< Seen the SEMICOLON keyword.
     LOGICAL :: seen_nondecimal = .FALSE.  !< Seen the NONDECIMAL keyword

     !> Not allocated if we have not yet seen the DELIM keyword.  If we have 
     !! seen the delim keyword, this is allocated to the list of delimiter 
     !! characters.
     CHARACTER(:), ALLOCATABLE :: delim

     !> Not allocated if we have not yet seen the FIXED keyword.  If we 
     !! have seen the fixed keyword, this is allocated and has the value 
     !! of the integer literal constant.
     INTEGER, ALLOCATABLE :: fixed
  END TYPE literal_parse_results

CONTAINS

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for LEN and LEN_TRIM.
  !
  ! We have moved these to be the first specific procedures defined in the 
  ! program unit to avoid issues with references to the generic LEN 
  ! intrinsic occuring before the specific non-intrinsic procedure for LEN 
  ! has been defined (F2008 7.1.11p9 as modified by TC1).

  !*****************************************************************************
  !!
  !> Procedure for implementation of len - return the length of a varying 
  !! string.

  ELEMENTAL FUNCTION len_(string) RESULT(length)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string.
    TYPE(varying_string), INTENT(IN) :: string

    !> @returns The length (number of characters) of the varying string.
    INTEGER :: length

    !***************************************************************************

    IF (ALLOCATED(string%chars)) THEN
       length = LEN(string%chars)
    ELSE
       length = 0
    END IF

  END FUNCTION len_


  !*****************************************************************************
  !!
  !> Procedure for implementation of len_trim - return the length of a 
  !! varying_string not counting any trailing blanks.

  ELEMENTAL FUNCTION len_trim_(string) RESULT(length)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string.
    TYPE(varying_string), INTENT(IN) :: string

    !> @returns The length (number of characters) of the varying string, 
    !! ignoring trailing blanks (the trimmed length).
    INTEGER :: length

    !***************************************************************************

    IF (ALLOCATED(string%chars)) THEN
       length = LEN_TRIM(string%chars)
    ELSE
       length = 0
    END IF

  END FUNCTION len_trim_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for ASSIGNMENT(=)


  !*****************************************************************************
  !!
  !> Specific procedure for defined assignment of a varying_string 
  !! to a CHARACTER string.

  ELEMENTAL SUBROUTINE op_assign_CH_VS(var, exp)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The CHARACTER string being assigned to.
    CHARACTER(LEN=*,KIND=ck), INTENT(OUT) :: var

    !> The varying_string expression to be copied.
    TYPE(varying_string), INTENT(IN) :: exp

    !***************************************************************************

    var = exp%chars

  END SUBROUTINE op_assign_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of ASSIGNMENT(=), to assign a CHARACTER string
  !! to a varying_string.

  ELEMENTAL SUBROUTINE op_assign_VS_CH(var, exp)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being assigned to.
    TYPE(varying_string), INTENT(OUT) :: var

    !> The CHARACTER string to be copied.
    CHARACTER(LEN=*,KIND=ck), INTENT(IN) :: exp

    !***************************************************************************

    var%chars = exp

  END SUBROUTINE op_assign_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(//)


  !*****************************************************************************
  !!
  !> Specific procedure for OPERATOR(//), to concatenate two 
  !! varying_strings.

  ELEMENTAL FUNCTION op_concat_VS_VS(string_a, string_b)  &
       RESULT(concat_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns The concatenation of @a string_a and @a string_b.
    TYPE(varying_string) :: concat_string

    !***************************************************************************

    concat_string%chars = string_a%chars // string_b%chars

  END FUNCTION op_concat_VS_VS


  !*****************************************************************************
  !!
  !> Specific procedure for OPERATOR(//), to concatenate a CHARACTER
  !! string with a varying string.

  ELEMENTAL FUNCTION op_concat_CH_VS(string_a, string_b)  &
       RESULT(concat_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*,KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns The concatenation of @a string_a and @a string_b.
    TYPE(varying_string) :: concat_string

    !***************************************************************************

    concat_string%chars = string_a // string_b%chars

  END FUNCTION op_concat_CH_VS


  !*****************************************************************************
  !!
  !> Specific procedure for OPERATOR(//), to concatenate a varying 
  !! string with a CHARACTER string.

  ELEMENTAL FUNCTION op_concat_VS_CH(string_a, string_b)  &
       RESULT(concat_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*,KIND=ck), INTENT(IN) :: string_b

    !> @returns The concatenation of @a string_a and @a string_b.
    TYPE(varying_string) :: concat_string

    !***************************************************************************

    concat_string%chars = string_a%chars // string_b

  END FUNCTION op_concat_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(==)


  !*****************************************************************************
  !!
  !> Specific procedure for OPERATOR(==), to compare two varying strings.

  ELEMENTAL FUNCTION op_eq_VS_VS(string_a, string_b) RESULT(op_eq)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if the strings compare equal, .FALSE. otherwise.  
    LOGICAL :: op_eq

    !***************************************************************************

    op_eq = string_a%chars == string_b%chars

  END FUNCTION op_eq_VS_VS


  !*****************************************************************************
  !!
  !> Specific procedure for OPERATOR(==), to compare a CHARACTER 
  !! string with a varying string.

  ELEMENTAL FUNCTION op_eq_CH_VS(string_a, string_b) RESULT(op_eq)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*,KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if the strings compare equal, .FALSE. otherwise.  
    LOGICAL :: op_eq

    !***************************************************************************

    op_eq = string_a == string_b%chars

  END FUNCTION op_eq_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(==), to compare a varying string
  !! with a CHARACTER string.

  ELEMENTAL FUNCTION op_eq_VS_CH(string_a, string_b) RESULT(op_eq)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*,KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if the strings compare equal, .FALSE. otherwise.  
    LOGICAL :: op_eq

    !***************************************************************************

    op_eq = string_a%chars == string_b

  END FUNCTION op_eq_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(/=)


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(/=), inequality comparison of 
  !! two varying strings.

  ELEMENTAL FUNCTION op_ne_VS_VS(string_a, string_b) RESULT(op_ne)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if the strings do not compare, .FALSE. otherwise.
    LOGICAL :: op_ne

    !***************************************************************************

    op_ne = string_a%chars /= string_b%chars

  END FUNCTION op_ne_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(/=), inequality comparison of 
  !! a CHARACTER string and a varying string.

  ELEMENTAL FUNCTION op_ne_CH_VS(string_a, string_b) RESULT(op_ne)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if the strings do not compare, .FALSE. otherwise.
    LOGICAL :: op_ne

    !***************************************************************************

    op_ne = string_a /= string_b%chars

  END FUNCTION op_ne_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(/=), inequality comparison of 
  !! a varying string and a CHARACTER string.

  ELEMENTAL FUNCTION op_ne_VS_CH(string_a, string_b) RESULT(op_ne)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_strign.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if the strings do not compare, .FALSE. otherwise.
    LOGICAL :: op_ne

    !***************************************************************************

    op_ne = string_a%chars /= string_b

  END FUNCTION op_ne_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(<)


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(<), less than comparison of two 
  !! varying strings.

  ELEMENTAL FUNCTION op_lt_VS_VS(string_a, string_b) RESULT(op_lt)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_lt

    !***************************************************************************

    op_lt = string_a%chars < string_b%chars

  END FUNCTION op_lt_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(<), less than comparison of a 
  !! CHARACTER string with a varying string.

  ELEMENTAL FUNCTION op_lt_CH_VS(string_a, string_b) RESULT(op_lt)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_lt

    !***************************************************************************

    op_lt = string_a < string_b%chars

  END FUNCTION op_lt_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(<), less than comparison of a 
  !! varying string with a CHARACTER string.

  ELEMENTAL FUNCTION op_lt_VS_CH(string_a, string_b) RESULT(op_lt)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_lt

    !***************************************************************************

    op_lt = string_a%chars < string_b

  END FUNCTION op_lt_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(<=)


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(<=), less than or equal to 
  !! comparison of two varying strings.

  ELEMENTAL FUNCTION op_le_VS_VS(string_a, string_b) RESULT(op_le)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than or equal to @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_le

    !***************************************************************************

    op_le = string_a%chars <= string_b%chars

  END FUNCTION op_le_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(<=), less than or equal to 
  !! comparison of a CHARACTER string with a varying strings.

  ELEMENTAL FUNCTION op_le_CH_VS(string_a, string_b) RESULT(op_le)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than or equal to @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_le

    !***************************************************************************

    op_le = string_a <= string_b%chars

  END FUNCTION op_le_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(<=), less than or equal to 
  !! comparison of a varying string with a CHARACTER string.

  ELEMENTAL FUNCTION op_le_VS_CH(string_a, string_b) RESULT(op_le)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than or equal to @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_le

    !***************************************************************************

    op_le = string_a%chars <= string_b

  END FUNCTION op_le_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(>=)


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(>=), greater than or equal to 
  !! comparison of two varying strings.

  ELEMENTAL FUNCTION op_ge_VS_VS(string_a, string_b) RESULT(op_ge)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than or equal to @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_ge

    !***************************************************************************

    op_ge = string_a%chars >= string_b%chars

  END FUNCTION op_ge_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(>=), greater than or equal to 
  !! comparison of a CHARACTER string with a varying string.

  ELEMENTAL FUNCTION op_ge_CH_VS(string_a, string_b) RESULT(op_ge)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than or equal to @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_ge

    !***************************************************************************

    op_ge = string_a >= string_b%chars

  END FUNCTION op_ge_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(>=), greater than or equal to 
  !! comparison of a varying string with a CHARACTER string.

  ELEMENTAL FUNCTION op_ge_VS_CH(string_a, string_b) RESULT(op_ge)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than or equal to @a string_b, 
    !! .FALSE. otherwise.
    LOGICAL :: op_ge

    !***************************************************************************

    op_ge = string_a%chars >= string_b

  END FUNCTION op_ge_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for OPERATOR(>)


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(>), greater than comparison of 
  !! two varying strings.

  ELEMENTAL FUNCTION op_gt_VS_VS(string_a, string_b) RESULT(op_gt)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than @a string_b, .FALSE. 
    !! otherwise.
    LOGICAL :: op_gt

    !***************************************************************************

    op_gt = string_a%chars > string_b%chars

  END FUNCTION op_gt_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(>), greater than comparison of a
  !! CHARACTER string with a varying string.

  ELEMENTAL FUNCTION op_gt_CH_VS(string_a, string_b) RESULT(op_gt)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than @a string_b, .FALSE. 
    !! otherwise.
    LOGICAL :: op_gt

    !***************************************************************************

    op_gt = string_a > string_b%chars

  END FUNCTION op_gt_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of OPERATOR(>), greater than comparison of a 
  !! varying string with a CHARACTER string.

  ELEMENTAL FUNCTION op_gt_VS_CH(string_a, string_b) RESULT(op_gt)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than @a string_b, .FALSE. 
    !! otherwise.
    LOGICAL :: op_gt

    !***************************************************************************

    op_gt = string_a%chars > string_b

  END FUNCTION op_gt_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for ADJUSTL


  !*****************************************************************************
  !!
  !> Procedure for implementation of adjustl - adjusts a varying_string to the 
  !! left.

  ELEMENTAL FUNCTION adjustl_(string) RESULT(adjustl_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to adjust left.
    TYPE(varying_string), INTENT(IN) :: string

    !> @returns A varying string adjusted to the left - removing any 
    !! leading blanks and inserting the same number of trailing blanks
    TYPE(varying_string) :: adjustl_string

    !***************************************************************************

    adjustl_string%chars = ADJUSTL(string%chars)

  END FUNCTION adjustl_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for ADJUSTR


  !*****************************************************************************
  !!
  !> Procedure for implementation of adjustr - adjusts a varying_string to the 
  !! right.

  ELEMENTAL FUNCTION adjustr_(string) RESULT(adjustr_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to adjust right.
    TYPE(varying_string), INTENT(IN) :: string

    !>  @returns A varying string adjusted to the right - removing any 
    !! trailing blanks and inserting the same number of leading blanks.
    TYPE(varying_string) :: adjustr_string

    !***************************************************************************

    adjustr_string%chars = ADJUSTR(string%chars)

  END FUNCTION adjustr_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for CHAR.


  !*****************************************************************************
  !!
  !> Procedure for implementation of char - convert a varying_string into a
  !! CHARACTER string.

  PURE FUNCTION char_fixed(string, length) RESULT(char_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to convert.
    TYPE(varying_string), INTENT(IN) :: string

    !> The length of the result string.
    INTEGER, INTENT(IN), OPTIONAL :: length

    !> @returns A default CHARACTER string of length @a length if @a 
    !! length is present, or the length of @a string if not, with the same 
    !! sequence of characters as @a string up to the length of the result, 
    !! padded with blanks on the right if necessary.
    !!
    !! Because @a length is OPTIONAL per the description of CHAR in 
    !! ISO1539-2 3.4.3, it cannot be used in a specification 
    !! expression (F2008 7.1.11p2(2)).  Consequently the result of this 
    !! function needs to be deferred.
    !!
    !! It is possible that the specification of CHAR in ISO1539-2 is 
    !! a mistake - the intent may have been for it to be two separate 
    !! specific procedures, one with one argument, one with two, and 
    !! no OPTIONAL arguments.

    CHARACTER(:, KIND=ck), ALLOCATABLE :: char_string

    !***************************************************************************

    ALLOCATE( CHARACTER(get_char_result_length(len_(string), length)) ::  &
         char_string )

    char_string(:) = string%chars

  END FUNCTION char_fixed


  !*****************************************************************************
  !!
  !> Get the length to use for the result of the char_fixed function.
  !!
  !! This handles the optional nature of the argument for the desired 
  !! string length.

  PURE FUNCTION get_char_result_length(string_length, desired_length) RESULT(l)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The length of the @a string argument to char_fixed.s
    INTEGER, INTENT(IN) :: string_length

    !> The optional desired length of the result from char_fixed.
    INTEGER, INTENT(IN), OPTIONAL :: desired_length

    !> @returns @a desired_length, if present, otherwise @a string_length.
    INTEGER :: l

    !***************************************************************************

    IF (PRESENT(desired_length)) THEN
       l = desired_length
    ELSE
       l = string_length
    END IF

  END FUNCTION get_char_result_length


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for IACHAR and ICHAR.


  !*****************************************************************************
  !!
  !> Procedure for implementation of iachar - returns the position in the ASCII
  !! collating sequence of the first character in a varying_string.

  ELEMENTAL FUNCTION iachar_(c) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> A varying_string.
    TYPE(varying_string), INTENT(IN) :: c

    !> @returns The position of the first character in @a c in the ISO 646 
    !! (ASCII) collating sequence.
    INTEGER :: i

    !***************************************************************************

    i = IACHAR(c%chars)

  END FUNCTION iachar_


  !*****************************************************************************
  !!
  !> Procedure for implementation of ichar - returns the position in the 
  !! processor defined collation sequence of the first character of a 
  !! varying_string.

  ELEMENTAL FUNCTION ichar_(c) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> A varying string.
    TYPE(varying_string), INTENT(IN) :: c

    !> @returns The position of the first character of @a c in the processor 
    !! defined collation sequence.  
    INTEGER :: i

    !***************************************************************************

    i = ICHAR(c%chars)

  END FUNCTION ichar_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `Index`


  !*****************************************************************************
  !!
  !> Procedure for implementation of index - returns the index of the starting
  !! position of a varying_string within another varying_string.

  ELEMENTAL FUNCTION index_VS_VS(string, substring, back)  &
       RESULT(i_substring)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to search.
    TYPE(varying_string), INTENT(IN) :: string

    !> The varying_string to search for.
    TYPE(varying_string), INTENT(IN) :: substring

    !> Optional search direction - if .TRUE. then searching works back 
    !! from the end of @a string.  If not present or .FALSE. then 
    !! searching works forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The position of @a substring within @a string.
    INTEGER :: i_substring

    !***************************************************************************

    i_substring = INDEX(string%chars, substring%chars, back)

  END FUNCTION index_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of index - returns the index of the starting
  !! position of a varying_string within a CHARACTER string.

  ELEMENTAL FUNCTION index_CH_VS(string, substring, back)  &
       RESULT(i_substring)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The CHARACTER string to search.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The varying_string to search for.
    TYPE(varying_string), INTENT(IN) :: substring

    !> Optional search direction - if .TRUE. then searching works back 
    !! from the end of @a string.  If not present or .FALSE. then 
    !! searching works forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The position of @a substring within @a string.
    INTEGER :: i_substring

    !***************************************************************************

    i_substring = INDEX(string, substring%chars, back)

  END FUNCTION index_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of index - returns the index of the starting
  !! position of a CHARACTER string within a varying_string.

  ELEMENTAL FUNCTION index_VS_CH(string, substring, back)  &
       RESULT(i_substring)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to search.
    TYPE(varying_string), INTENT(IN) :: string

    !> The CHARACTER string to search for.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> Optional search direction - if .TRUE. then searching works back 
    !! from the end of @a string.  If not present or .FALSE. then 
    !! searching works forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The position of @a substring within @a string.
    INTEGER :: i_substring

    !***************************************************************************

    i_substring = INDEX(string%chars, substring, back)

  END FUNCTION index_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `lge`


  !*****************************************************************************
  !!
  !> Procedure for implementation of lge - greater than or equal to 
  !! comparison of two varying_strings using the ASCII collation sequence.

  ELEMENTAL FUNCTION lge_VS_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than or equal to @a string_b 
    !! according to the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LGE(string_a%chars, string_b%chars)

  END FUNCTION lge_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of lge - greater than or equal to 
  !! comparison of a CHARACTER string and a varying_string using the ASCII 
  !! collation sequence.

  ELEMENTAL FUNCTION lge_CH_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than or equal to @a string_b 
    !! according to the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LGE(string_a, string_b%chars)

  END FUNCTION lge_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of lge - greater than or equal to 
  !! comparison of a varying_string and a CHARACTER string using the ASCII 
  !! collation sequence.

  ELEMENTAL FUNCTION lge_VS_CH(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is greater than or equal to @a string_b 
    !! according to the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LGE(string_a%chars, string_b)

  END FUNCTION lge_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `lgt`


  !*****************************************************************************
  !!
  !> Procedure for implementation of lgt - greater than comparison of two 
  !! varying_strings using the ASCII collation sequence.

  ELEMENTAL FUNCTION lgt_VS_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @return .TRUE. if @a string_a is greater than @a string_b according to 
    !! the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LGT(string_a%chars, string_b%chars)

  END FUNCTION lgt_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of lgt - greater than comparison of a 
  !! CHARACTER string and a varying_string using the ASCII collation sequence.

  ELEMENTAL FUNCTION lgt_CH_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @return .TRUE. if @a string_a is greater than @a string_b according to 
    !! the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LGT(string_a, string_b%chars)

  END FUNCTION lgt_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of lgt - greater than comparison of a 
  !! varying_string and a CHARACTER string using the ASCII collation sequence.

  ELEMENTAL FUNCTION lgt_VS_CH(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @return .TRUE. if @a string_a is greater than @a string_b according to 
    !! the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LGT(string_a%chars, string_b)

  END FUNCTION lgt_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `lle`


  !*****************************************************************************
  !!
  !> Procedure for implementation of lle - less than or equal to comparison
  !! of two varying_strings using the ASCII collation sequence.

  ELEMENTAL FUNCTION lle_VS_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than or equal to @a string_b 
    !! according to the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LLE(string_a%chars, string_b%chars)

  END FUNCTION lle_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of lle - less than or equal to comparison
  !! of a CHARACTER string and a varying_string using the ASCII collation 
  !! sequence.

  ELEMENTAL FUNCTION lle_CH_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than or equal to @a string_b 
    !! according to the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LLE(string_a, string_b%chars)

  END FUNCTION lle_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of lle - less than or equal to comparison
  !! of a varying_string and a CHARACTER string using the ASCII collation 
  !! sequence.

  ELEMENTAL FUNCTION lle_VS_CH(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than or equal to @a string_b 
    !! according to the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LLE(string_a%chars, string_b)

  END FUNCTION lle_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `llt`


  !*****************************************************************************
  !!
  !> Procedure for implementation of llt - less than comparison of two 
  !! varying_strings using the ASCII collation sequence.

  ! Uses the intrinsic LLT after conversion of arguments to CHARACTER 
  ! strings.
  ELEMENTAL FUNCTION llt_VS_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than @a string_b according to 
    !! the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LLT(string_a%chars, string_b%chars)

  END FUNCTION llt_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of llt - less than comparison of a 
  !! CHARACTER string and a varying_string using the ASCII collation sequence.

  ! Uses the intrinsic LLT after conversion of arguments to CHARACTER 
  ! strings.
  ELEMENTAL FUNCTION llt_CH_VS(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_a

    !> Right hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than @a string_b according to 
    !! the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LLT(string_a, string_b%chars)

  END FUNCTION llt_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of llt - less than comparison of a 
  !! varying_string and a CHARACTER string using the ASCII collation sequence.

  ! Uses the intrinsic LLT after conversion of arguments to CHARACTER 
  ! strings.
  ELEMENTAL FUNCTION llt_VS_CH(string_a, string_b) RESULT(comp)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> Left hand varying_string.
    TYPE(varying_string), INTENT(IN) :: string_a

    !> Right hand CHARACTER string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string_b

    !> @returns .TRUE. if @a string_a is less than @a string_b according to 
    !! the ISO 646 collation sequence (ASCII), .FALSE. otherwise.
    LOGICAL :: comp

    !***************************************************************************

    comp = LLT(string_a%chars, string_b)

  END FUNCTION llt_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `repeat`


  !*****************************************************************************
  !!
  !! Procedure for implementation of repeat - concatenate several copies of a
  !! varying string.

  ! Converts @a string to a CHARACTER string, then returns the result of a
  ! call to the intrinsic function %REPEAT after conversion to a 
  ! varying_string.
  ELEMENTAL FUNCTION repeat_(string, ncopies) RESULT(repeat_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to copy.
    TYPE(varying_string), INTENT(IN) :: string

    !> Number of copies to make.  Must not be negative.
    INTEGER, INTENT(IN) :: ncopies

    !> @returns The concatenation of @a ncopies of @a string.
    TYPE(varying_string) :: repeat_string

    !***************************************************************************

    repeat_string = var_str(REPEAT(string%chars, ncopies))

  END FUNCTION repeat_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `scan`


  !*****************************************************************************
  !!
  !> Procedure for implementation of scan - search a varying_string for 
  !! occurrences of characters in a varying_string set.

  ELEMENTAL FUNCTION scan_VS_VS(string, set, back) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to scan.
    TYPE(varying_string), INTENT(IN) :: string

    !> The varying_string with the set of characters to scan for.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional search direction - if .TRUE. thensearching works back from 
    !! the end of @a string.  If not present or .FALSE. then searching works 
    !! forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns If @a back is not present or .FALSE., the position of the 
    !! leftmostcharacter of @a string that is in @a set.  If @a back is 
    !! .TRUE., the position of the rightmost character of @a string that is in 
    !! @a set.  If no character of @a string is in @a set or the length of 
    !! @a string or @a set is zero, then zero.
    INTEGER :: i

    !***************************************************************************

    i = SCAN(string%chars, set%chars, back)

  END FUNCTION scan_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of scan - search a CHARACTER string for 
  !! occurrences of characters in a varying_string set.

  ELEMENTAL FUNCTION scan_CH_VS(string, set, back) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The CHARACTER string to scan.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The varying_string with the set of characters to scan for.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional search direction - if .TRUE. thensearching works back from 
    !! the end of @a string.  If not present or .FALSE. then searching works 
    !! forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns If @a back is not present or .FALSE., the position of the 
    !! leftmostcharacter of @a string that is in @a set.  If @a back is 
    !! .TRUE., the position of the rightmost character of @a string that is in 
    !! @a set.  If no character of @a string is in @a set or the length of 
    !! @a string or @a set is zero, then zero.
    INTEGER :: i

    !***************************************************************************

    i = SCAN(string, set%chars, back)

  END FUNCTION scan_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of scan - search a varying_string for 
  !! occurrences of characters in a CHARACTER string set.

  ELEMENTAL FUNCTION scan_VS_CH(string, set, back) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to scan.
    TYPE(varying_string), INTENT(IN) :: string

    !> The CHARACTER string with the set of characters to scan for.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> Optional search direction - if .TRUE. thensearching works back from 
    !! the end of @a string.  If not present or .FALSE. then searching works 
    !! forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns If @a back is not present or .FALSE., the position of the 
    !! leftmostcharacter of @a string that is in @a set.  If @a back is 
    !! .TRUE., the position of the rightmost character of @a string that is in 
    !! @a set.  If no character of @a string is in @a set or the length of 
    !! @a string or @a set is zero, then zero.
    INTEGER :: i

    !***************************************************************************

    i = SCAN(string%chars, set, back)

  END FUNCTION scan_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `trim`


  !*****************************************************************************
  !!
  !> Procedure for implmentation of trim - remove trailing blanks from a 
  !! varying_string.

  ! Converts @a string to a CHARACTER string and then returns the result of 
  ! a call to the intrinsic function %TRIM.
  ELEMENTAL FUNCTION trim_(string) RESULT(trim_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to trim.
    TYPE(varying_string), INTENT(IN) :: string

    !> The value of @a string, with any trailing blanks removed.
    TYPE(varying_string) :: trim_string

    !***************************************************************************

    trim_string%chars = TRIM(string%chars)

  END FUNCTION trim_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `verify`


  !*****************************************************************************
  !!
  !> Procedure for implementation of verify - search for the first character
  !! in a varying_string that is not in a varying_string set.

  ! Converts @a string and @a set to CHARACTER strings and then returns 
  ! the result of the intrinsic function %VERIFY.
  ELEMENTAL FUNCTION verify_VS_VS(string, set, back) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to search.
    TYPE(varying_string), INTENT(IN) :: string

    !> The varying_string with the set of characters to ignore.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional search direction - if .TRUE. then searching  works back 
    !! from the end of @a string.  If not present or .FALSE. then 
    !! searching works forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns If @a back is not present or .FALSE., the position of the 
    !! leftmostcharacter of @a string that is not in @a set.  If @a back is 
    !! .TRUE., the position of the rightmost character of @a string that is 
    !! not in @a set.  If all characters of @a string are in @a set or the 
    !! length of @a string is zero, then zero.
    INTEGER :: i

    !***************************************************************************

    i = VERIFY(string%chars, set%chars, back)

  END FUNCTION verify_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of verify - search for the first character
  !! in a CHARACTER string that is not in a varying_string set.

  ! Converts @a set to a CHARACTER string and then returns the result of 
  ! the intrinsic function %VERIFY.
  ELEMENTAL FUNCTION verify_CH_VS(string, set, back) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The CHARACTER string to search.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The varying_string with the set of characters to ignore.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional search direction - if .TRUE. then searching  works back 
    !! from the end of @a string.  If not present or .FALSE. then 
    !! searching works forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns If @a back is not present or .FALSE., the position of the 
    !! leftmostcharacter of @a string that is not in @a set.  If @a back is 
    !! .TRUE., the position of the rightmost character of @a string that is 
    !! not in @a set.  If all characters of @a string are in @a set or the 
    !! length of @a string is zero, then zero.
    INTEGER :: i

    !***************************************************************************

    i = VERIFY(string, set%chars, back)

  END FUNCTION verify_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of verify - search for the first character 
  !! in a varying_string that is not in a CHARACTER string set.

  ! Converts @a string to a CHARACTER string and then returns the result of 
  ! the intrinsic function %VERIFY.
  ELEMENTAL FUNCTION verify_VS_CH(string, set, back) RESULT(i)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string to search.
    TYPE(varying_string), INTENT(IN) :: string

    !> The CHARACTER string with the set of characters to ignore.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> Optional search direction - if .TRUE. then searching  works back 
    !! from the end of @a string.  If not present or .FALSE. then 
    !! searching works forward from the start of @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns If @a back is not present or .FALSE., the position of the 
    !! leftmostcharacter of @a string that is not in @a set.  If @a back is 
    !! .TRUE., the position of the rightmost character of @a string that is 
    !! not in @a set.  If all characters of @a string are in @a set or the 
    !! length of @a string is zero, then zero.
    INTEGER :: i

    !***************************************************************************

    i = VERIFY(string%chars, set, back)

  END FUNCTION verify_VS_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `var_str`


  !*****************************************************************************
  !!
  !> Procedure for implementation of var_str - convert a CHARACTER string into
  !! a varying_string.
  !!
  !! As the chars component is PUBLIC, this doesn't offer anything beyond 
  !! the compiler provided structure constructor, apart from being a 
  !! procedure that can be associated with a procedure pointer or dummy 
  !! procedure.

  ELEMENTAL FUNCTION var_str_(char) RESULT(string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The CHARACTER string to convert.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: char

    !> @returns a varying_string with the same string of characters as @a char.
    TYPE(varying_string) :: string

    !***************************************************************************

    string%chars = char

  END FUNCTION var_str_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for the `get` generic.


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from the default 
  !! input unit into a varying_string.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen, input is terminated if the 
  !! end of record is encountered.
  !!
  !! This variant provides the characters read as a varying_string.

  ! Forwards the string%chars component to the variant that takes a unit 
  ! number, supplying OUTPUT_UNIT from ISO_FORTRAN_ENV as the unit.
  SUBROUTINE get_VS(string, maxlen, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The characters read from the file.
    TYPE(varying_string), INTENT(OUT) :: string

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL get(OUTPUT_UNIT, string%chars, maxlen, iostat, iomsg)

  END SUBROUTINE get_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from the default 
  !! input unit into a varying_string.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen, input is terminated if the 
  !! end of record is encountered.
  !!
  !! This variant provides the characters read as a deferred length 
  !! allocatable CHARACTER.

  ! Forwards to the variant that takes a unit number, supplying OUTPUT_UNIT 
  ! from ISO_FORTRAN_ENV as the unit.
  SUBROUTINE get_CH(string, maxlen, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The characters read from the file.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE :: string

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL get(OUTPUT_UNIT, string, maxlen, iostat, iomsg)

  END SUBROUTINE get_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from a unit 
  !! connected for formatted stream or sequential input into a varying_string.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen, input is terminated if the 
  !! end of record is encountered.

  SUBROUTINE get_unit_VS(unit, string, maxlen, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from.
    INTEGER, INTENT(IN) :: unit

    !> The characters read from the file.
    TYPE(varying_string), INTENT(OUT) :: string

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.  
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL get(unit, string%chars, maxlen, iostat, iomsg)

  END SUBROUTINE get_unit_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from a unit 
  !! connected for formatted input into a varying_string.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen, input is terminated if the 
  !! end of record is encountered.

  ! This is the ultimate implementation for all forms of the `get` generic 
  ! that do not take a terminating set of characters.
  SUBROUTINE get_unit_CH(unit, string, maxlen, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from.
    INTEGER, INTENT(IN) :: unit

    !> The characters read from the file.
    !!
    !! Not defined if @a iostat is positive.
    !!
    !! If @a iostat is IOSTAT_END and the unit is connected for stream access 
    !! this is defined with the characters in the incomplete final record, 
    !! if any, or a zero length string if there was no incomplete final 
    !! record.  If @a iostat is IOSTAT_END and the unit is not connected 
    !! for stream access, this will be zero length.
    !!
    !! If @a iostat is IOSTAT_EOR, this will be defined with the characters 
    !! read up until the end of record - zero length if there were no 
    !! such characters.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE :: string

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.  
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: access         ! Access to the unit - see get_access.

    ! Number of characters remaining to be read.
    INTEGER :: n_chars_remain

    ! Characters to attempt to acquire in the next read.  We separated this 
    ! from n_chars_read in the original code for clarity, and because 
    ! referencing and defining the same entity in the one statement 
    ! always makes me nervous.
    INTEGER :: n_chars_to_read

    ! Characters actually read.
    INTEGER :: n_chars_read

    ! Buffer for reading a stretch of characters.
    CHARACTER(LEN=GET_BUFFER_LEN, KIND=ck) :: buffer

    INTEGER :: local_iostat   ! Local variant of iostat

    ! Buffer for retrieving error message.
    CHARACTER(:), ALLOCATABLE :: local_iomsg

    ! for compiler bug workaround
    integer :: bug_length

    !***************************************************************************

    ! To reduce the number of combinations required to be handled in logic 
    ! below for of arguments present/not present, we use a temporary buffer 
    ! for retrieving @a iomsg.  If IOMSG isn't going to be defined by this 
    ! procedure (either it is not present, or @a iostat is not present) then 
    ! the temporary buffer is zero length.
    IF (PRESENT(iomsg) .AND. PRESENT(iostat)) THEN
       !=========================================================================
       ! Silliness to work around ifort ICE.
       !
       ! ALLOCATE(CHARACTER(LEN(iomsg)) :: local_iomsg)
       !=========================================================================
       bug_length = LEN(iomsg)
       ALLOCATE(CHARACTER(bug_length) :: local_iomsg)
       !=========================================================================
    ELSE
       ALLOCATE(CHARACTER(0) :: local_iomsg)
    END IF

    string = ""

    CALL get_access(unit, access, local_iostat, local_iomsg)
    IF (local_iostat /= 0) THEN
       ! We need some way of terminating things, just like INQUIRE or 
       ! READ would terminate us.  If the unit above is invalid, the INQUIRE 
       ! call inside get_access should still complete with a zero IOSTAT, 
       ! so that sort of error shouldn't trigger the following.
       IF (.NOT. PRESENT(iostat)) THEN
          ! We cold make local_iomsg non-zero in length and dump it to 
          ! ERROR_UNIT here.  As of F2008, this should be ERROR STOP.
          STOP 'Unable to determine unit access.'
       END IF
       iostat = local_iostat
       IF (PRESENT(iomsg)) iomsg = local_iomsg
       RETURN
    END IF

    IF (PRESENT(maxlen)) THEN
       n_chars_remain = maxlen
    ELSE
       n_chars_remain = HUGE(1)
    END IF

    read_loop : do

       IF (n_chars_remain <= 0) RETURN

       ! For stream access, we get one character at a time.  This is to 
       ! accomodate the possibility of an incomplete record at the 
       ! end of the file.  If @a unit doesn't exist, we treat it like 
       ! a sequential or direct access file.
       IF (access == 3) THEN
          n_chars_to_read = 1
       ELSE
          n_chars_to_read = MIN(n_chars_remain, GET_BUFFER_LEN)
       END IF

       ! Two combinations for the READ statement, based on the presence of the 
       ! IOSTAT argument.
       IF (PRESENT(iostat)) THEN
          READ (  &
               UNIT=unit,  &
               FMT="(A)",  &
               ADVANCE="NO", &
               IOSTAT=iostat,  &
               IOMSG=local_iomsg,  &
               SIZE=n_chars_read )  &
               buffer(:n_chars_to_read)

          IF (IS_IOSTAT_EOR(iostat)) THEN
             ! Depending on the length of the buffer supplied to the procedure, 
             ! there may have been some characters successfully read from the 
             ! record prior to the end-of-record condition.  These are appended 
             ! in the code after the end of read_loop.
             IF (PRESENT(iomsg)) iomsg = local_iomsg
             EXIT read_loop
          END IF
          IF (iostat /= 0) THEN
             IF (PRESENT(iomsg)) iomsg = local_iomsg
             RETURN
          END IF
       ELSE
          ! This may terminate execution on error or end of file.  We don't 
          ! have to care about @a iomsg because it isn't defined if @a iostat 
          ! isn't present.  On EOR we execute the equivalent of EXIT.  The 
          ! comments for the other READ variant about characters read 
          ! on end-of-record also apply.
          READ (  &
               UNIT=unit,  &
               FMT="(A)",  &
               ADVANCE="NO", &
               EOR=100,  &
               SIZE=n_chars_read )  &
               buffer(:n_chars_to_read)
       END IF

       string = string // buffer(:n_chars_read)
       n_chars_remain = n_chars_remain - n_chars_read

    END DO read_loop

100 CONTINUE   ! Here on EOR, either by EXIT or jump to EOR label.
    string = string // buffer(:n_chars_read)

  END SUBROUTINE get_unit_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from the default 
  !! input unit into a varying_string, with input terminated either by 
  !! encountering end of record, or encountering a character 
  !! from a specified set of characters 
  !!
  !! This variant takes the set of characters that may terminate input 
  !! as varying_string.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen or @a set, input is terminated 
  !! if the end of record is encountered.

  ! Forwards to the variant that takes a unit number and @a set as CHARACTER, 
  ! supplying OUTPUT_UNIT from ISO_FORTRAN_ENV as the unit.
  SUBROUTINE get_set_VS_VS(string, set, separator, maxlen, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The characters read from the file.  If input is terminated by 
    !! encountering a character in @a set, that character will not be 
    !! included in @a string.
    TYPE(varying_string), INTENT(OUT) :: string

    !> The set of characters, the occurence of any of which will terminate 
    !! input.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    TYPE(varying_string), INTENT(OUT), OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    IF (PRESENT(separator)) THEN
       CALL get(  &
            OUTPUT_UNIT,  &
            string%chars,  &
            set%chars,  &
            separator%chars,  &
            maxlen,  &
            iostat,  &
            iomsg)
    ELSE
       CALL get(  &
            OUTPUT_UNIT,  &
            string,  &
            set%chars,  &
            MAXLEN=maxlen,  &
            IOSTAT=iostat,  &
            IOMSG=iomsg )
    END IF

  END SUBROUTINE get_set_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from the default 
  !! input unit into a varying_string, with input terminated either by 
  !! encountering end of record, or encountering a character 
  !! from a specified set of characters 
  !!
  !! This variant provides the output string as deferred length 
  !! CHARACTER and takes the set of characters that may terminate input 
  !! as varying_string.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen or @a set, input is terminated 
  !! if the end of record is encountered.

  ! Forwards to the variant that takes a unit number and @a set as CHARACTER, 
  ! supplying OUTPUT_UNIT from ISO_FORTRAN_ENV as the unit.
  SUBROUTINE get_set_CH_VS(string, set, separator, maxlen, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The characters read from the file.  If input is terminated by 
    !! encountering a character in @a set, that character will not be 
    !! included in @a string.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE :: string

    !> The set of characters, the occurence of any of which will terminate 
    !! input.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE, OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL get(OUTPUT_UNIT, string, set%chars, separator, maxlen, iostat, iomsg)

  END SUBROUTINE get_set_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from the default 
  !! input unit into a varying_string, with input terminated either by 
  !! encountering end of record, or encountering a character 
  !! from a specified set of characters 
  !!
  !! This variant takes the set of characters that may terminate input 
  !! as CHARACTER.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen or @a set, input is terminated 
  !! if the end of record is encountered.

  ! Forwards to the variant that takes a unit number, supplying OUTPUT_UNIT 
  ! from ISO_FORTRAN_ENV as the unit.
  SUBROUTINE get_set_VS_CH(string, set, separator, maxlen, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The characters read from the file.  If input is terminated by 
    !! encountering a character in @a set, that character will not be 
    !! included in @a string.
    TYPE(varying_string), INTENT(OUT) :: string

    !> The set of characters, the occurence of any of which in the input 
    !! will terminate input.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    TYPE(varying_string), INTENT(OUT), OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    IF (PRESENT(separator)) THEN
       CALL get(  &
            OUTPUT_UNIT,  &
            string%chars,  &
            set,  &
            separator%chars,  &
            maxlen,  &
            iostat,  &
            iomsg )
    ELSE
       CALL get(  &
            OUTPUT_UNIT,  &
            string%chars,  &
            set,  &
            MAXLEN=maxlen,  &
            IOSTAT=iostat,  &
            IOMSG=iomsg )
    END IF

  END SUBROUTINE get_set_VS_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from the default 
  !! input unit into a varying_string, with input terminated either by 
  !! encountering end of record, or encountering a character 
  !! from a specified set of characters 
  !!
  !! This variant provides the result as deferred length CHARACTER and takes 
  !! the set of characters that may terminate input as CHARACTER.
  !!
  !! Input commences with the next character in the current record, if there 
  !! is a current record, or the first character in the next record if 
  !! there is no current record.  If @iostat is not positive, the file 
  !! position after data transfer is after the last character read.
  !!
  !! Regardless of the value of @a maxlen or @a set, input is terminated 
  !! if the end of record is encountered.

  ! Forwards to the variant that takes a unit number, supplying OUTPUT_UNIT 
  ! from ISO_FORTRAN_ENV as the unit.
  SUBROUTINE get_set_CH_CH(string, set, separator, maxlen, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The characters read from the file.  If input is terminated by 
    !! encountering a character in @a set, that character will not be 
    !! included in @a string.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE :: string

    !> The set of characters, the occurence of any of which in the input 
    !! will terminate input.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE, OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL get(OUTPUT_UNIT, string, set, separator, maxlen, iostat, iomsg)

  END SUBROUTINE get_set_CH_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from a unit 
  !! into a varying_string, with input terminated either by encountering 
  !! end of record or encountering a character from a specified set 
  !! of characters.
  !!
  !! This variant takes the set of characters that may terminate input 
  !! as varying_string.

  ! Forwards to the variant that takes @a set as CHARACTER.
  SUBROUTINE get_unit_set_VS_VS( unit, string, set, separator, maxlen,  &
       iostat, iomsg )

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from, connected for formatted sequential or 
    !! formatted stream read access.
    INTEGER, INTENT(IN) :: unit

    !> The characters read from the file.
    TYPE(varying_string), INTENT(OUT) :: string

    !> The set of characters, the occurence of any of which will terminate 
    !! input.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    TYPE(varying_string), INTENT(OUT), OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    IF (PRESENT(separator)) THEN
       CALL get(  &
            unit,  &
            string%chars,  &
            set%chars,  &
            separator%chars,  &
            MAXLEN=maxlen,  &
            IOSTAT=iostat,  &
            IOMSG=iomsg )
    ELSE
       CALL get(  &
            unit,  &
            string%chars,  &
            set%chars,  &
            MAXLEN=maxlen,  &
            IOSTAT=iostat,  &
            IOMSG=iomsg )
    END IF

  END SUBROUTINE get_unit_set_VS_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from a unit 
  !! into a varying_string, with input terminated either by encountering 
  !! end of record or encountering a character from a specified set 
  !! of characters.
  !!
  !! This variant provides the characters read as deferred length 
  !! CHARACTER and takes the set of characters that may terminate input 
  !! as varying_string.

  ! Forwards to the variant that takes @a set as CHARACTER.
  SUBROUTINE get_unit_set_CH_VS( unit, string, set, separator, maxlen,  &
       iostat, iomsg )

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from, connected for formatted sequential or 
    !! formatted stream read access.
    INTEGER, INTENT(IN) :: unit

    !> The characters read from the file.
    CHARACTER(:, KIND=ck), INTENT(OUT), ALLOCATABLE :: string

    !> The set of characters, the occurence of any of which will terminate 
    !! input.
    TYPE(varying_string), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE, OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL get(unit, string, set%chars, separator, maxlen, iostat, iomsg)

  END SUBROUTINE get_unit_set_CH_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from a unit 
  !! into a varying_string, with input terminated either by encountering 
  !! end of record or encountering a character from a specified set 
  !! of characters.
  !!
  !! This variant takes the set of characters that may terminate input 
  !! as CHARACTER.

  ! This is the ultimate implementation for all forms of the 
  ! get generic that take a set of characters that may terminate the 
  ! input.
  SUBROUTINE get_unit_set_VS_CH( unit, string, set, separator, maxlen,  &
       iostat, iomsg )

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from, connected for formatted sequential or 
    !! formatted stream read access.
    INTEGER, INTENT(IN) :: unit

    !> The characters read from the file.  If input is terminated by 
    !! encountering a character in @a set, that character will not be 
    !! included in @a string.
    TYPE(varying_string), INTENT(OUT) :: string

    !> The set of characters, the occurence of any of which will terminate 
    !! input.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    TYPE(varying_string), INTENT(OUT), OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    IF (PRESENT(separator)) THEN
       CALL get(  &
            unit,  &
            string%chars,  &
            set,  &
            SEPARATOR=separator%chars,  &
            MAXLEN=maxlen,  &
            IOSTAT=iostat,  &
            IOMSG=iomsg )
    ELSE
       CALL get(  &
            unit,  &
            string%chars,  &
            set,  &
            MAXLEN=maxlen,  &
            IOSTAT=iostat,  &
            IOMSG=iomsg )
    END IF

  END SUBROUTINE get_unit_set_VS_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of get - read characters from a unit 
  !! into a varying_string, with input terminated either by encountering 
  !! end of record or encountering a character from a specified set 
  !! of characters.
  !!
  !! This variant takes the set of characters that may terminate input 
  !! as CHARACTER and provides the resulting string as CHARACTER.

  ! This is the ultimate implementation for all forms of the 
  ! get generic that take a set of characters that may terminate the 
  ! input.
  SUBROUTINE get_unit_set_CH_CH( unit, string, set, separator, maxlen,  &
       iostat, iomsg )

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from, connected for formatted sequential or 
    !! formatted stream read access.
    INTEGER, INTENT(IN) :: unit

    !> The characters read from the file.  If input is terminated by 
    !! encountering a character in @a set, that character will not be 
    !! included in @a string.
    CHARACTER(:, KIND=ck), INTENT(OUT), ALLOCATABLE :: string

    !> The set of characters, the occurence of any of which will terminate 
    !! input.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> Optional character that was encountered that terminated input.  If 
    !! input was terminated other than by encountering a character in @a set, 
    !! then this will be zero length.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE, OPTIONAL :: separator

    !> Optional maximum number of characters to read.  If not present then 
    !! a maximum of HUGE(1) will be used.  If negative or zero then no 
    !! characters will be read and @a string will be set to zero length.
    INTEGER, INTENT(IN), OPTIONAL :: maxlen

    !> Optional status of the read.  Set to zero if a valid read occurs 
    !! and the end of record is not reached, set to a positive value if 
    !! an error occurs and set to a negative value (from the intrinsic 
    !! module ISO_FORTRAN_ENV) if an end of file (IOSTAT_END) or end of 
    !! record condition (IOSTAT_EOR) occurs.  If not present and an 
    !! error or end of file condition occurs (but not end of record) 
    !! then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Number of characters remaining to be read.
    INTEGER :: n_chars_remain

    ! Single character buffer for reading a stretch of characters.  This has 
    ! to be a single character because of the need to search for a terminating 
    ! character from @a sep.
    CHARACTER(LEN=1, KIND=ck) :: ch

    INTEGER :: i_set          ! Index of the character just read in @a set.

    ! Buffer for retrieving error message.
    CHARACTER(:), ALLOCATABLE :: local_iomsg

    ! for compiler bug workaround
    integer :: bug_length

    !***************************************************************************

    ! To reduce the number of combinations required to be handled in logic 
    ! below for of arguments present/not present, we use a temporary buffer 
    ! for retrieving @a iomsg.  If IOMSG isn't going to be defined by this 
    ! procedure (either it is not present, or @a iostat is not present) then 
    ! the temporary buffer is zero length.
    IF (PRESENT(iomsg) .AND. PRESENT(iostat)) THEN
       !=========================================================================
       ! Silliness to work around ifort 16.0.2 ICE.
       !
       ! ALLOCATE(CHARACTER(LEN(iomsg)) :: local_iomsg)
       !=========================================================================
       bug_length = LEN(iomsg)
       ALLOCATE(CHARACTER(bug_length) :: local_iomsg)
       !=========================================================================
    ELSE
       ALLOCATE(CHARACTER(0) :: local_iomsg)
    END IF

    string = ""

    IF (PRESENT(maxlen)) THEN
       n_chars_remain = maxlen
    ELSE
       n_chars_remain = HUGE(1)
    END IF

    IF (PRESENT(separator)) separator = ""

    read_loop : do

       IF (n_chars_remain <= 0) RETURN

       ! Two combinations for the READ statement, based on the presence of the 
       ! IOSTAT argument.
       !
       ! Each read gets at most one character, so no need to worry about 
       ! the number of characters read (one if things worked, zero otherwise).  
       IF (PRESENT(iostat)) THEN
          READ ( &
               UNIT=unit,  &
               FMT="(A)",  &
               ADVANCE="NO",  &
               IOSTAT=iostat,  &
               IOMSG=local_iomsg )  &
               ch
          IF (iostat /= 0) EXIT read_loop
       ELSE
          ! This may terminate execution on error or end of file.  We don't 
          ! have to care about @a iomsg because it isn't defined if @a iostat 
          ! isn't present.  On EOR we execute the equivalent of EXIT.
          READ ( &
               UNIT=unit,  &
               FMT="(A1)",  &
               ADVANCE="NO",  &
               EOR=100 )  &
               ch
       END IF

       ! Go see if the character read was in the terminating set.
       i_set = SCAN(ch, set)

       IF (i_set == 1) THEN
          IF (PRESENT(separator)) separator = ch
          EXIT read_loop
       END IF

       string = string // ch
       n_chars_remain = n_chars_remain - 1

    END DO read_loop

100 CONTINUE
    ! Here on EOR, either by EXIT or jump to EOR label, or by encountering 
    ! a character from @a set.

  END SUBROUTINE get_unit_set_CH_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------  
  !
  ! Specific procedures for the `put` generic.


  !*****************************************************************************
  !!
  !> Append a string to the current record of the default unit, or to the 
  !! next record if there is no current record.
  !!
  !! This variant takes the string as varying_string.
  !!
  !! The output is non-advancing.

  ! Forwards to the variant that takes a unit.
  SUBROUTINE put_VS(string, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The string to be transferred.
    TYPE(varying_string), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL put(OUTPUT_UNIT, string%chars, iostat, iomsg)

  END SUBROUTINE put_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of put - append a string to the 
  !! current record of the default unit, or to the next record if there 
  !! is no current record.
  !!
  !! This variant takes the string as CHARACTER.
  !!
  !! The output is non-advancing.

  SUBROUTINE put_CH(string, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The string to be transferred.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL put(OUTPUT_UNIT, string, IOSTAT=iostat, IOMSG=iomsg)

  END SUBROUTINE put_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of put - append a varying_string to the 
  !! current record of the specified unit, or to the next record if there 
  !! is no current record.
  !!
  !! This variant takes the string as varying_string.
  !!
  !! The output is non-advancing.

  ! Forwards to the CHARACTER variant.
  SUBROUTINE put_unit_VS(unit, string, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to write to, connected for formatted sequential or 
    !! formatted stream write access.
    INTEGER, INTENT(IN) :: unit

    !> The string to be transferred.
    TYPE(varying_string), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL put(unit, string%chars, iostat, iomsg)

  END SUBROUTINE put_unit_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of put - append a CHARACTER string to the 
  !! current record of the specified unit, or to the next record if there 
  !! is no current record.
  !!
  !! This variant takes the string as CHARACTER.
  !!
  !! The output is non-advancing.

  ! This is the common implementation for all specifics for the put generic.
  SUBROUTINE put_unit_CH(unit, string, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to write to, connected for formatted sequential or 
    !! formatted stream write access.
    INTEGER, INTENT(IN) :: unit

    !> The string to be transferred.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Buffer for retrieving error message.
    CHARACTER(:), ALLOCATABLE :: local_iomsg

    ! for compiler bug workaround
    integer :: bug_length

    !***************************************************************************

    ! To reduce the number of combinations required to be handled in logic 
    ! below for of arguments present/not present, we use a temporary buffer 
    ! for retrieving @a iomsg.  If IOMSG isn't going to be defined by this 
    ! procedure (either it is not present, or @a iostat is not present) then 
    ! the temporary buffer is zero length.
    IF (PRESENT(iomsg) .AND. PRESENT(iostat)) THEN
       !=========================================================================
       ! Silliness to work around ifort ICE.
       !
       ! ALLOCATE(CHARACTER(LEN(iomsg)) :: local_iomsg)
       !=========================================================================
       bug_length = LEN(iomsg)
       ALLOCATE(CHARACTER(bug_length) :: local_iomsg)
       !=========================================================================
    ELSE
       ALLOCATE(CHARACTER(0) :: local_iomsg)
    END IF

    IF (PRESENT(iostat)) THEN
       WRITE (  &
            UNIT=unit,  &
            FMT="(A)",  &
            ADVANCE="NO",  &
            IOSTAT=iostat,  &
            IOMSG=local_iomsg )  &
            string
       IF (iostat /= 0) THEN
          IF (PRESENT(iomsg)) iomsg = local_iomsg
       END IF
    ELSE
       WRITE (UNIT=unit, FMT="(A)", ADVANCE="NO") string
    END IF

  END SUBROUTINE put_unit_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `put_line`.


  !*****************************************************************************
  !!
  !> Procedure for implementation of put_line - append a varying string to the 
  !! current record of the default unit, or the next record if there 
  !! is no current record, terminating the record.

  ! Forwards to the variant that takes a unit, supplying OUTPUT_UNIT.
  SUBROUTINE put_line_VS(string, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The string to be transferred.
    TYPE(varying_string), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL put_line(OUTPUT_UNIT, string%chars, iostat, iomsg)

  END SUBROUTINE put_line_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of put_line - append a CHARACTER string to 
  !! the current record of the default unit, terminating the record.

  ! Forwards to the variant that takes a unit, supplying OUTPUT_UNIT.
  SUBROUTINE put_line_CH(string, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The string to be transferred.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL put_line(OUTPUT_UNIT, string, iostat, iomsg)

  END SUBROUTINE put_line_CH


  !*****************************************************************************
  !!
  !> Procedure for implementation of put_line - append a varying_string to 
  !! the current record of a specified unit, terminating the record.

  ! Forwards to the variant that takes @a string as CHARACTER.
  SUBROUTINE put_line_unit_VS(unit, string, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to write to, connected for formatted sequential or 
    !! formatted stream write access.
    INTEGER, INTENT(IN) :: unit

    !> The string to be transferred.
    TYPE(varying_string), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !***************************************************************************

    CALL put_line(unit, string%chars, iostat, iomsg)

  END SUBROUTINE put_line_unit_VS


  !*****************************************************************************
  !!
  !> Procedure for implementation of put_line - append a CHARACTER string to 
  !! the current record of a specified unit, terminating the record.

  ! This is the common implementation for all specific procedures of put_line.
  SUBROUTINE put_line_unit_CH(unit, string, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to write to, connected for formatted sequential or 
    !! formatted stream write access.
    INTEGER, INTENT(IN) :: unit

    !> The string to be transferred.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> Optional status code - positive value on error.  If not present and 
    !! an error occurs then execution is terminated.
    INTEGER, INTENT(OUT), OPTIONAL :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is both 
    !! present and defined with a non-zero value.
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Buffer for retrieving error message.
    CHARACTER(:), ALLOCATABLE :: local_iomsg

    ! for compiler bug workaround
    integer :: bug_length

    !***************************************************************************

    ! To reduce the number of combinations required to be handled in logic 
    ! below for of arguments present/not present, we use a temporary buffer 
    ! for retrieving @a iomsg.  If IOMSG isn't going to be defined by this 
    ! procedure (either it is not present, or @a iostat is not present) then 
    ! the temporary buffer is zero length.
    IF (PRESENT(iomsg) .AND. PRESENT(iostat)) THEN
       !=========================================================================
       ! Silliness to work around ifort ICE.
       !
       ! ALLOCATE(CHARACTER(LEN(iomsg)) :: local_iomsg)
       !=========================================================================
       bug_length = LEN(iomsg)
       ALLOCATE(CHARACTER(bug_length) :: local_iomsg)
       !=========================================================================
    ELSE
       ALLOCATE(CHARACTER(0) :: local_iomsg)
    END IF

    ! By using an explicit format to advance to the next record, we force 
    ! the behaviour to match the description even when this is called 
    ! as part of processing a child output statement.  Presumably that 
    ! is a good thing.
    IF (PRESENT(iostat)) THEN
       WRITE (  &
            UNIT=unit,  &
            FMT="(A,/)",  &
            ADVANCE="NO",  &
            IOSTAT=iostat,  &
            IOMSG=local_iomsg )  &
            string
       IF (iostat /= 0) THEN
          IF (PRESENT(iomsg)) iomsg = local_iomsg
       END IF
    ELSE
       WRITE (UNIT=unit, FMT="(A,/)", ADVANCE="NO") string
    END IF

  END SUBROUTINE put_line_unit_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `Extract`.


  !*****************************************************************************
  !!
  !> Extracts a specified substring from a string.
  !!
  !! This variant takes the principal string as varying_string.

  ELEMENTAL FUNCTION extract_VS(string, start, finish) RESULT(ext_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to extract the substring from.
    TYPE(varying_string), INTENT(IN) :: string

    !> Optional inclusive start position of the substring to be extracted.  If 
    !! not present or less than one then one is used for the start position.
    INTEGER, INTENT(IN), OPTIONAL :: start

    !> Optional inclusive finish position of the substring to be extracted.  
    !! If not present or greater than LEN(@a string) then LEN(@a string) is 
    !! used for the finish position.
    INTEGER, INTENT(IN), OPTIONAL :: finish

    !> @returns The substring of @a string between the start and finish 
    !! positions inclusive.  If the finish position is less than the start 
    !! position a zero length string is returned.
    TYPE(varying_string) :: ext_string

    !***************************************************************************

    ext_string = extract(string%chars, start, finish)

  END FUNCTION extract_VS


  !*****************************************************************************
  !!
  !> Extracts a specified substring from a string.
  !!
  !! This variant takes the principal string as CHARACTER.

  ELEMENTAL FUNCTION extract_CH(string, start, finish) RESULT(ext_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to extract the substring from.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> Optional inclusive start position of the substring to be extracted.  If 
    !! not present or less than one then one is used for the start position.
    INTEGER, INTENT(IN), OPTIONAL :: start

    !> Optional inclusive finish position of the substring to be extracted.  
    !! If not present or greater than LEN(@a string) then LEN(@a string) is 
    !! used for the finish position.
    INTEGER, INTENT(IN), OPTIONAL :: finish

    !> @returns The substring of @a string between the start and finish 
    !! positions inclusive.  If the finish position is less than the start 
    !! position a zero length string is returned.
    TYPE(varying_string) :: ext_string

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: start_         ! Local copy of @a start.
    INTEGER :: finish_        ! Local copy of @a finish.

    !***************************************************************************

    IF (PRESENT(start)) THEN
       start_ = MAX(1, start)
    ELSE
       start_ = 1
    END IF

    IF (PRESENT(finish)) THEN
       finish_ = MIN(LEN(string), finish)
    ELSE
       finish_ = LEN(string)
    END IF

    ext_string = string(start_:finish_)

  END FUNCTION extract_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `Insert`.


  !*****************************************************************************
  !!
  !> Specific procedure for Insert - insert a substring into a string at 
  !! a specified position.
  !!
  !! This variant takes the principal string and the substring as 
  !! varying_string.

  ELEMENTAL FUNCTION insert_VS_VS(string, start, substring) RESULT(ins_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to have the substring inserted.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position.  If less than one then a start position of one is 
    !! used, if greater than LEN(@a string) then a start position of 
    !! LEN(@a string) + 1 is used.
    INTEGER, INTENT(IN) :: start

    !> The substring to insert.
    TYPE(varying_string), INTENT(IN) :: substring

    !> @returns A copy of @a string with the characters of @a substring 
    !! inserted before the start position.
    TYPE(varying_string) :: ins_string

    !***************************************************************************

    ins_string = insert(string%chars, start, substring%chars)

  END FUNCTION insert_VS_VS


  !*****************************************************************************
  !!
  !> Specific procedure for Insert - insert a substring into a string at 
  !! a specified position.
  !!
  !! This variant takes the principal string as CHARACTER and the substring 
  !! as varying_string.

  ELEMENTAL FUNCTION insert_CH_VS(string, start, substring) RESULT(ins_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to have the substring inserted.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position.  If less than one then a start position of one is 
    !! used, if greater than LEN(@a string) then a start position of 
    !! LEN(@a string) + 1 is used.
    INTEGER, INTENT(IN) :: start

    !> The substring to insert.
    TYPE(varying_string), INTENT(IN) :: substring

    !> @returns A copy of @a string with the characters of @a substring 
    !! inserted before the start position.
    TYPE(varying_string) :: ins_string

    !***************************************************************************

    ins_string = insert(string, start, substring%chars)

  END FUNCTION insert_CH_VS


  !*****************************************************************************
  !!
  !> Specific procedure for Insert - insert a substring into a string at 
  !! a specified position.
  !!
  !! This variant takes the principal string as varying_string and the 
  !! substring as CHARACTER.

  ELEMENTAL FUNCTION insert_VS_CH(string, start, substring) RESULT(ins_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to have the substring inserted.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position.  If less than one then a start position of one is 
    !! used, if greater than LEN(@a string) then a start position of 
    !! LEN(@a string) + 1 is used.
    INTEGER, INTENT(IN) :: start

    !> The substring to insert.
    CHARACTER(LEN=*), INTENT(IN) :: substring

    !> @returns A copy of @a string with the characters of @a substring 
    !! inserted before the start position.
    TYPE(varying_string) :: ins_string

    !***************************************************************************

    ins_string = insert(string%chars, start, substring)

  END FUNCTION insert_VS_CH


  !*****************************************************************************
  !!
  !> Specific procedure for Insert - insert a substring into a string at 
  !! a specified position.
  !!
  !! This variant takes the principal string and the substring as 
  !! varying_string.

  ELEMENTAL FUNCTION insert_CH_CH(string, start, substring) RESULT(ins_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to have the substring inserted.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position.  If less than one then a start position of one is 
    !! used, if greater than LEN(@a string) then a start position of 
    !! LEN(@a string) + 1 is used.
    INTEGER, INTENT(IN) :: start

    !> The substring to insert.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> @returns A copy of @a string with the characters of @a substring 
    !! inserted before the start position.
    TYPE(varying_string) :: ins_string

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: start_         ! Local variant of @a start.

    !***************************************************************************

    start_ = MAX(1, MIN(start, LEN(string)+1))

    ins_string = string(:start_-1) // substring // string(start_:)

  END FUNCTION insert_CH_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `Remove`.


  !*****************************************************************************
  !!
  !> Specific procedure for Remove - remove a substring specified by 
  !! a range from a string.
  !!
  !! This variant takes the principal string as varying_string.

  ELEMENTAL FUNCTION remove_VS(string, start, finish) RESULT(rem_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to have a substring removed from it.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position of the substring to be removed.  If not present 
    !! or less than one then the start position is one.
    INTEGER, INTENT(IN), OPTIONAL :: start

    !> The finish position of the substring to be removed.  If not present 
    !! or greater than LEN(@a string) then the finish position is 
    !! LEN(@a string).
    INTEGER, INTENT(IN), OPTIONAL :: finish

    !> @returns A copy of @a string with the characters between the start 
    !! position and the finish position, inclusive, removed.  If the 
    !! finish position is less than the start position then @a string 
    !! is returned unchanged.
    TYPE(varying_string) :: rem_string

    !***************************************************************************

    rem_string = remove(string%chars, start, finish)

  END FUNCTION remove_VS


  !*****************************************************************************
  !!
  !> Specific procedure for Remove - remove a substring specified by 
  !! a range from a string.
  !!
  !! This variant takes the principal string as varying_string.

  ELEMENTAL FUNCTION remove_CH(string, start, finish) RESULT(rem_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to have a substring removed from it.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position of the substring to be removed.  If not present 
    !! or less than one then the start position is one.
    INTEGER, INTENT(IN), OPTIONAL :: start

    !> The finish position of the substring to be removed.  If not present 
    !! or greater than LEN(@a string) then the finish position is 
    !! LEN(@a string).
    INTEGER, INTENT(IN), OPTIONAL :: finish

    !> @returns A copy of @a string with the characters between the start 
    !! position and the finish position, inclusive, removed.  If the 
    !! finish position is less than the start position then @a string 
    !! is returned unchanged.
    TYPE(varying_string) :: rem_string

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: start_         ! The start position.
    INTEGER :: finish_        ! The finish position.

    !***************************************************************************

    IF (PRESENT(start)) THEN
       start_ = MAX(1, start)
    ELSE
       start_ = 1
    END IF

    IF (PRESENT(finish)) THEN
       finish_ = MIN(LEN(string), finish)
    ELSE
       finish_ = LEN(string)
    END IF

    IF (finish_ >= start_) THEN
       rem_string = string(:start_-1) // string(finish_+1:)
    ELSE
       rem_string = string
    END IF

  END FUNCTION remove_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `Replace`.


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of 
  !! characters from a given index through to the end of a string by 
  !! a given substring.
  !!
  !! This particular specific takes the principal string and the 
  !! replacement string as varying_string.
  !!
  !! Characters are inserted into a copy of @a string starting at @a start.  
  !! The original characters in the copy of @a string are deleted from 
  !! @a start to MIN(@a start + LEN(@a substring) - 1, LEN(@a string)).  

  ELEMENTAL FUNCTION replace_VS_VS_auto(string, start, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! greater than LEN(@a string) then the value LEN(@a string) + is used and 
    !! @a substring is appended to a copy of @a string.  If less than one, 
    !! the value of one is used for the starting position.
    INTEGER, INTENT(IN) :: start

    !> The string to replace the range of characters from @a start to the 
    !! end of @a string.
    TYPE(varying_string), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(  &
         string%chars,  &
         start,  &
         MAX(start, 1) + len(substring) - 1,  &
         substring%chars )

  END FUNCTION replace_VS_VS_auto


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of 
  !! characters from a given index through to the end of a string by 
  !! a given substring.
  !!
  !! This particular specific takes the principal string as CHARACTER and 
  !! the replacement string as varying_string.
  !!
  !! Characters are inserted into a copy of @a string starting at @a start.  
  !! The original characters in the copy of @a string are deleted from 
  !! @a start to MIN(@a start + LEN(@a substring) - 1, LEN(@a string)).  

  ELEMENTAL FUNCTION replace_CH_VS_auto (string, start, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! greater than LEN(@a string) then the value LEN(@a string) + is used and 
    !! @a substring is appended to a copy of @a string.  If less than one, 
    !! the value of one is used for the starting position.
    INTEGER, INTENT(IN) :: start

    !> The string to replace the range of characters from @a start to the 
    !! end of @a string.
    TYPE(varying_string), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(  &
         string,  &
         start,  &
         MAX(start, 1) + len(substring) - 1,  &
         substring%chars )

  END FUNCTION replace_CH_VS_auto


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of 
  !! characters from a given index through to the end of a string by 
  !! a given substring.
  !!
  !! This particular specific takes the principal string and the 
  !! replacement string as CHARACTER.
  !!
  !! Characters are inserted into a copy of @a string starting at @a start.  
  !! The original characters in the copy of @a string are deleted from 
  !! @a start to MIN(@a start + LEN(@a substring) - 1, LEN(@a string)).  

  ELEMENTAL FUNCTION replace_VS_CH_auto (string, start, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! greater than LEN(@a string) then the value LEN(@a string) + is used and 
    !! @a substring is appended to a copy of @a string.  If less than one, 
    !! the value of one is used for the starting position.
    INTEGER, INTENT(IN) :: start

    !> The string to replace the range of characters from @a start to the 
    !! end of @a string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(  &
         string%chars,  &
         start,  &
         MAX(start, 1) + LEN(substring) - 1,  &
         substring )

  END FUNCTION replace_VS_CH_auto


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of 
  !! characters from a given index through to the end of a string by 
  !! a given substring.
  !!
  !! This particular specific takes the principal string and the 
  !! replacement string as CHARACTER.
  !!
  !! Characters are inserted into a copy of @a string starting at @a start.  
  !! The original characters in the copy of @a string are deleted from 
  !! @a start to MIN(@a start + LEN(@a substring) - 1, LEN(@a string)).  

  ELEMENTAL FUNCTION replace_CH_CH_auto(string, start, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! greater than LEN(@a string) then the value LEN(@a string) + is used and 
    !! @a substring is appended to a copy of @a string.  If less than one, 
    !! the value of one is used for the starting position.
    INTEGER, INTENT(IN) :: start

    !> The string to replace the range of characters from @a start.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(  &
         string,  &
         start,  &
         MAX(start, 1) + LEN(substring) - 1,  &
         substring )

  END FUNCTION replace_CH_CH_auto


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of 
  !! characters in a string by a given substring.
  !!
  !! This particular specific takes the principal string and the 
  !! replacement string as varying_string.
  !!
  !! The positions nominated by @a start and @a finish are inclusive.  If 
  !! @a finish is less than @a start, the characters of @a substring are 
  !! before the character at @a start and no characters are deleted.

  ELEMENTAL FUNCTION replace_VS_VS_fixed(string, start, finish, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! this is less than one then a starting position of one is used.
    INTEGER, INTENT(IN) :: start

    !> The finish position of the range of characters to be replaced.  If 
    !! this is greater than LEN(@a string) then the finish position 
    !! is LEN(@a string).
    INTEGER, INTENT(IN) :: finish

    !> The string to replace the range of characters between @a start and 
    !! @a finish in @a string.
    TYPE(varying_string), INTENT(IN) :: substring

    !> @returns The string that results from the replacement. 
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string%chars, start, finish, substring%chars)

  END FUNCTION replace_VS_VS_fixed


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of 
  !! characters in a string by a given substring.
  !!
  !! This particular specific takes the principal string as CHARACTER and the 
  !! replacement string as varying_string.
  !!
  !! The positions nominated by @a start and @a finish are inclusive.  If 
  !! @a finish is less than @a start, the characters of @a substring are 
  !! before the character at @a start and no characters are deleted.

  ELEMENTAL FUNCTION replace_CH_VS_fixed(string, start, finish, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! this is less than one then a starting position of one is used.
    INTEGER, INTENT(IN) :: start

    !> The finish position of the range of characters to be replaced.  If 
    !! this is greater than LEN(@a string) then the finish position 
    !! is LEN(@a string).
    INTEGER, INTENT(IN) :: finish

    !> The string to replace the range of characters between @a start and 
    !! @a finish in @a string.
    TYPE(varying_string), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    ! Replace part of a character string with a varying
    ! substring

    rep_string = replace(string, start, finish, substring%chars)

  END FUNCTION replace_CH_VS_fixed


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of the 
  !! characters in a string by a given substring.
  !!
  !! This particular specific takes the principal string as varying_string and 
  !! the replacement string as CHARACTER.
  !!
  !! The positions nominated by @a start and @a finish are inclusive.  If 
  !! @a finish is less than @a start, the characters of @a substring are 
  !! before the character at @a start and no characters are deleted.

  ELEMENTAL FUNCTION replace_VS_CH_fixed(string, start, finish, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! this is less than one then a starting position of one is used.
    INTEGER, INTENT(IN) :: start

    !> The finish position of the range of characters to be replaced.  If 
    !! this is greater than LEN(@a string) then the finish position 
    !! is LEN(@a string).
    INTEGER, INTENT(IN) :: finish

    !> The string to replace the range of characters between @a start and 
    !! @a finish in @a string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string%chars, start, finish, substring)

  END FUNCTION replace_VS_CH_fixed


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a range of the 
  !! characters in a string by a given substring.
  !!
  !! This particular specific takes all string arguments as CHARACTER.
  !!
  !! The positions nominated by @a start and @a finish are inclusive.  If 
  !! @a finish is less than @a start, the characters of @a substring are 
  !! before the character at @a start and no characters are deleted.

  ! This variant contains the processing common to all other range REPLACE 
  ! specifics.
  ELEMENTAL FUNCTION replace_CH_CH_fixed(string, start, finish, substring)  &
       RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with the range to be replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The start position of the range of characters to be replaced.  If 
    !! this is less than one then a starting position of one is used.
    INTEGER, INTENT(IN) :: start

    !> The finish position of the range of characters to be replaced.  If 
    !! this is greater than LEN(@a string) then the finish position 
    !! is LEN(@a string).
    INTEGER, INTENT(IN) :: finish

    !> The string to replace the range of characters between @a start and 
    !! @a finish in @a string.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: start_         ! The greater of @a start or 1.
    INTEGER :: finish_        ! The lessor of @a finish or LEN(@a string).

    !***************************************************************************

    ! Replace part of a character string with a character
    ! substring

    start_ = MAX(1, start)
    finish_ = MIN(LEN(string), finish)

    IF (finish_ < start_) THEN
       rep_string = insert(string, start_, substring)
    ELSE
       rep_string = string(:start_-1) // substring // string(finish_+1:)
    END IF

  END FUNCTION replace_CH_CH_fixed


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This particular specific takes all string arguments as varying_string.

  ! Implementation just hands off to the all character variant.
  ELEMENTAL FUNCTION replace_VS_VS_VS_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The string to search for.
    TYPE(varying_string), INTENT(IN) :: target

    !> The string to replace @a target.
    TYPE(varying_string), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(  &
         string%chars,  &
         target%chars,  &
         substring%chars,  &
         every, back )

  END FUNCTION replace_VS_VS_VS_target


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This particular specific takes the @a string argument as a CHARACTER 
  !! and the @a target and @a substring arguments as varying_string.

  ! Implementation just hands off to the all character variant.
  ELEMENTAL FUNCTION replace_CH_VS_VS_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The string to search for.
    TYPE(varying_string), INTENT(IN) :: target

    !> The string to replace @a target.
    TYPE(varying_string), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string, target%chars, substring%chars, every, back)

  END FUNCTION replace_CH_VS_VS_target


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This particular specific take the @a string and @a substring arguments 
  !! as varying_string, @a target as CHARACTER.

  ! Implementation just hands off to the all character variant.
  ELEMENTAL FUNCTION replace_VS_CH_VS_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The string to search for.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: target

    !> The string to replace @a target.
    TYPE(varying_string), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string%chars, target, substring%chars, every, back)

  END FUNCTION replace_VS_CH_VS_target


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This particular specific take the @a substring argument as 
  !! varying_string, @a string and @a target as CHARACTER.

  ! Implementation just hands off to the all character variant.
  ELEMENTAL FUNCTION replace_CH_CH_VS_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The string to search for.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: target

    !> The string to replace @a target.
    TYPE(varying_string), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string, target, substring%chars, every, back)

  END FUNCTION replace_CH_CH_VS_target


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This specific takes the @a string and @a target arguments as 
  !! varying_string, @a substring as CHARACTER.

  ! Implementation just hands off to the all character variant.
  ELEMENTAL FUNCTION replace_VS_VS_CH_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The string to search for.
    TYPE(varying_string), INTENT(IN) :: target

    !> The string to replace @a target.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string%chars, target%chars, substring, every, back)

  END FUNCTION replace_VS_VS_CH_target


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This specific takes the @a string and @a substring arguments as 
  !! varying_string, and the @a target argument as CHARACTER.

  ! Implementation just hands off to the all character variant.
  ELEMENTAL FUNCTION replace_CH_VS_CH_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The string to search for.
    TYPE(varying_string), INTENT(IN) :: target

    !> The string to replace @a target.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string, target%chars, substring, every, back)

  END FUNCTION replace_CH_VS_CH_target


  !*****************************************************************************
  !!
  !> Specific procedure for the REPLACE generic - replace part of a 
  !! varying string with a character substring, at a location matching 
  !! a character-string target.
  !!
  !! This specific takes the @a string argument as varying_string, and 
  !! the @a target and @a substring arguments as CHARACTER.

  ELEMENTAL FUNCTION replace_VS_CH_CH_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    TYPE(varying_string), INTENT(IN) :: string

    !> The string to search for.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: target

    !> The string to replace @a target.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !***************************************************************************

    rep_string = replace(string%chars, target, substring, every, back)

  END FUNCTION replace_VS_CH_CH_target


  !*****************************************************************************
  !!
  !> Specific procedure for the Replace generic - replaces a subset of the 
  !! characters in a string by a given substring.
  !!
  !! This specific takes all string input arguments as type CHARACTER.

  ELEMENTAL FUNCTION replace_CH_CH_CH_target( string, target, substring,  &
       every, back ) RESULT(rep_string)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string with contents to be searched and replaced.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: string

    !> The string to search for.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: target

    !> The string to replace @a target.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: substring

    !> If present and true, then every occurrence of target in @a string will 
    !! be replaced with @a substring.  Otherwise only the first occurrence is 
    !! replaced.
    LOGICAL, INTENT(IN), OPTIONAL :: every

    !> If present and true, then the search for @a target in @a string 
    !! begins from the back.  Otherwise the search beings from the front.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !> @returns The string that results from the replacement.
    TYPE(varying_string) :: rep_string

    !---------------------------------------------------------------------------
    ! Locals

    LOGICAL :: every_         ! Local defaulted variant of @a every.
    LOGICAL :: back_          ! Local defaulted variant of @a back.

    ! Progressively replaced copy of @a string.
    CHARACTER(LEN=:, KIND=ck), ALLOCATABLE :: work_string

    INTEGER :: length_target  ! Length of the target string.
    INTEGER :: i_target       ! Index of the target in @a work_string.

    !***************************************************************************

    ! Handle special cases when LEN(target) == 0. Such instances are 
    ! prohibited by the standard, but since this function is elemental, 
    ! no error can be thrown. Therefore, it makes sense to handle them 
    ! in a sensible manner

    IF (LEN(target) == 0) THEN
       IF (LEN(string) /= 0) THEN
          rep_string = string
       ELSE
          rep_string = substring
       END IF
       RETURN
    END IF

    ! Set local copies of the values of the optional arguments, 
    ! defaulted where those arguments are not present.
    IF (PRESENT(every)) THEN
       every_ = every
    ELSE
       every_ = .false.
    END IF

    IF (PRESENT(back)) THEN
       back_ = back
    ELSE
       back_ = .false.
    END IF

    rep_string = ""

    work_string = string

    length_target = LEN(target)

    replace_loop : DO

       i_target = INDEX(work_string, target, back_)

       IF (i_target == 0) EXIT replace_loop

       IF (back_) THEN
          rep_string = substring  &
               // work_string(i_target+length_target:)  &
               // rep_string
          work_string = work_string(:i_target-1)
       ELSE
          rep_string = rep_string  &
               // work_string(:i_target-1)  &
               // substring
          work_string = work_string(i_target+length_target:)
       END IF

       IF (.NOT. every_) EXIT replace_loop

    END DO replace_loop

    IF (back_) THEN
       rep_string = work_string // rep_string
    ELSE
       rep_string = rep_string // work_string
    END IF

  END FUNCTION replace_CH_CH_CH_target


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for `Split`.


  !*****************************************************************************
  !!
  !> Specific procedure for the SPLIT generic - splits a string into 
  !! two substrings with the substrings specified by the occurence of a 
  !! character from a specified character set.
  !!
  !! This specific has a set of type varying_string.

  ! This implementation simply converts @a set to character and then forwards 
  ! to the other specific.
  ELEMENTAL SUBROUTINE split_VS_VS_VS(string, word, set, separator, back)

    !---------------------------------------------------------------------------
    ! Arguments

    !> On input, the string to split.  On output, this is the 
    !! remainder of the string, excluding the separator character.
    TYPE(varying_string), INTENT(INOUT) :: string

    !> The fragment of the input @a string prior to encountering the 
    !! separator.
    TYPE(varying_string), INTENT(OUT) :: word

    !> The set of separator characters.
    TYPE(varying_string), INTENT(IN) :: set

    !> The separator encountered in @a string, or zero length if no 
    !! separator was found.
    TYPE(varying_string), INTENT(OUT), OPTIONAL :: separator

    !> Nominates the direction of search.  If not present or false, then 
    !! the search for the separator commences from the start of @a string, 
    !! if present and true then the search commences from the end of 
    !! @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !***************************************************************************

    IF (PRESENT(separator)) THEN
       CALL split(string%chars, word%chars, set%chars, separator%chars, back)
    ELSE
       CALL split(string%chars, word%chars, set%chars, BACK=back)
    END IF

  END SUBROUTINE split_VS_VS_VS


  !*****************************************************************************
  !!
  !> Specific procedure for the SPLIT generic - splits a string into 
  !! two substrings with the substrings specified by the occurence of a 
  !! character from a specified character set.
  !!
  !! This specific has a set of type CHARACTER.

  ELEMENTAL SUBROUTINE split_VS_VS_CH(string, word, set, separator, back)

    !---------------------------------------------------------------------------
    ! Arguments

    !> On input, the string to split.  On output, this is the 
    !! remainder of the string, excluding the separator character.
    TYPE(varying_string), INTENT(INOUT) :: string

    !> The fragment of the input @a string prior to encountering the 
    !! separator.
    TYPE(varying_string), INTENT(OUT) :: word

    !> The set of separator characters.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> The separator encountered in @a string, or zero length if no 
    !! separator was found.
    TYPE(varying_string), INTENT(OUT), OPTIONAL :: separator

    !> Nominates the direction of search.  If not present or false, then 
    !! the search for the separator commences from the start of @a string, 
    !! if present and true then the search commences from the end of 
    !! @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !***************************************************************************

    IF (PRESENT(separator)) THEN
       CALL split(string%chars, word%chars, set, separator%chars, back)
    ELSE
       CALL split(string%chars, word%chars, set, BACK=back)
    END IF

  END SUBROUTINE split_VS_VS_CH


  !*****************************************************************************
  !!
  !> Specific procedure for the SPLIT generic - splits a string into 
  !! two substrings with the substrings specified by the occurence of a 
  !! character from a specified character set.
  !!
  !! This specific has a set of type CHARACTER.

  ! This has the common implementation for all the specific procedures 
  ! for Split.
  PURE SUBROUTINE split_CH_CH_CH(string, word, set, separator, back)

    !---------------------------------------------------------------------------
    ! Arguments

    !> On input, the string to split.  On output, this is the 
    !! remainder of the string, excluding the separator character.
    CHARACTER(LEN=:, KIND=ck), INTENT(INOUT), ALLOCATABLE :: string

    !> The fragment of the input @a string prior to encountering the 
    !! separator.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE :: word

    !> The set of separator characters.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: set

    !> The separator encountered in @a string, or zero length if no 
    !! separator was found.
    CHARACTER(LEN=:, KIND=ck), INTENT(OUT), ALLOCATABLE, OPTIONAL :: separator

    !> Nominates the direction of search.  If not present or false, then 
    !! the search for the separator commences from the start of @a string, 
    !! if present and true then the search commences from the end of 
    !! @a string.
    LOGICAL, INTENT(IN), OPTIONAL :: back

    !---------------------------------------------------------------------------
    ! Locals

    LOGICAL :: back_          ! Defaulted variant of @a back.
    INTEGER :: i_separator    ! Index of the separator.

    !***************************************************************************

    IF (PRESENT(back)) THEN
       back_ = back
    ELSE
       back_ = .false.
    END IF

    i_separator = SCAN(string, set, back_)

    IF (i_separator /= 0) THEN

       IF (back_) THEN
          word = string(i_separator+1:)
          IF (PRESENT(separator)) THEN
             separator = string(i_separator:i_separator)
          END IF
          string = string(:i_separator-1)
       ELSE
          word = string(:i_separator-1)
          IF (PRESENT(separator)) THEN
             separator = string(i_separator:i_separator)
          END IF
          string = string(i_separator+1:)
       END IF

    ELSE

       word = string
       IF (PRESENT(separator)) separator = ""
       string = ""

    END IF

  END SUBROUTINE split_CH_CH_CH


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedures for unformatted defined input/output support.


  !*****************************************************************************
  !!
  !> Specific procedure for defined io (unformatted output) of a varying_string.

  SUBROUTINE write_unformatted(dtv, unit, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being written.
    CLASS(varying_string), INTENT(IN) :: dtv

    !> The unit being read.
    INTEGER, INTENT(IN) :: unit

    !> IOSTAT error code - positive on error, IOSTAT_END for end of 
    !! file, IOSTAT_EOR for end of record, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !***************************************************************************

    IF (dbg_flag .AND. .NOT. ALLOCATED(dtv%chars)) THEN
       STOP 'Output of an undefined varying_string.'
    END IF

    WRITE (unit, IOSTAT=iostat, IOMSG=iomsg) LEN(dtv%chars)
    IF (iostat /= 0) RETURN

    WRITE (unit, IOSTAT=iostat, IOMSG=iomsg) dtv%chars

  END SUBROUTINE write_unformatted


  !*****************************************************************************
  !!
  !> Specific procedure for defined io (unformatted input) of a varying_string.

  SUBROUTINE read_unformatted(dtv, unit, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being read.  
    !! 
    !! This does not have to have been previously given a value.
    CLASS(varying_string), INTENT(INOUT) :: dtv

    !> The unit being read.
    INTEGER, INTENT(IN) :: unit

    !> IOSTAT error code - positive on error, IOSTAT_END for end of 
    !! file, IOSTAT_EOR for end of record, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: the_len        ! Length of the string data.

    !***************************************************************************

    READ (unit, IOSTAT=iostat, IOMSG=iomsg) the_len
    IF (iostat /= 0) RETURN

    ! If the existing allocation is the right length, then we retain 
    ! the existing allocation.
    IF (ALLOCATED(dtv%chars)) THEN
       IF (LEN(dtv%chars) /= the_len) THEN
          DEALLOCATE(dtv%chars)
       END IF
    END IF

    IF (.NOT. ALLOCATED(dtv%chars)) THEN
       ALLOCATE(CHARACTER(LEN=the_len, KIND=ck) :: dtv%chars)
    END IF

    READ (unit, IOSTAT=iostat, IOMSG=iomsg) dtv%chars

  END SUBROUTINE read_unformatted


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedure and supporting procedures for formatted defined 
  ! output support.


  !*****************************************************************************
  !!
  !> Specific procedure for defined io (formatted output) of a varying_string.
  !!
  !! For list directed output to external files, the value is delimited 
  !! based on the DELIM changeable connection mode.  For internal files 
  !! the string is always undelimited.  Blanks may be added before and 
  !! after the value.
  !!
  !! For namelist output, the value is always delimited.  For external 
  !! files where the DELIM changeable connection mode is APOSTROPHE or 
  !! QUOTE, then that form of delimiter is used, in all other cases double 
  !! quotes are used.
  !!
  !! For DT output the value is never delimited.  The iotype and v_list 
  !! values are currently not significant.
  !!
  !! In all cases, we do not handle the situation where the size of record 
  !! remaining is insufficient to hold the value representation.

  SUBROUTINE write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being written.
    CLASS(varying_string), INTENT(IN) :: dtv

    !> The unit being read.
    INTEGER, INTENT(IN) :: unit

    !> The type of io/edit descriptor.
    !!
    !! - 'LISTDIRECTED'
    !! - 'NAMELIST'
    !! - 'DTxxx'
    CHARACTER(*), INTENT(IN) :: iotype

    !> The v-list of the edit descriptor.
    INTEGER, INTENT(IN) :: v_list(:)

    !> IOSTAT error code - positive on error, IOSTAT_END for end of 
    !! file, IOSTAT_EOR for end of record, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! The delimiter character (apostrophe or quote) to use for output, 
    ! or blank if the string is not to be delimited.
    CHARACTER(KIND=ck) :: delim

    !***************************************************************************

    IF (dbg_flag .AND. .NOT. ALLOCATED(dtv%chars)) THEN
       STOP 'Output of an undefined varying_string.'
    END IF

    IF (iotype == 'LISTDIRECTED') THEN
       CALL get_delim_mode(unit, delim, iostat, iomsg)

       ! List directed formatting typically gives lots of freedom to the 
       ! processor on output.  However, there is a requirement that each 
       ! record (that is not a continuation record) starts with a leading 
       ! blank.  ifort doesn't do that for us (not sure if it should), so 
       ! we explicitly stick one in here.
       WRITE (unit, "(1X)")
       CALL write_formatted_worker(dtv, unit, delim, iostat, iomsg)

       ! There must be a separating blank prior to the next item in the 
       ! record, otherwise we have no way of knowing where the string 
       ! ends.  We don't need a separating blank if the varying_string 
       ! is the last item in the record.  Using 1X should achieves this 
       ! goal, but there's nothing to stop the processor adding its own 
       ! additional blanks.
       WRITE (unit, "(1X)")
    ELSE IF (iotype == 'NAMELIST') THEN
       CALL get_delim_mode(unit, delim, iostat, iomsg)
       ! We always delimit.
       IF (delim == '') delim = '"'
       CALL write_formatted_worker(dtv, unit, delim, iostat, iomsg)
    ELSE
       IF (LEN(iotype) > 2) THEN
          iostat = 2
          iomsg = 'The character literal after the DT edit descriptor &
               &for a varying_string must not be present.'
          RETURN
       END IF
       IF (SIZE(v_list) /= 0) THEN
          iostat = 2
          iomsg = 'The list of integers after the DT edit descriptor &
               &for a varying_string must not be present.'
          RETURN
       END IF

       ! We assume the user will apply the necessary delimiters.
       CALL write_formatted_worker(dtv, unit, '', iostat, iomsg)
    END IF

  END SUBROUTINE write_formatted


  !*****************************************************************************
  !!
  !> Write a possibly delimited formatted representation of the value of 
  !! a string.

  SUBROUTINE write_formatted_worker(dtv, unit, delim, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varyingstring to be written.
    TYPE(varying_string), INTENT(IN) :: dtv

    !> The unit to write to.  This must be connected for formatted output.
    INTEGER, INTENT(IN) :: unit

    !> The delimiter to use for the value representation.
    !!
    !! The length of this should not be greater than one.
    !!
    !! If non-blank, then any characters in the value of the string that match 
    !! this value will be doubled in the formatted representation.
    CHARACTER(LEN=*, KIND=ck), INTENT(IN) :: delim

    !> IOSTAT code - IOSTAT_END on end of file, IOSTAT_EOR on end of record, 
    !! positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG error message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: i              ! Index into @a dtv%chars.

    !***************************************************************************

    IF (delim /= '') THEN
       WRITE (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) delim
       IF (iostat /= 0) RETURN
    END IF

    DO i = 1, LEN(dtv%chars)
       IF (delim /= '' .AND. dtv%chars(i:i) == delim) THEN
          WRITE (unit, "(A,A)", IOSTAT=iostat, IOMSG=iomsg) delim, delim
       ELSE
          WRITE (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) dtv%chars(i:i)
       END IF
       IF (iostat /= 0) RETURN
    END DO

    IF (delim /= '') THEN
       WRITE (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) delim
       IF (iostat /= 0) RETURN
    END IF

  END SUBROUTINE write_formatted_worker


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedure and supporting procedures for formatted defined 
  ! input support.


  !*****************************************************************************
  !!
  !> Specific procedure for defined io (formatted input) of a varying_string.
  !!
  !! For list directed and namelist reads we approximate the requirements 
  !! of list directed and namelist reads of character data, that is:
  !!
  !! For list directed input:
  !! - the value may be unquoted or quoted with either apostrophes or 
  !!   double quotes.  
  !! - In either case blanks before the first non-blank character (before 
  !!   the opening quote character if the string is quoted) are ignored.  
  !!   Additional records may be read until the first non-blank character 
  !!   is encountered.
  !! - Inside quoted values double the quotes without an intervening record 
  !!   break to represent the quote character in the value, otherwise all 
  !!   characters are considered part of the value and record breaks are 
  !!   not significant in any way.
  !! - Unquoted values are terminated by the list directed value separator 
  !!   encountered (blank after non-blank, slash, comma or semicolon 
  !!   (depending on DECIMAL) or end of record.  If a separator is 
  !!   encountered without encountering any other non-blank characters, 
  !!   then the input is considered a null value and the definition 
  !!   status of the item remains unchanged.
  !!
  !! For namelist input:
  !! - The value must be quoted with either apostrophes or double quotes, 
  !!   and the same rules for list directed input of a quoted value are then 
  !!   followed.
  !!
  !! For explicit DT input with nothing provided in the optional 
  !! character literal of the edit descriptor:
  !! - The v_list values are not signficant.
  !! - The value may be unquoted or quoted with either apostrophes or 
  !!   double quotes.
  !! - Blanks before the first non-blank character (before the opening 
  !!   quote character if the string is quoted) or end of record are 
  !!   not signficant.
  !! - Inside quoted values double the quotes without an intervening record 
  !!   break to represent the quote character in the value, otherwise all 
  !!   characters are considered part of the value and record breaks are 
  !!   not significant in any way.
  !! - Unquoted values are terminated by the list directed value separator 
  !!   encountered (blank after non-blank, slash, comma or semicolon 
  !!   (depending on DECIMAL) or end of record.  If a separator is 
  !!   encountered without encountering any other non-blank characters, 
  !!   then the resulting value is a zero length string.  If end of 
  !!   record is encountered without encountering any other non-blank 
  !!   characters, then end of record is returned.
  !!
  !! The following comma or semicolon separated list of modifiers can be 
  !! provided in the character literal of the DT edit descriptor:
  !! - SKIPBLANK:   Leading blank characters before the first non-blank 
  !!                character are skipped before determining whether the 
  !!                input is delimited or not.  NOSKIPBLANK must not 
  !!                be provided.  This modifier is assumed by default if 
  !!                any other modifier, apart from FIXED or NOSKIPBLANK is 
  !!                present.  If the end of record is encountered before 
  !!                any non-blank character, then an end-fo-record 
  !!                condition results.  If the NODELIMITED modifier is not 
  !!                provided and if the initial character is a quote or 
  !!                apostrophe, the input is treated as delimited as 
  !!                discussed above, otherwise the input is treated as 
  !!                undelimited, with the conditions and characters that 
  !!                terminate input determined by the other modifiers.
  !! - NOSKIPBLANK: Leading blank characters before the first non-blank 
  !!                character are not skipped before determining whether 
  !!                the input is a delimited or not.  If the first 
  !!                character read is a blank then the input is considered 
  !!                undelimited, in which case the leading blanks appear 
  !!                in the resulting value.  SKIPBLANK must not be provided.
  !! - EOR:         If the input is undelimited, input will be terminated 
  !!                by the end of record.  This is assumed by default 
  !!                if any other modifier, apart from FIXED, is present.
  !! - BLANK:       In the absence of quoting, input will be terminated 
  !!                by the next blank encountered.
  !! - SLASH:       In the absence of quoting, input will be terminated 
  !!                by the next `/` encountered.
  !! - NODELIMITED: Quoting is ignored - it is always considered absent, 
  !!                and any quote characters are considered part of the 
  !!                value.
  !! - COMMA:       In the absence of quoting, input will be terminated 
  !!                by the next `,` encountered.
  !! - SEMICOLON:   In the absence of quoting, input will be terminated 
  !!                by the next `;` encountered.
  !! - NON_DECIMAL: In the absence of quoting, input will be terminated 
  !!                by whatever is the alternative character to that 
  !!                specified by the current DECIMAL mode.
  !! - DELIM(str):  `str` is a character literal, in the usual form of 
  !!                such a literal embedded in a format specification.  
  !!                In the absence of quoting, input will be terminated 
  !!                by the end of record or by the appearance of any 
  !!                character from the set nominated by `str`.
  !! - FIXED(n):    `n` is an unsigned integer literal without a kind 
  !!                specifier.  `n` characters will be read.  No other 
  !!                keywords may be provided.
  !!
  !! Where input is terminated by a specific character, that character 
  !! will be the next character read in the file.  For modifiers other than 
  !! FIXED, if input is terminated by an end-of-record condition, then 
  !! an end-of-record condition results.  For the FIXED modifier, if there 
  !! are less than `n` characters remaining in the record, the varying 
  !! string object is defined with the characters in the record and an 
  !! end-of-record condition results.
  !! 
  !! Modifier keywords are not case sensitive, blanks may be used freely 
  !! outside of modifier keywords, integer literals and character literals, 
  !! modifiers cannot appear more than once.
  !!
  !! Note that due to limitations associated with Fortran's IO model, 
  !! the changeable connection modes for connections to internal files are 
  !! always treated as being at their default values, regardless of any 
  !! specifiers in the READ statement or any control edit descriptors that 
  !! may dictate otherwise.

  SUBROUTINE read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being read.  
    !! 
    !! This does not have to have been previously given a value.
    CLASS(varying_string), INTENT(INOUT) :: dtv

    !> The unit being read.
    INTEGER, INTENT(IN) :: unit

    !> The type of io/edit descriptor.
    !!
    !! - 'LISTDIRECTED'
    !! - 'NAMELIST'
    !! - 'DTxxx'
    CHARACTER(*), INTENT(IN) :: iotype

    !> The v-list of the edit descriptor.
    INTEGER, INTENT(IN) :: v_list(:)

    !> IOSTAT error code - positive on error, IOSTAT_END for end of 
    !! file, IOSTAT_EOR for end of record, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Local variant of iomsg, so it doesn't get inappropriately redefined.
    !
    ! Historically we suppressed IOSTAT_EOR and the associated redefinition 
    ! of IOMSG in the worker procedures called from this procedure.  However, 
    ! we kept changing our mind about how things work, so now most IOSTAT_EOR 
    ! supression logic is in this procedure.
    CHARACTER(LEN=LEN(iomsg)) :: local_iomsg

    ! Delimiter character
    CHARACTER(KIND=ck) :: delim

    !***************************************************************************

    ! All branches of the following if construct RETURN.
    IF (iotype == 'LISTDIRECTED') THEN
       CALL get_next_non_blank_any_record(unit, delim, iostat, iomsg)
       IF (iostat /= 0) RETURN

       IF (delim == '"' .OR. delim == "'") THEN
          ! We have a delimited string.  
          CALL read_delimited(dtv, unit, delim, iostat, local_iomsg)
       ELSE
          ! Step back before the non-blank.
          READ (unit, "(TL1)", IOSTAT=iostat, IOMSG=iomsg)
          IF (iostat /= 0) RETURN
          ! Read undelimited.  Note that up until this point we have not 
          ! changed the definition status of @a dtv - the following call 
          ! may similarly not define dtv if no value characters 
          ! are encountered 
          CALL read_undelimited_listdirected(dtv, unit, iostat, local_iomsg)
       END IF
       ! We suppress IOSTAT_EOR.
       IF (IS_IOSTAT_EOR(iostat)) THEN
          iostat = 0
       ELSE IF (iostat /= 0) THEN
          iomsg = local_iomsg
       END IF
       RETURN
    ELSE IF (iotype == 'NAMELIST') THEN
       ! Name list string input must be delimited, but apart from that 
       ! the rules are as for list directed.
       CALL get_next_non_blank_any_record(unit, delim, iostat, iomsg)
       IF (iostat /= 0) RETURN

       IF (delim == '"' .OR. delim == "'") THEN
          ! We have a delimited string.  
          CALL read_delimited(dtv, unit, delim, iostat, local_iomsg)
          ! We suppress IOSTAT_EOR.
          IF (IS_IOSTAT_EOR(iostat)) THEN
             iostat = 0
          ELSE IF (iostat /= 0) THEN
             iomsg = local_iomsg
          END IF
          RETURN
       ELSE
          ! We require delimited strings for namelist input.
          iostat = 1
          iomsg = 'A single or double quote character was expected.'
          RETURN
       END IF
    ELSE ! DTxxxx
       IF (LEN(iotype) > LEN('DT')) THEN
          ! The rules depend on the character literal.  This includes what 
          ! happens regarding EOR.
          CALL read_dt_with_literal(  &
               dtv,  &
               unit,  &
               iotype(3:),  &
               v_list,  &
               iostat,  &
               iomsg )
          RETURN
       ELSE
          ! Like list directed, but we always stay within the current record.  
          !
          ! If we get end of record without a non-blank character, then we 
          ! return end of record and an empty string.
          CALL get_next_non_blank_this_record(unit, delim, iostat, iomsg)
          IF (IS_IOSTAT_EOR(iostat)) THEN
             dtv%chars = ''
             RETURN
          END IF
          IF (iostat /= 0) RETURN

          IF (delim == '"' .OR. delim == "'") THEN
             ! We have a delimited string.  We suppress IOSTAT_EOR below.
             CALL read_delimited(dtv, unit, delim, iostat, local_iomsg)
          ELSE
             ! Unlike list directed, if we encounter no non-blank characters 
             ! prior to the thing that terminates input, the result here 
             ! is a zero length string (for list directed the definition 
             ! of the item is not changed).
             dtv%chars = ''
             ! Step back before the non-blank.
             READ (unit, "(TL1)", IOSTAT=iostat, IOMSG=iomsg)
             IF (iostat /= 0) RETURN
             ! Read undelimited, just like list directed input.  We also 
             ! suppress IOSTAT_EOR below.
             CALL read_undelimited_listdirected(dtv, unit, iostat, local_iomsg)
          END IF
          ! We suppress IOSTAT_EOR for DT input, as long as we got at least one 
          ! non-blank character
          IF (IS_IOSTAT_EOR(iostat)) THEN
             iostat = 0
          ELSE IF (iostat /= 0) THEN
             iomsg = local_iomsg
          END IF
          RETURN
       END IF
    END IF

    ! Should never be here.

  END SUBROUTINE read_formatted


  !*****************************************************************************
  !!
  !> Read an undelimited (no leading apostrophe or double quote) character 
  !! value according to the rules for list directed input.
  !!
  !! A blank, comma/semicolon (depending on the decimal mode), slash or 
  !! end of record terminates the string.
  !!
  !! If input is terminated by end of record, then this procedure 
  !! returns an end-of-record condition.

  SUBROUTINE read_undelimited_listdirected(dtv, unit, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being read.  This is only redefined if a non-blank, 
    !! non-separator character is read from the unit.
    TYPE(varying_string), INTENT(INOUT) :: dtv

    !> The unit to read from.  This must be connected for formatted input.
    INTEGER, INTENT(IN) :: unit

    !> IOSTAT code - IOSTAT_END on end of file, IOSTAT_EOR on end-of-record, 
    !! positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG error message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! True if DECIMAL=POINT in effect.
    LOGICAL :: decimal_point

    !***************************************************************************

    ! Get the relevant changeable modes.
    CALL get_decimal_mode(unit, decimal_point, iostat, iomsg)
    IF (iostat /= 0) RETURN

    CALL read_undelimited(  &
         dtv,  &
         unit,  &
         ' ' // '/' // MERGE(ck_',', ck_';', decimal_point),  &
         iostat,  &
         iomsg )

  END SUBROUTINE read_undelimited_listdirected


  !*****************************************************************************
  !!
  !> Read a delimited string from a unit connected for formatted input.
  !!
  !! If the closing delimiter is followed by end of record, then we 
  !! return end of record.

  SUBROUTINE read_delimited(dtv, unit, delim, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being read.
    TYPE(varying_string), INTENT(OUT) :: dtv

    !> The unit to read from.  This must be connected for formatted input.  
    !! The current file position should be immediately after the opening 
    !! delimiter.
    INTEGER, INTENT(IN) :: unit

    !> The delimiter character.
    CHARACTER(KIND=ck), INTENT(IN) :: delim

    !> IOSTAT code - IOSTAT_END on end of file, IOSTAT_EOR on end-of-record, 
    !! positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG error message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Character read from record.
    CHARACTER(KIND=ck) :: ch

    ! Indicates that the last character read was a delimiter.
    LOGICAL :: was_delim

    !***************************************************************************

    was_delim = .FALSE.
    dtv%chars = ''

    DO
       READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) ch
       IF (IS_IOSTAT_EOR(iostat)) THEN
          IF (was_delim) THEN
             ! End of delimited string followed by end of record is end of 
             ! the string.  We pass back the end of record condition to the 
             ! caller.
             RETURN
          ELSE
             ! End of record without terminating delimiter - move along.
             CYCLE
          END IF
       ELSE IF (iostat /= 0) THEN
          RETURN
       END IF

       IF (ch == delim) THEN
          IF (was_delim) THEN
             ! Doubled delimiter is one delimiter in the value.
             dtv%chars = dtv%chars // ch
             was_delim = .FALSE.
          ELSE
             ! Need to test next character to see what is happening.
             was_delim = .TRUE.
          END IF
       ELSE IF (was_delim) THEN
          ! The previous character was actually the delimiter for the 
          ! end of the string.  Put back the current character.
          READ (unit, "(TL1)", IOSTAT=iostat, IOMSG=iomsg)
          RETURN
       ELSE
          dtv%chars = dtv%chars // ch
       END IF
    END DO

  END SUBROUTINE read_delimited


  !*****************************************************************************
  !!
  !> Handle explicit (DT) formatted input with something provided in the 
  !! character literal of the edit descriptor.
  !!
  !! An IOSTAT of IOSTAT_EOR is only returned as specified by the 
  !! behaviour of the modifiers.

  SUBROUTINE read_dt_with_literal(dtv, unit, literal, v_list, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being read.
    CLASS(varying_string), INTENT(OUT) :: dtv

    !> The unit being read.
    INTEGER, INTENT(IN) :: unit

    !> The value of the character literal provided afer the DT edit 
    !! descriptor.
    CHARACTER(*), INTENT(IN) :: literal

    !> The v-list of the edit descriptor.
    !!
    !! This is not currently used for any modifier, and must be zero 
    !! size.
    INTEGER, INTENT(IN) :: v_list(:)

    !> IOSTAT error code - positive on error, IOSTAT_END for end of 
    !! file, IOSTAT_EOR for end of record, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! The results of parsing the literal.
    TYPE(literal_parse_results) :: parse_results

    ! The set of terminator characters.
    CHARACTER(:,KIND=ck), ALLOCATABLE :: terminators

    LOGICAL :: decimal_point  ! .TRUE. if the decimal mode is POINT

    ! Single character read from file that might be a delimiter.
    CHARACTER(KIND=ck) :: delim

    INTEGER :: chars_read     ! Number of characters successfully read.

    ! Local variant of iomsg, so it doesn't get inappropriately redefined.
    CHARACTER(LEN(iomsg)) :: local_iomsg

    !***************************************************************************

    IF (SIZE(v_list) /= 0) THEN
       iostat = 2
       iomsg = 'The list of integers after the DT edit descriptor &
            &for a varying_string must not be present.'
       RETURN
    END IF

    CALL parse_literal(literal, parse_results, iostat, iomsg)
    IF (iostat /= 0) RETURN

    ! Validate that the parsed representation makes sense.
    IF (ALLOCATED(parse_results%fixed)) THEN
       IF ( (parse_results%skipblank /= 0)  &
            .OR. parse_results%seen_eor  &
            .OR. parse_results%seen_blank  &
            .OR. parse_results%seen_slash  &
            .OR. parse_results%seen_nodelimited  &
            .OR. parse_results%seen_comma  &
            .OR. parse_results%seen_semicolon  &
            .OR. parse_results%seen_nondecimal ) THEN
          iostat = 2
          iomsg = 'Inconsistent modifiers for DT for varying_string.'
          RETURN
       END IF
    END IF

    ! Do the read!
    IF (ALLOCATED(parse_results%fixed)) THEN
       ALLOCATE(  &
            CHARACTER(parse_results%fixed) :: dtv%chars,  &
            STAT=iostat,  &
            ERRMSG=iomsg )
       IF (iostat /= 0) RETURN
       READ (  &
            UNIT=unit,  &
            FMT="(A)",  &
            IOSTAT=iostat,  &
            IOMSG=iomsg,  &
            SIZE=chars_read,  &
            ADVANCE='NO' )  &
            dtv%chars
       IF (IS_IOSTAT_EOR(iostat)) THEN
          ! Truncate back to what was actually read.
          dtv%chars = dtv%chars(:chars_read)
       ELSE
          ! end-of-file or other error - dtv%chars undefined here.
       END IF
       RETURN
    ELSE
       ! Get the first significant character from the record.  If we are not 
       ! skipping blanks that is the first character, otherwise it is 
       ! the first non-blank character in the current record.
       IF (parse_results%skipblank == 2) THEN
          ! We are not skipping leading blanks.
          READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) delim
          ! Not getting a single character is an error or condition.
          IF (iostat /= 0) RETURN
       ELSE
          CALL get_next_non_blank_this_record(unit, delim, iostat, iomsg)
          ! If the result of skipping all those leading blanks is the 
          ! end of record, then a zero length string and an end-of-record 
          ! condition results.
          IF (iostat /= 0) RETURN
       END IF

       ! If we permit delimited input, then see if we have  
       ! delimited input.
       IF (.NOT. parse_results%seen_nodelimited) THEN
          ! See if the first character looks like a delimiter.
          IF (delim == ck_'"' .OR. delim == "'") THEN
             CALL read_delimited(dtv, unit, delim, iostat, iomsg)
             RETURN
          END IF
       END IF

       ! Here if we are processing undelimited input.

       ! Prepend the single character already read to the result string.
       dtv%chars = delim

       ! We are not dealing with a quoted string, or quoting is being 
       ! suppressed.  Assemble the list of terminator characters.
       terminators = ''
       IF (parse_results%seen_blank) terminators = terminators // ck_' '
       IF (parse_results%seen_slash) terminators = terminators // ck_'/'
       IF (parse_results%seen_comma) terminators = terminators // ck_','
       IF (parse_results%seen_semicolon) terminators = terminators // ck_';'
       IF (parse_results%seen_nondecimal) THEN
          CALL get_decimal_mode(unit, decimal_point, iostat, iomsg)
          IF (iostat /= 0) RETURN
          IF (decimal_point) THEN
             terminators = terminators // ck_','
          ELSE 
             terminators = terminators // ck_';'
          END IF
       END IF
       CALL read_undelimited(dtv, unit, terminators, iostat, local_iomsg)
       ! Because we have definitely obtained one character of the value 
       ! successfully, we suppress IOSTAT_EOR.
       IF (IS_IOSTAT_EOR(iostat)) THEN
          iostat = 0
       ELSE IF (iostat /= 0) THEN
          iomsg = local_iomsg
       END IF
       RETURN
    END IF

    ! Shouldn't make it here.

  END SUBROUTINE read_dt_with_literal


  !*****************************************************************************
  !!
  !> Read an undelimited string up until end of record or a character from 
  !! a set of terminators is encountered.
  !!
  !! If a terminator is encountered, the file position will be at that 
  !! terminating character.  If end of record is encountered, the 
  !! file remains at end of record.

  SUBROUTINE read_undelimited(dtv, unit, terminators, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The varying_string being read.  Any characters that have already 
    !! been read from the file should be prepended to the internal character 
    !! array.
    TYPE(varying_string), INTENT(INOUT) :: dtv

    !> The unit to read from.  This must be connected for formatted input.
    INTEGER, INTENT(IN) :: unit

    !> Characters that are considered to terminate the string.  Blanks in 
    !! this string are meaningful.
    CHARACTER(*,KIND=ck), INTENT(IN) :: terminators

    !> IOSTAT code - IOSTAT_END on end of file, IOSTAT_EOR on end of record, 
    !! positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG error message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Character read from record.
    CHARACTER(KIND=ck) :: ch

    !***************************************************************************

    ! Loop until we stop having fun.
    DO
       READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) ch
       IF (IS_IOSTAT_EOR(iostat)) THEN
          ! End of record just means end of string.  We pass on the condition.
          RETURN
       ELSE IF (iostat /= 0) THEN
          ! Something odd happened.
          RETURN
       END IF

       IF (SCAN(ch, terminators) /= 0) THEN
          ! Change the file position so that the next read sees the terminator.
          READ (unit, "(TL1)", IOSTAT=iostat, IOMSG=iomsg)
          IF (iostat /= 0) RETURN
          iostat = 0
          RETURN
       END IF
       ! We got a character - append it.
       dtv%chars = dtv%chars // ch
    END DO

  END SUBROUTINE read_undelimited


  !*****************************************************************************
  !!
  !> Parse the literal supplied with a DT edit descriptor.

  SUBROUTINE parse_literal(literal, results, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The literal supplied after the DT edit descriptor.
    CHARACTER(*), INTENT(IN) :: literal

    !> The results of a successful parse.  Not valid if @a iostat is 
    !! non-zero.
    TYPE(literal_parse_results), INTENT(OUT) :: results

    !> IOSTAT error code - positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: start          ! Start position of current modifier.
    INTEGER :: pos            ! Current position in the literal.

    !***************************************************************************

    ! We could consider writing modifiers for varying_string that would let us 
    ! parse the DT literal using varying_string defined input with keywords 
    ! in the character literal on an interal unit!  In the meantime...
    pos = 1

    DO WHILE (pos <= LEN(literal))
       CALL skip_ws(literal, pos)
       IF (pos > LEN(literal)) EXIT

       ! Remember the start of the sequence of non-blanks.
       start = pos

       ! Find the end of the modifier keyword.
       CALL skip_keyword_chars(literal, pos)

       ! Interpret the modifier keyword.
       SELECT CASE (upper_case(literal(start:pos-1)))
       CASE ('SKIPBLANK')
          IF (results%skipblank == 1) THEN
             iostat = 2
             iomsg = 'SKIPBLANK modifier repeated after the DT edit descriptor'
             RETURN
          ELSE IF (results%skipblank == 2) THEN
             iostat = 2
             iomsg = 'SKIPBLANK and NOSKIPBLANK not permitted together &
                  &after the DT edit descriptor for a varying_string'
             RETURN
          END IF
          results%skipblank = 1

       CASE ('NOSKIPBLANK')
          IF (results%skipblank == 2) THEN
             iostat = 2
             iomsg = 'NOSKIPBLANK modifier repeated after the DT edit descriptor'
             RETURN
          ELSE IF (results%skipblank == 1) THEN
             iostat = 2
             iomsg = 'SKIPBLANK and NOSKIPBLANK not permitted together &
                  &after the DT edit descriptor for a varying_string'
             RETURN
          END IF
          results%skipblank = 2

       CASE ('EOR')
          IF (results%seen_eor) THEN
             iostat = 2
             iomsg = 'EOR modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_eor = .TRUE.

       CASE ('BLANK')
          IF (results%seen_blank) THEN
             iostat = 2
             iomsg = 'BLANK modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_blank = .TRUE.

       CASE ('SLASH')
          IF (results%seen_slash) THEN
             iostat = 2
             iomsg = 'SLASH modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_slash = .TRUE.

       CASE ('NODELIMITED')
          IF (results%seen_nodelimited) THEN
             iostat = 2
             iomsg = 'NODELIMITED modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_nodelimited = .TRUE.

       CASE ('COMMA')
          IF (results%seen_comma) THEN
             iostat = 2
             iomsg = 'COMMA modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_comma = .TRUE.

       CASE ('SEMICOLON')
          IF (results%seen_semicolon) THEN
             iostat = 2
             iomsg = 'SEMICOLON modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_semicolon = .TRUE.

       CASE ('NONDECIMAL')
          IF (results%seen_nondecimal) THEN
             iostat = 2
             iomsg = 'NONDECIMAL modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          results%seen_nondecimal = .TRUE.

       CASE ('DELIM')
          IF (ALLOCATED(results%delim)) THEN
             iostat = 2
             iomsg = 'DELIM modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          CALL parse_delim(literal, pos, results%delim, iostat, iomsg)
          IF (iostat /= 0) RETURN

       CASE ('FIXED')
          IF (ALLOCATED(results%fixed)) THEN
             iostat = 2
             iomsg = 'FIXED modifier repeated after the DT edit descriptor'
             RETURN
          END IF
          ALLOCATE(results%fixed)
          CALL parse_fixed(literal, pos, results%fixed, iostat, iomsg)
          IF (iostat /= 0) RETURN

       CASE DEFAULT
          iostat = 2
          iomsg = 'Unrecognised modifier ' // upper_case(literal(start:pos-1))  &
               // ' after the DT edit descriptor'
          RETURN
       END SELECT

       CALL skip_ws(literal, pos)
       IF (pos > LEN(literal)) EXIT

       ! Perhaps a separating comma or semicolon?
       IF ((literal(pos:pos) == ',') .OR. (literal(pos:pos) == ';')) THEN
          ! There must be a following keyword.
          pos = pos + 1
          CALL skip_ws(literal, pos)
          IF (pos > LEN(literal)) THEN
             iostat = 2
             iomsg = 'Trailing comma at the end of the DT literal for &
                  &a varying_string'
             RETURN
          END IF
       ELSE
          ! Skip over the separator.
          pos = pos + 1
       END IF
    END DO

  END SUBROUTINE parse_literal


  !*****************************************************************************
  !!
  !> Skip over whitespace in a character string.
  !!
  !! This is robust to @a pos being greater than LEN(@a str) when 
  !! invoked.

  SUBROUTINE skip_ws(str, pos)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The character string being walked.
    CHARACTER(*), INTENT(IN) :: str

    !> The current position in @a str, or any value greater than the length 
    !! of @ str.  This is advanced until it exceeds the length of @a str, 
    !! or that position in @a str is not a blank.
    INTEGER, INTENT(INOUT) :: pos

    !***************************************************************************

    DO WHILE (pos <= LEN(str))
       IF (str(pos:pos) /= '') EXIT
       pos = pos + 1
    END DO

  END SUBROUTINE skip_ws


  !*****************************************************************************
  !!
  !> Skip over characters that could be in a keyword.

  SUBROUTINE skip_keyword_chars(str, pos)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The character string being walked.
    CHARACTER(*), INTENT(IN) :: str

    !> The current position in @a str.  This is advanced until it exceeds the 
    !! length of @a str, or that position in @a str is not an alphanumeric.
    INTEGER, INTENT(INOUT) :: pos

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: p              ! Position in remaining substring of @a str.

    !***************************************************************************

    p = VERIFY( &
         str(pos:),  &
         'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789' )
    IF (p == 0) THEN
       pos = LEN(str) + 1
    ELSE
       pos = pos + p - 1
    END IF

  END SUBROUTINE skip_keyword_chars


  !*****************************************************************************
  !!
  !> Upper case letter characters in a string from the Fortran character 
  !! set.

  PURE FUNCTION upper_case(str) RESULT(ucase)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The string to be upper cased.  
    CHARACTER(*), INTENT(IN) :: str

    !> @returns An upper cased copy of @a str.  We ignore any characters that 
    !! are not letter characters (F2008 3.1.2).
    CHARACTER(LEN(str)) :: ucase

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: pos            ! Position in @a str.

    !***************************************************************************

    DO pos = 1, LEN(str)
       ! We rely on the ASCII collation sequence.
       IF ( (IACHAR(str(pos:pos)) >= IACHAR('a'))  &
            .AND. (IACHAR(str(pos:pos)) <= IACHAR('z')) ) THEN
          ucase(pos:pos) = ACHAR(IACHAR(str(pos:pos)) - IACHAR('a') + IACHAR('A'))
       ELSE
          ucase(pos:pos) = str(pos:pos)
       END IF
    END DO

  END FUNCTION upper_case


  !*****************************************************************************
  !!
  !> Parse the parenthesised argument list of the DELIM keyword.

  SUBROUTINE parse_delim(literal, pos, delim, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The literal supplied after the DT edit descriptor.
    CHARACTER(*), INTENT(IN) :: literal

    !> The current position in the literal.  This should be the first 
    !! character after DELIM.  If @a iostat is non-zero, this will 
    !! be updated to the first position after the end of the closing 
    !! parenthesis.
    INTEGER, INTENT(INOUT) :: pos

    !> The set of delimiters.
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: delim

    !> IOSTAT error code - positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !***************************************************************************

    ! Next non-blank must be opening parenthesis.
    CALL skip_ws(literal, pos)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Missing `(` after DELIM'
       RETURN
    END IF
    IF (literal(pos:pos) /= '(') THEN
       iostat = 2
       iomsg = 'Missing `(` after DELIM'
       RETURN
    END IF
    ! Skip the `(`
    pos = pos + 1

    ! Then there must be a quote character.
    CALL skip_ws(literal, pos)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Missing quoted string argument for DELIM'
       RETURN
    END IF
    IF ((literal(pos:pos) /= '"') .AND. (literal(pos:pos) /= "'")) THEN
       iostat = 2
       iomsg = 'Missing quoted string argument for DELIM'
       RETURN
    END IF
    ! Get the quoted string.  literal(pos:pos) is to indicate 
    ! the type of quoting.
    CALL get_quoted_string(literal, pos, delim, iostat, iomsg)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Mismatched quotes for argument for DELIM'
       RETURN
    END IF

    ! Closing parenthesis.
    CALL skip_ws(literal, pos)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Missing `)` after DELIM'
       RETURN
    END IF
    IF (literal(pos:pos) /= ')') THEN
       iostat = 2
       iomsg = 'Missing `)` after DELIM'
       RETURN
    END IF
    ! Skip the `)`
    pos = pos + 1

    iostat = 0

  END SUBROUTINE parse_delim


  !*****************************************************************************
  !!
  !> Parse the parenthesised argument list of the FIXED keyword.

  SUBROUTINE parse_fixed(literal, pos, fixed, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The literal supplied after the DT edit descriptor.
    CHARACTER(*), INTENT(IN) :: literal

    !> The current position in the literal.  This should be the first 
    !! character after DELIM.  If @a iostat is non-zero, this will 
    !! be updated to the first position after the end of the closing 
    !! parenthesis.
    INTEGER, INTENT(INOUT) :: pos

    !> The fixed width.
    INTEGER, INTENT(OUT) :: fixed

    !> IOSTAT error code - positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    INTEGER :: start          ! Start of the integer literal for the width.
    CHARACTER(10) :: fmt      ! Format spec for reading the width.

    !***************************************************************************

    ! Next non-blank must be opening parenthesis.
    CALL skip_ws(literal, pos)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Missing `(` after FIXED'
       RETURN
    END IF
    IF (literal(pos:pos) /= '(') THEN
       iostat = 2
       iomsg = 'Missing `(` after FIXED'
       RETURN
    END IF
    ! Skip the `(`
    pos = pos + 1

    ! Can have some blanks.
    CALL skip_ws(literal, pos)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Missing width argument for FIXED'
       RETURN
    END IF

    ! A string of unsigned digits
    start = pos
    ! Advance pos to one past the end of the digit string.
    DO WHILE (pos <= LEN(literal))
       IF (SCAN(literal(pos:pos), '0123456789') == 0) EXIT
       pos = pos + 1
    END DO
    IF (pos == start) THEN
       ! There were no digits!
       iostat = 2
       iomsg = 'Missing or badly formed width for FIXED'
       RETURN
    END IF
    ! Convert the digit string to an integer.
    WRITE (fmt, "('(I',I0,')')") pos - start
    READ (literal(start:pos-1), fmt, IOSTAT=iostat, IOMSG=iomsg) fixed
    IF (iostat /= 0) RETURN

    ! Closing parenthesis.
    CALL skip_ws(literal, pos)
    IF (pos > LEN(literal)) THEN
       iostat = 2
       iomsg = 'Missing `)` after FIXED'
       RETURN
    END IF
    IF (literal(pos:pos) /= ')') THEN
       iostat = 2
       iomsg = 'Missing `)` after FIXED'
       RETURN
    END IF
    ! Skip the `)`
    pos = pos + 1

    iostat = 0

  END SUBROUTINE parse_fixed


  !*****************************************************************************
  !!
  !> Parse a quoted string out of a literal.

  ! This bears a striking resemblance to read_delimited.
  SUBROUTINE get_quoted_string(literal, pos, string, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The literal supplied after the DT edit descriptor.
    CHARACTER(*), INTENT(IN) :: literal

    !> The current position in the literal.  Before calling this should be 
    !! the quote character (delimiter) at the start of the string.  After 
    !! calling this will be the position after the closing delimiter.
    INTEGER, INTENT(INOUT) :: pos

    !> The string parsed out from the literal.
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: string

    !> IOSTAT error code - positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    CHARACTER :: delim        ! The quote character delimiting the string.
    LOGICAL :: was_delim      ! Indicate the last character was a delimiter.

    !***************************************************************************

    ! Save the delimiting quote.
    delim = literal(pos:pos)
    pos = pos + 1

    was_delim = .FALSE.
    string = ''

    ! Loop until we run out of characters or we detect end of the delimited 
    ! string.
    DO
       IF (pos > LEN(literal)) THEN
          IF (was_delim) THEN
             ! End of delimited string immediately before end of the literal.  
             ! We pretend the string parse was ok, and then let our callers 
             ! deal with the absence of any subsequent tokens.
             iostat = 0
             RETURN
          ELSE
             ! Undelimited end of string.
             iostat = 2
             iomsg = 'Missing closing ' // delim // ' character'
             RETURN
          END IF
       END IF

       IF (literal(pos:pos) == delim) THEN
          IF (was_delim) THEN
             ! Doubled delimiter is one delimiter in the value.
             string = string // literal(pos:pos)
             was_delim = .FALSE.
          ELSE
             ! Need to test next character to see what is happening.
             was_delim = .TRUE.
          END IF
       ELSE IF (was_delim) THEN
          ! The previous character was actually the delimiter for the 
          ! end of the string.
          iostat = 0
          RETURN
       ELSE
          string = string // literal(pos:pos)
       END IF

       ! Loop, considering the next character.
       pos = pos + 1
    END DO

  END SUBROUTINE get_quoted_string


  !*****************************************************************************
  !!
  !> Get the next non-blank character, advancing records if necessary.

  SUBROUTINE get_next_non_blank_any_record(unit, ch, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from.
    INTEGER, INTENT(IN) :: unit

    !> The non-blank character read.  Not valid if IOSTAT is non-zero.
    CHARACTER(LEN=1,KIND=ck), INTENT(OUT) :: ch

    !> IOSTAT code - IOSTAT_END on end of file, positive on error, 
    !! zero otherwise.  This will never be IOSTAT_EOR.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG error message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Local variant of iomsg, so it doesn't get inappropriately redefined.
    CHARACTER(LEN=LEN(iomsg)) :: local_iomsg

    !***************************************************************************

    DO
       CALL get_next_non_blank_this_record(unit, ch, iostat, local_iomsg)
       IF (IS_IOSTAT_EOR(iostat)) THEN
          ! Try again on the next record.
          READ (unit, "(/)", IOSTAT=iostat, IOMSG=iomsg)
          IF (iostat /= 0) RETURN
       ELSE IF (iostat /= 0) THEN
          ! Some sort of problem.
          iomsg = local_iomsg
          RETURN
       ELSE 
          ! Got it!
          EXIT
       END IF
    END DO

  END SUBROUTINE get_next_non_blank_any_record


  !*****************************************************************************
  !!
  !> Get the next non-blank character in the current record.

  SUBROUTINE get_next_non_blank_this_record(unit, ch, iostat, iomsg)

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit to read from.
    INTEGER, INTENT(IN) :: unit

    !> The non-blank character read.  Not defined if IOSTAT is non-zero.
    CHARACTER(LEN=1,KIND=ck), INTENT(OUT) :: ch

    !> IOSTAT code - IOSTAT_END on end of file, IOSTAT_EOR on end of record, 
    !! positive on error, zero otherwise.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG error message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !***************************************************************************

    DO
       ! We spcify non-advancing, just in case we want this callable outside 
       ! the context of a child input statement.  The PAD specifier simply 
       ! saves the need for the READ statement to define ch if EOR is hit.
       !=========================================================================
       !READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg, ADVANCE='NO') ch
       !=========================================================================
       ! ...but that causes ifort to blow up at runtime.
       READ (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg, PAD='NO') ch
       !=========================================================================
       IF (iostat /= 0) RETURN

       IF (ch /= '') EXIT
    END DO

  END SUBROUTINE get_next_non_blank_this_record


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Specific procedure for the IOLength generic.


  !*****************************************************************************
  !!
  !> Get the IOLENGTH required to write a varying string value to a unit 
  !! connected for unformatted output.
  !!
  !! This procedure invokes INQUIRE without IOSTAT - if that fails then 
  !! execution will be terminated.

  ! Can't be PURE because inquire-stmt not permitted.
  FUNCTION IOLength_(string) RESULT(l)

    !---------------------------------------------------------------------------
    ! Characteristics

    !> The varying_string being inquired about.
    TYPE(varying_string), INTENT(IN) :: string

    !> @returns The number of file storage units required for the string 
    !! in an unformatted file.
    INTEGER :: l

    !---------------------------------------------------------------------------
    ! Locals

    ! File storage units of a default integer scalar.
    INTEGER :: integer_iolength

    ! File storage units of a CHARACTER(KIND=ck) scalar.
    INTEGER :: character_iolength

    !***************************************************************************

    ! Get the length required for a single integer.
    INQUIRE(IOLENGTH=integer_iolength) 1

    ! Get the length required for a single character of the relevant kind.
    INQUIRE(IOLENGTH=character_iolength) ck_'a'

    ! The unformatted representation is a default integer for the length 
    ! followed by the individual characters of the string.
    l = integer_iolength + character_iolength * LEN(string)

  END FUNCTION IOLength_


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures supporting defined input support.


  !*****************************************************************************
  !!
  !> Get the ACCESS mode for a unit.

  SUBROUTINE get_access(unit, access, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_INQUIRE_INTERNAL_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit for the connection.
    INTEGER, INTENT(IN) :: unit

    !> The access in force for the unit.
    !!
    !! - 0: UNDEFINED
    !! - 1: SEQUENTIAL
    !! - 2: DIRECT
    !! - 3: STREAM
    INTEGER, INTENT(OUT) :: access

    !> IOSTAT error code, non-zero on error.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Buffer for inquire of ACCESS, sized for SEQUENTIAL.
    CHARACTER(10) :: access_buffer

    ! Local variant of iomsg, so it doesn't get inappropriately redefined.
    CHARACTER(LEN(iomsg)) :: local_iomsg

    !***************************************************************************

    INQUIRE(unit, ACCESS=access_buffer, IOSTAT=iostat, IOMSG=local_iomsg)
    IF (iostat == IOSTAT_INQUIRE_INTERNAL_UNIT) THEN
       access = 1
       iostat = 0
       RETURN
    ELSE IF (iostat /= 0) THEN
       iomsg = local_iomsg
       RETURN
    END IF

    ! access_buffer will be UNDEFINED if the unit does not exist.
    SELECT CASE (access_buffer)
    CASE ('SEQUENTIAL')   ; access = 1
    CASE ('DIRECT')       ; access = 2
    CASE ('STREAM')       ; access = 3
    CASE DEFAULT          ; access = 0
    END SELECT

  END SUBROUTINE get_access


  !*****************************************************************************
  !!
  !> Get the DELIM changeable connection mode for the given unit.
  !!
  !! If the unit is connected to an internal file, then the default value 
  !! of NONE is always returned.

  SUBROUTINE get_delim_mode(unit, delim, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_INQUIRE_INTERNAL_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit for the connection.
    INTEGER, INTENT(IN) :: unit

    !> Represents the value of the DELIM mode.
    CHARACTER(LEN=1, KIND=ck), INTENT(OUT) :: delim

    !> IOSTAT error code, non-zero on error.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Buffer for INQUIRE about DELIM, sized for APOSTROHPE.
    CHARACTER(10) :: delim_buffer

    ! Local variant of iomsg, so it doesn't get inappropriately redefined.
    CHARACTER(LEN(iomsg)) :: local_iomsg

    !***************************************************************************

    ! Get the string representation of the changeable mode.
    INQUIRE(unit, DELIM=delim_buffer, IOSTAT=iostat, IOMSG=local_iomsg)
    IF (iostat == IOSTAT_INQUIRE_INTERNAL_UNIT) THEN
       ! We have no way of determining the DELIM mode for an internal file.
       iostat = 0
       delim = ''
       RETURN
    ELSE IF (iostat /= 0) THEN
       iomsg = local_iomsg
       RETURN
    END IF

    ! Interpret the DELIM string.
    IF (delim_buffer == 'QUOTE') THEN
       delim = '"'
    ELSE IF (delim_buffer == 'APOSTROPHE') THEN
       delim = ''''
    ELSE
       delim = '"'
    END IF

  END SUBROUTINE get_delim_mode


  !*****************************************************************************
  !!
  !> Get the DECIMAL changeable connection mode for the given unit.
  !!
  !! If the unit is connected to an internal file, then the default 
  !! value of DECIMAL is always returned.  This may not be the actual value in 
  !! force at the time of the call to this procedure.

  SUBROUTINE get_decimal_mode(unit, decimal_point, iostat, iomsg)

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_INQUIRE_INTERNAL_UNIT

    !---------------------------------------------------------------------------
    ! Arguments

    !> The unit for the connection.
    INTEGER, INTENT(IN) :: unit

    !> True if the decimal mode is POINT, false otherwise.
    LOGICAL, INTENT(OUT) :: decimal_point

    !> IOSTAT error code, non-zero on error.
    INTEGER, INTENT(OUT) :: iostat

    !> IOMSG explanatory message - only defined if @a iostat is non-zero.
    CHARACTER(*), INTENT(INOUT) :: iomsg

    !---------------------------------------------------------------------------
    ! Locals

    ! Buffer for INQUIRE about DECIMAL, sized for POINT or COMMA.
    CHARACTER(5) :: decimal_buffer

    ! Local variant of iomsg, so it doesn't get inappropriately redefined.
    CHARACTER(LEN(iomsg)) :: local_iomsg

    !***************************************************************************

    ! Get the string representation of the changeable mode.
    INQUIRE(unit, DECIMAL=decimal_buffer, IOSTAT=iostat, IOMSG=local_iomsg)
    IF (iostat == IOSTAT_INQUIRE_INTERNAL_UNIT) THEN
       ! We have no way of determining the decimal mode for an internal file.
       iostat = 0
       decimal_point = .TRUE.
       RETURN
    ELSE IF (iostat /= 0) THEN
       iomsg = local_iomsg
       RETURN
    END IF

    ! Interpret the DECIMAL string.
    decimal_point = decimal_buffer == 'POINT'

  END SUBROUTINE get_decimal_mode

END MODULE iso_varying_string
