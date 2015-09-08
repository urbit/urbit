
#=============================================================================
#
# This Makefile is part of the SoftFloat IEEE Floating-Point Arithmetic
# Package, Release 3, by John R. Hauser.
#
# Copyright 2011, 2012, 2013, 2014 The Regents of the University of California
# (Regents).  All Rights Reserved.  Redistribution and use in source and
# binary forms, with or without modification, are permitted provided that the
# following conditions are met:
#
# Redistributions of source code must retain the above copyright notice,
# this list of conditions, and the following two paragraphs of disclaimer.
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions, and the following two paragraphs of disclaimer in
# the documentation and/or other materials provided with the distribution.
# Neither the name of the Regents nor the names of its contributors may be
# used to endorse or promote products derived from this software without
# specific prior written permission.
#
# IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
# SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
# REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.  THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
# HEREUNDER IS PROVIDED "AS IS".  REGENTS HAS NO OBLIGATION TO PROVIDE
# MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#
#=============================================================================

SOURCE_DIR = ../../source
SPECIALIZE_TYPE = 8086

SOFTFLOAT_OPTS = -DINLINE_LEVEL=5 -DSOFTFLOAT_FAST_DIV64TO32

DELETE = rm -f
C_INCLUDES = -I. -I$(SOURCE_DIR)/$(SPECIALIZE_TYPE) -I$(SOURCE_DIR)/include
COMPILE_C = \
  $(CC) -c -std=gnu89 -Werror-implicit-function-declaration $(SOFTFLOAT_OPTS) \
    $(C_INCLUDES) -O2 -o $@
MAKELIB = ar crs $@

OBJSUF = .o
LIBSUF = .a

.PHONY: all
all: softfloat$(LIBSUF)

OBJS_PRIMITIVES = \
  s_compare96M$(OBJSUF) \
  s_compare128M$(OBJSUF) \
  s_shortShiftLeft64To96M$(OBJSUF) \
  s_shortShiftLeftM$(OBJSUF) \
  s_shiftLeftM$(OBJSUF) \
  s_shortShiftRightM$(OBJSUF) \
  s_shortShiftRightJam64$(OBJSUF) \
  s_shortShiftRightJamM$(OBJSUF) \
  s_shiftRightJam32$(OBJSUF) \
  s_shiftRightJam64$(OBJSUF) \
  s_shiftRightJamM$(OBJSUF) \
  s_shiftRightM$(OBJSUF) \
  s_countLeadingZeros8$(OBJSUF) \
  s_countLeadingZeros32$(OBJSUF) \
  s_countLeadingZeros64$(OBJSUF) \
  s_addM$(OBJSUF) \
  s_addCarryM$(OBJSUF) \
  s_addComplCarryM$(OBJSUF) \
  s_negXM$(OBJSUF) \
  s_sub1XM$(OBJSUF) \
  s_subM$(OBJSUF) \
  s_mul64To128M$(OBJSUF) \
  s_mul128MTo256M$(OBJSUF) \
  s_approxRecip32_1$(OBJSUF) \
  s_approxRecipSqrt32_1$(OBJSUF) \
  s_remStepMBy32$(OBJSUF) \

OBJS_SPECIALIZE = \
  softfloat_raiseFlags$(OBJSUF) \
  s_f32UIToCommonNaN$(OBJSUF) \
  s_commonNaNToF32UI$(OBJSUF) \
  s_propagateNaNF32UI$(OBJSUF) \
  s_f64UIToCommonNaN$(OBJSUF) \
  s_commonNaNToF64UI$(OBJSUF) \
  s_propagateNaNF64UI$(OBJSUF) \
  extF80M_isSignalingNaN$(OBJSUF) \
  s_extF80MToCommonNaN$(OBJSUF) \
  s_commonNaNToExtF80M$(OBJSUF) \
  s_propagateNaNExtF80M$(OBJSUF) \
  f128M_isSignalingNaN$(OBJSUF) \
  s_f128MToCommonNaN$(OBJSUF) \
  s_commonNaNToF128M$(OBJSUF) \
  s_propagateNaNF128M$(OBJSUF) \

OBJS_OTHERS = \
  s_roundPackToUI32$(OBJSUF) \
  s_roundPackMToUI64$(OBJSUF) \
  s_roundPackToI32$(OBJSUF) \
  s_roundPackMToI64$(OBJSUF) \
  s_normSubnormalF32Sig$(OBJSUF) \
  s_roundPackToF32$(OBJSUF) \
  s_normRoundPackToF32$(OBJSUF) \
  s_addMagsF32$(OBJSUF) \
  s_subMagsF32$(OBJSUF) \
  s_mulAddF32$(OBJSUF) \
  s_normSubnormalF64Sig$(OBJSUF) \
  s_roundPackToF64$(OBJSUF) \
  s_normRoundPackToF64$(OBJSUF) \
  s_addMagsF64$(OBJSUF) \
  s_subMagsF64$(OBJSUF) \
  s_mulAddF64$(OBJSUF) \
  s_tryPropagateNaNExtF80M$(OBJSUF) \
  s_invalidExtF80M$(OBJSUF) \
  s_normExtF80SigM$(OBJSUF) \
  s_roundPackMToExtF80M$(OBJSUF) \
  s_normRoundPackMToExtF80M$(OBJSUF) \
  s_addExtF80M$(OBJSUF) \
  s_compareNonnormExtF80M$(OBJSUF) \
  s_isNaNF128M$(OBJSUF) \
  s_tryPropagateNaNF128M$(OBJSUF) \
  s_invalidF128M$(OBJSUF) \
  s_shiftNormSigF128M$(OBJSUF) \
  s_roundPackMToF128M$(OBJSUF) \
  s_normRoundPackMToF128M$(OBJSUF) \
  s_addF128M$(OBJSUF) \
  s_mulAddF128M$(OBJSUF) \
  softfloat_state$(OBJSUF) \
  ui32_to_f32$(OBJSUF) \
  ui32_to_f64$(OBJSUF) \
  ui32_to_extF80M$(OBJSUF) \
  ui32_to_f128M$(OBJSUF) \
  ui64_to_f32$(OBJSUF) \
  ui64_to_f64$(OBJSUF) \
  ui64_to_extF80M$(OBJSUF) \
  ui64_to_f128M$(OBJSUF) \
  i32_to_f32$(OBJSUF) \
  i32_to_f64$(OBJSUF) \
  i32_to_extF80M$(OBJSUF) \
  i32_to_f128M$(OBJSUF) \
  i64_to_f32$(OBJSUF) \
  i64_to_f64$(OBJSUF) \
  i64_to_extF80M$(OBJSUF) \
  i64_to_f128M$(OBJSUF) \
  f32_to_ui32$(OBJSUF) \
  f32_to_ui64$(OBJSUF) \
  f32_to_i32$(OBJSUF) \
  f32_to_i64$(OBJSUF) \
  f32_to_ui32_r_minMag$(OBJSUF) \
  f32_to_ui64_r_minMag$(OBJSUF) \
  f32_to_i32_r_minMag$(OBJSUF) \
  f32_to_i64_r_minMag$(OBJSUF) \
  f32_to_f64$(OBJSUF) \
  f32_to_extF80M$(OBJSUF) \
  f32_to_f128M$(OBJSUF) \
  f32_roundToInt$(OBJSUF) \
  f32_add$(OBJSUF) \
  f32_sub$(OBJSUF) \
  f32_mul$(OBJSUF) \
  f32_mulAdd$(OBJSUF) \
  f32_div$(OBJSUF) \
  f32_rem$(OBJSUF) \
  f32_sqrt$(OBJSUF) \
  f32_eq$(OBJSUF) \
  f32_le$(OBJSUF) \
  f32_lt$(OBJSUF) \
  f32_eq_signaling$(OBJSUF) \
  f32_le_quiet$(OBJSUF) \
  f32_lt_quiet$(OBJSUF) \
  f32_isSignalingNaN$(OBJSUF) \
  f64_to_ui32$(OBJSUF) \
  f64_to_ui64$(OBJSUF) \
  f64_to_i32$(OBJSUF) \
  f64_to_i64$(OBJSUF) \
  f64_to_ui32_r_minMag$(OBJSUF) \
  f64_to_ui64_r_minMag$(OBJSUF) \
  f64_to_i32_r_minMag$(OBJSUF) \
  f64_to_i64_r_minMag$(OBJSUF) \
  f64_to_f32$(OBJSUF) \
  f64_to_extF80M$(OBJSUF) \
  f64_to_f128M$(OBJSUF) \
  f64_roundToInt$(OBJSUF) \
  f64_add$(OBJSUF) \
  f64_sub$(OBJSUF) \
  f64_mul$(OBJSUF) \
  f64_mulAdd$(OBJSUF) \
  f64_div$(OBJSUF) \
  f64_rem$(OBJSUF) \
  f64_sqrt$(OBJSUF) \
  f64_eq$(OBJSUF) \
  f64_le$(OBJSUF) \
  f64_lt$(OBJSUF) \
  f64_eq_signaling$(OBJSUF) \
  f64_le_quiet$(OBJSUF) \
  f64_lt_quiet$(OBJSUF) \
  f64_isSignalingNaN$(OBJSUF) \
  extF80M_to_ui32$(OBJSUF) \
  extF80M_to_ui64$(OBJSUF) \
  extF80M_to_i32$(OBJSUF) \
  extF80M_to_i64$(OBJSUF) \
  extF80M_to_ui32_r_minMag$(OBJSUF) \
  extF80M_to_ui64_r_minMag$(OBJSUF) \
  extF80M_to_i32_r_minMag$(OBJSUF) \
  extF80M_to_i64_r_minMag$(OBJSUF) \
  extF80M_to_f32$(OBJSUF) \
  extF80M_to_f64$(OBJSUF) \
  extF80M_to_f128M$(OBJSUF) \
  extF80M_roundToInt$(OBJSUF) \
  extF80M_add$(OBJSUF) \
  extF80M_sub$(OBJSUF) \
  extF80M_mul$(OBJSUF) \
  extF80M_div$(OBJSUF) \
  extF80M_rem$(OBJSUF) \
  extF80M_sqrt$(OBJSUF) \
  extF80M_eq$(OBJSUF) \
  extF80M_le$(OBJSUF) \
  extF80M_lt$(OBJSUF) \
  extF80M_eq_signaling$(OBJSUF) \
  extF80M_le_quiet$(OBJSUF) \
  extF80M_lt_quiet$(OBJSUF) \
  f128M_to_ui32$(OBJSUF) \
  f128M_to_ui64$(OBJSUF) \
  f128M_to_i32$(OBJSUF) \
  f128M_to_i64$(OBJSUF) \
  f128M_to_ui32_r_minMag$(OBJSUF) \
  f128M_to_ui64_r_minMag$(OBJSUF) \
  f128M_to_i32_r_minMag$(OBJSUF) \
  f128M_to_i64_r_minMag$(OBJSUF) \
  f128M_to_f32$(OBJSUF) \
  f128M_to_f64$(OBJSUF) \
  f128M_to_extF80M$(OBJSUF) \
  f128M_roundToInt$(OBJSUF) \
  f128M_add$(OBJSUF) \
  f128M_sub$(OBJSUF) \
  f128M_mul$(OBJSUF) \
  f128M_mulAdd$(OBJSUF) \
  f128M_div$(OBJSUF) \
  f128M_rem$(OBJSUF) \
  f128M_sqrt$(OBJSUF) \
  f128M_eq$(OBJSUF) \
  f128M_le$(OBJSUF) \
  f128M_lt$(OBJSUF) \
  f128M_eq_signaling$(OBJSUF) \
  f128M_le_quiet$(OBJSUF) \
  f128M_lt_quiet$(OBJSUF) \

OBJS_ALL = $(OBJS_PRIMITIVES) $(OBJS_SPECIALIZE) $(OBJS_OTHERS)

$(OBJS_ALL): \
  platform.h $(SOURCE_DIR)/include/primitiveTypes.h \
  $(SOURCE_DIR)/include/primitives.h
$(OBJS_SPECIALIZE) $(OBJS_OTHERS): \
  $(SOURCE_DIR)/include/softfloat_types.h $(SOURCE_DIR)/include/internals.h \
  $(SOURCE_DIR)/$(SPECIALIZE_TYPE)/specialize.h \
  $(SOURCE_DIR)/include/softfloat.h

$(OBJS_PRIMITIVES) $(OBJS_OTHERS): %$(OBJSUF): $(SOURCE_DIR)/%.c
	$(COMPILE_C) $(SOURCE_DIR)/$*.c

$(OBJS_SPECIALIZE): %$(OBJSUF): $(SOURCE_DIR)/$(SPECIALIZE_TYPE)/%.c
	$(COMPILE_C) $(SOURCE_DIR)/$(SPECIALIZE_TYPE)/$*.c

softfloat$(LIBSUF): $(OBJS_ALL)
	$(DELETE) $@
	$(MAKELIB) $^

.PHONY: clean
clean:
	$(DELETE) $(OBJS_ALL) softfloat$(LIBSUF)

