
#=============================================================================
#
# This Makefile is part of the SoftFloat IEEE Floating-Point Arithmetic
# Package, Release 3c, by John R. Hauser.
#
# Copyright 2011, 2012, 2013, 2014, 2015, 2016, 2017 The Regents of the
# University of California.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions, and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions, and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#
#  3. Neither the name of the University nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
# DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#=============================================================================

SOURCE_DIR ?= ../../source
SPECIALIZE_TYPE ?= 8086

SOFTFLOAT_OPTS ?= \
  -DSOFTFLOAT_ROUND_ODD -DINLINE_LEVEL=5 -DSOFTFLOAT_FAST_DIV32TO16 \
  -DSOFTFLOAT_FAST_DIV64TO32

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
  s_countLeadingZeros16$(OBJSUF) \
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
  s_approxRecip_1Ks$(OBJSUF) \
  s_approxRecip32_1$(OBJSUF) \
  s_approxRecipSqrt_1Ks$(OBJSUF) \
  s_approxRecipSqrt32_1$(OBJSUF) \
  s_remStepMBy32$(OBJSUF) \

OBJS_SPECIALIZE = \
  softfloat_raiseFlags$(OBJSUF) \
  s_f16UIToCommonNaN$(OBJSUF) \
  s_commonNaNToF16UI$(OBJSUF) \
  s_propagateNaNF16UI$(OBJSUF) \
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
  s_roundToUI32$(OBJSUF) \
  s_roundMToUI64$(OBJSUF) \
  s_roundToI32$(OBJSUF) \
  s_roundMToI64$(OBJSUF) \
  s_normSubnormalF16Sig$(OBJSUF) \
  s_roundPackToF16$(OBJSUF) \
  s_normRoundPackToF16$(OBJSUF) \
  s_addMagsF16$(OBJSUF) \
  s_subMagsF16$(OBJSUF) \
  s_mulAddF16$(OBJSUF) \
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
  ui32_to_f16$(OBJSUF) \
  ui32_to_f32$(OBJSUF) \
  ui32_to_f64$(OBJSUF) \
  ui32_to_extF80M$(OBJSUF) \
  ui32_to_f128M$(OBJSUF) \
  ui64_to_f16$(OBJSUF) \
  ui64_to_f32$(OBJSUF) \
  ui64_to_f64$(OBJSUF) \
  ui64_to_extF80M$(OBJSUF) \
  ui64_to_f128M$(OBJSUF) \
  i32_to_f16$(OBJSUF) \
  i32_to_f32$(OBJSUF) \
  i32_to_f64$(OBJSUF) \
  i32_to_extF80M$(OBJSUF) \
  i32_to_f128M$(OBJSUF) \
  i64_to_f16$(OBJSUF) \
  i64_to_f32$(OBJSUF) \
  i64_to_f64$(OBJSUF) \
  i64_to_extF80M$(OBJSUF) \
  i64_to_f128M$(OBJSUF) \
  f16_to_ui32$(OBJSUF) \
  f16_to_ui64$(OBJSUF) \
  f16_to_i32$(OBJSUF) \
  f16_to_i64$(OBJSUF) \
  f16_to_ui32_r_minMag$(OBJSUF) \
  f16_to_ui64_r_minMag$(OBJSUF) \
  f16_to_i32_r_minMag$(OBJSUF) \
  f16_to_i64_r_minMag$(OBJSUF) \
  f16_to_f32$(OBJSUF) \
  f16_to_f64$(OBJSUF) \
  f16_to_extF80M$(OBJSUF) \
  f16_to_f128M$(OBJSUF) \
  f16_roundToInt$(OBJSUF) \
  f16_add$(OBJSUF) \
  f16_sub$(OBJSUF) \
  f16_mul$(OBJSUF) \
  f16_mulAdd$(OBJSUF) \
  f16_div$(OBJSUF) \
  f16_rem$(OBJSUF) \
  f16_sqrt$(OBJSUF) \
  f16_eq$(OBJSUF) \
  f16_le$(OBJSUF) \
  f16_lt$(OBJSUF) \
  f16_eq_signaling$(OBJSUF) \
  f16_le_quiet$(OBJSUF) \
  f16_lt_quiet$(OBJSUF) \
  f16_isSignalingNaN$(OBJSUF) \
  f32_to_ui32$(OBJSUF) \
  f32_to_ui64$(OBJSUF) \
  f32_to_i32$(OBJSUF) \
  f32_to_i64$(OBJSUF) \
  f32_to_ui32_r_minMag$(OBJSUF) \
  f32_to_ui64_r_minMag$(OBJSUF) \
  f32_to_i32_r_minMag$(OBJSUF) \
  f32_to_i64_r_minMag$(OBJSUF) \
  f32_to_f16$(OBJSUF) \
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
  f64_to_f16$(OBJSUF) \
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
  extF80M_to_f16$(OBJSUF) \
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
  f128M_to_f16$(OBJSUF) \
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

