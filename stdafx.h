// a8rawconv - A8 raw disk conversion utility
// Copyright (C) 2014 Avery Lee
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#pragma once

#ifdef _MSC_VER
	#define _CRT_SECURE_NO_WARNINGS
	#define _CRT_DISABLE_PERFCRIT_LOCKS
	#define _SECURE_SCL 0
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <vector>
#include <array>
#include <set>
#include <string>
#include <unordered_map>
#include <iostream>
#include <algorithm>
#include <memory>
#include <chrono>
#include <filesystem>

#include "binary.h"
#include "disk.h"
#include "diskio.h"
#include "globals.h"

#ifdef _DEBUG
	#define ASSERT(condition) if (!(condition)) { printf("Runtime assertion failed!\n" __FILE__ "(%u): "#condition"\n", __LINE__); __debugbreak(); } else ((void)0)
#else
	#define ASSERT(condition) if (!(condition)) { fatalf("Runtime assertion failed!\n" __FILE__ "(%u): "#condition"\n", __LINE__); } else ((void)0)
#endif

void fatal(const char *msg);
void fatalf(const char *msg, ...);
void fatal_read();
