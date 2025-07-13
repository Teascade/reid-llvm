; ModuleID = 'test'
source_filename = "test"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define ptr @array() {
array:
  %array_alloca = alloca i16, i16 4, align 2
  %array_gep = getelementptr i16, ptr %array_alloca, i32 0
  store i16 10, ptr %array_gep, align 2
  %array_gep1 = getelementptr i16, ptr %array_alloca, i32 1
  store i16 15, ptr %array_gep1, align 2
  %array_gep2 = getelementptr i16, ptr %array_alloca, i32 2
  store i16 7, ptr %array_gep2, align 2
  %array_gep3 = getelementptr i16, ptr %array_alloca, i32 3
  store i16 9, ptr %array_gep3, align 2

  %array_alloca4 = alloca ptr, i16 1, align 8
  %array_gep5 = getelementptr ptr, ptr %array_alloca4, i32 0
  store ptr %array_alloca, ptr %array_gep5, align 8
  ret ptr %array_alloca4
}

define i16 @main() {
main:
  %call = call ptr @array()
  %array_gep = getelementptr ptr, ptr %call, i32 0
  %load5 = load ptr, ptr %array_gep2, align 8
  %array_gep1 = getelementptr i16, ptr %load5, i32 3
  store i16 5, ptr %array_gep1, align 2

  %array_gep2 = getelementptr ptr, ptr %call, i32 0
  %load = load ptr, ptr %array_gep2, align 8
  %array_gep3 = getelementptr i16, ptr %load, i32 3
  %load4 = load i16, ptr %array_gep3, align 2
  
  ret i16 %load4
}