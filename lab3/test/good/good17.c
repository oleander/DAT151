.class public good17
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good17/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 3
ldc 6
dup
istore 0
pop
iload 0
ldc 7
iadd
dup
istore 1
pop
iload 1
invokestatic Runtime/printInt(I)V
ldc 4
dup
istore 2
pop
iload 2
invokestatic Runtime/printInt(I)V
iload 2
dup
istore 0
pop
iload 0
invokestatic Runtime/printInt(I)V
iload 0
invokestatic Runtime/printInt(I)V
iload 1
invokestatic Runtime/printInt(I)V
ldc 0
ireturn
.end method
