.class public good09
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good09/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 1
invokestatic Runtime/readInt()I
istore 0
iload 0
invokestatic Runtime/printInt(I)V
iload 0
dup
bipush 1
iadd
istore 0
invokestatic Runtime/printInt(I)V
iload 0
invokestatic Runtime/printInt(I)V
iload 0
ldc 1
iadd
istore 0
iload 0
invokestatic Runtime/printInt(I)V
iload 0
invokestatic Runtime/printInt(I)V
ldc 0
ireturn
.end method
