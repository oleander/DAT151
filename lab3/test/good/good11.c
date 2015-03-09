.class public good11
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good11/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 3
ldc 0
istore 0
ldc 0
istore 1
Label0:
bipush 1
invokestatic Runtime/readInt()I
dup
istore 2
ldc 0
if_icmpne Label2
pop
bipush 0
Label2:
ifeq Label1
iload 0
iload 2
iadd
dup
istore 0
pop
iload 1
dup
bipush 1
iadd
istore 1
pop
goto Label0
Label1:
iload 0
iload 1
idiv
invokestatic Runtime/printInt(I)V
ldc 0
ireturn
.end method
