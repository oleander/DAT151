.class public good03
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good03/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 3
invokestatic Runtime/readInt()I
istore 0
ldc 1
istore 1
ldc 1
istore 2
Label0:
bipush 1
iload 2
iload 0
ldc 1
iadd
if_icmplt Label2
pop
bipush 0
Label2:
ifeq Label1
iload 2
iload 1
imul
dup
istore 1
pop
iload 2
ldc 1
iadd
istore 2
iload 2
pop
goto Label0
Label1:
iload 1
invokestatic Runtime/printInt(I)V
ldc 0
ireturn
.end method
