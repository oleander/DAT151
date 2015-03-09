.class public good07
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good07/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 2
invokestatic Runtime/readInt()I
istore 0
iload 0
ldc 2
idiv
istore 1
Label0:
bipush 1
iload 1
ldc 1
if_icmpgt Label2
pop
bipush 0
Label2:
ifeq Label1
bipush 1
iload 1
iload 0
iload 1
idiv
imul
iload 0
if_icmpeq Label5
pop
bipush 0
Label5:
ifeq Label4
iload 1
invokestatic Runtime/printInt(I)V
goto Label3
Label4:
Label3:
iload 1
dup
bipush 1
isub
istore 1
pop
goto Label0
Label1:
ldc 0
ireturn
.end method
