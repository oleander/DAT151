.class public good13
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good13/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 4
invokestatic Runtime/readInt()I
dup
istore 0
pop
ldc 2
dup
istore 1
pop
Label0:
bipush 1
iload 1
iload 0
if_icmple Label2
pop
bipush 0
Label2:
ifeq Label1
bipush 1
istore 2
ldc 2
istore 3
Label3:
bipush 1
iload 3
iload 3
imul
iload 1
if_icmple Label7
pop
bipush 0
Label7:
ifeq Label5
iload 2
ifeq Label5
bipush 1
goto Label6
Label5:
bipush 0
Label6:
ifeq Label4
bipush 1
iload 1
iload 3
idiv
iload 3
imul
iload 1
if_icmpeq Label10
pop
bipush 0
Label10:
ifeq Label9
bipush 0
dup
istore 2
pop
goto Label8
Label9:
Label8:
iload 3
dup
bipush 1
iadd
istore 3
pop
goto Label3
Label4:
iload 2
ifeq Label13
bipush 1
iload 0
iload 1
idiv
iload 1
imul
iload 0
if_icmpeq Label15
pop
bipush 0
Label15:
ifeq Label13
bipush 1
goto Label14
Label13:
bipush 0
Label14:
ifeq Label12
iload 1
invokestatic Runtime/printInt(I)V
iload 0
iload 1
idiv
dup
istore 0
pop
goto Label11
Label12:
iload 1
dup
bipush 1
iadd
istore 1
pop
Label11:
goto Label0
Label1:
ldc 0
ireturn
ldc 0
ireturn
.end method
