.class public good15
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic good15/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 2
ldc 1
istore 0
iload 0
invokestatic Runtime/printInt(I)V
bipush 1
ifeq Label0
goto Label2
Label0:
bipush 1
iload 0
dup
bipush 1
iadd
istore 0
ldc 45
if_icmpne Label4
pop
bipush 0
Label4:
ifeq Label1
Label2:
bipush 1
goto Label3
Label1:
bipush 0
Label3:
pop
iload 0
invokestatic Runtime/printInt(I)V
bipush 0
ifeq Label5
goto Label7
Label5:
bipush 1
iload 0
dup
bipush 1
iadd
istore 0
ldc 0
if_icmpge Label9
pop
bipush 0
Label9:
ifeq Label6
Label7:
bipush 1
goto Label8
Label6:
bipush 0
Label8:
pop
iload 0
invokestatic Runtime/printInt(I)V
bipush 1
ifeq Label10
bipush 1
iload 0
dup
bipush 1
iadd
istore 0
ldc 0
if_icmplt Label12
pop
bipush 0
Label12:
ifeq Label10
bipush 1
goto Label11
Label10:
bipush 0
Label11:
pop
iload 0
invokestatic Runtime/printInt(I)V
bipush 0
ifeq Label13
bipush 1
iload 0
dup
bipush 1
iadd
istore 0
ldc 0
if_icmpgt Label15
pop
bipush 0
Label15:
ifeq Label13
bipush 1
goto Label14
Label13:
bipush 0
Label14:
pop
iload 0
invokestatic Runtime/printInt(I)V
ldc 0
istore 1
bipush 1
ldc 34
ldc 6
if_icmplt Label20
pop
bipush 0
Label20:
ifeq Label18
bipush 1
iload 1
ldc 0
if_icmplt Label21
pop
bipush 0
Label21:
ifeq Label18
bipush 1
goto Label19
Label18:
bipush 0
Label19:
ifeq Label17
iload 0
invokestatic Runtime/printInt(I)V
goto Label16
Label17:
ldc 42
invokestatic Runtime/printInt(I)V
Label16:
ldc 0
ireturn
.end method
