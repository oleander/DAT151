.class public core013
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic core013/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 0
ldc 0
ldc 1
isub
invokestatic core013/test(I)Z
ifeq Label0
ldc 0
invokestatic core013/test(I)Z
ifeq Label0
bipush 1
goto Label1
Label0:
bipush 0
Label1:
invokestatic core013/printBool(Z)V
ldc 0
ldc 2
isub
invokestatic core013/test(I)Z
ifeq Label2
goto Label4
Label2:
ldc 1
invokestatic core013/test(I)Z
ifeq Label3
Label4:
bipush 1
goto Label5
Label3:
bipush 0
Label5:
invokestatic core013/printBool(Z)V
ldc 3
invokestatic core013/test(I)Z
ifeq Label8
ldc 0
ldc 5
isub
invokestatic core013/test(I)Z
ifeq Label8
bipush 1
goto Label9
Label8:
bipush 0
Label9:
ifeq Label6
bipush 1
ifeq Label6
bipush 1
goto Label7
Label6:
bipush 0
Label7:
invokestatic core013/printBool(Z)V
ldc 3
invokestatic core013/test(I)Z
ifeq Label10
goto Label12
Label10:
ldc 0
ldc 5
isub
invokestatic core013/test(I)Z
ifeq Label14
bipush 1
ifeq Label14
bipush 1
goto Label15
Label14:
bipush 0
Label15:
ifeq Label11
Label12:
bipush 1
goto Label13
Label11:
bipush 0
Label13:
invokestatic core013/printBool(Z)V
bipush 1
invokestatic core013/printBool(Z)V
bipush 0
invokestatic core013/printBool(Z)V
ldc 0
ireturn
ldc 0
ireturn
.end method
.method public static printBool(Z)V
.limit stack 1000
.limit locals 1
iload 0
ifeq Label17
goto Label16
Label17:
Label16:
return
.end method
.method public static test(I)Z
.limit stack 1000
.limit locals 1
bipush 1
iload 0
ldc 0
if_icmpgt Label18
pop
bipush 0
Label18:
ireturn
.end method
