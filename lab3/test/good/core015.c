.class public core015
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic core015/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 0
ldc 17
invokestatic core015/ev(I)I
invokestatic Runtime/printInt(I)V
ldc 0
ireturn
ldc 0
ireturn
.end method
.method public static ev(I)I
.limit stack 1000
.limit locals 2
bipush 1
iload 0
ldc 0
if_icmpgt Label2
pop
bipush 0
Label2:
ifeq Label1
iload 0
ldc 2
isub
invokestatic core015/ev(I)I
dup
istore 1
pop
goto Label0
Label1:
bipush 1
iload 0
ldc 0
if_icmplt Label5
pop
bipush 0
Label5:
ifeq Label4
ldc 0
dup
istore 1
pop
goto Label3
Label4:
ldc 1
dup
istore 1
pop
Label3:
Label0:
iload 1
ireturn
.end method
