.class public core009
.super java/lang/Object
.method public <init>()V
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit locals 1
invokestatic core009/main()I
pop
return
.end method
.method public static main()I
.limit stack 1000
.limit locals 1
invokestatic core009/foo()I
istore 0
iload 0
invokestatic Runtime/printInt(I)V
ldc 0
ireturn
ldc 0
ireturn
.end method
.method public static foo()I
.limit stack 1000
.limit locals 0
ldc 10
ireturn
.end method
