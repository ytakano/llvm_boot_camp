# llvm_boot_camp

![LLVM Boot Camp](https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Marine_Corps_drill_instructor_yells_at_recruit.jpg/1920px-Marine_Corps_drill_instructor_yells_at_recruit.jpg)

## Create a Docker Container for LLVM-13

```text
$ docker run --rm -v $HOME/llvm:/llvm -it ytakanoster/llvm:13
# cd /llvm
# git clone https://github.com/ytakano/llvm_boot_camp.git
# cd llvm_boot_camp/src
# cmake .
# make
```

## Generate `example.ll`

```text
# ./llvm_boot_camp > example.ll
ex(false, 10, 20) = 30
ex(true, 10, 20) = 200
```

```llvm
; ModuleID = 'llvm_boot_camp'
source_filename = "llvm_boot_camp"

define i64 @__example(i1 %0, i64 %1, i64 %2) {
if:
  br i1 %0, label %then, label %else

then:                                             ; preds = %if
  %mul = mul i64 %1, %2
  br label %endif

else:                                             ; preds = %if
  %add = add i64 %1, %2
  br label %endif

endif:                                            ; preds = %else, %then
  %result = phi i64 [ %mul, %then ], [ %add, %else ]
  ret i64 %result
}
```

## Generate `example.s`

```text
# llc example.ll
# ls example.s
```

```assembly
        .text
        .file   "llvm_boot_camp"
        .globl  __example                       # -- Begin function __example
        .p2align        4, 0x90
        .type   __example,@function
__example:                              # @__example
        .cfi_startproc
# %bb.0:                                # %if
        movq    %rsi, %rax
        testb   $1, %dil
        je      .LBB0_2
# %bb.1:                                # %then
        imulq   %rdx, %rax
        retq
.LBB0_2:                                # %else
        addq    %rdx, %rax
        retq
.Lfunc_end0:
        .size   __example, .Lfunc_end0-__example
        .cfi_endproc
                                        # -- End function
        .section        ".note.GNU-stack","",@progbits
```
