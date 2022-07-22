#include <cctype>
#include <iostream>

#include "jit.hpp"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/LinkAllPasses.h>
#include <llvm/Support/TargetSelect.h>

#define TRUEVAL(ctx) llvm::ConstantInt::get(ctx, llvm::APInt(1, 1, false))

static std::unique_ptr<llvm::LLVMContext> llvmCtx =
    std::make_unique<llvm::LLVMContext>();
static std::unique_ptr<llvm::Module> llvmModule =
    std::make_unique<llvm::Module>("llvm_boot_camp", *llvmCtx);
static llvm::IRBuilder<> llvmBuilder(*llvmCtx);
// static llvm::FunctionPassManager fpm(llvmModule.get());
static llvm::ExitOnError ExitOnErr;

enum BinOps
{
    Add,
    Mul,
    Lt, // less than
    Eq, // equal
};

struct FunctionAST
{
    llvm::Function *func;
};

struct Expr
{
    Expr() {}
    virtual ~Expr() {}
};

// example: if (a) { b } else { d }
// If is an expression.
struct IfAST : public Expr
{
    Expr *condition;
    Expr *then_expr;
    Expr *else_expr;
};

// example: let a = 10 * 2; next
struct LetAST : public Expr
{
    std::string variable;
    Expr *expr;
    Expr *next;
};

// example: b * c
struct BinOpAST : public Expr
{
    Expr *left;
    Expr *right;
    BinOps operation;
};

// example: 100
struct IntAST : public Expr
{
    int value;
};

// example: true
struct BoolAST : public Expr
{
    bool value;
};

// example: return a
struct ReturnAST : public Expr
{
    Expr *expr;
};

// example: a
struct VarAST : public Expr
{
    std::string variable;
};

class Generator
{
public:
    llvm::Function *func_;
    std::map<std::string, llvm::Value *> variables_;
    Expr *expr_;

    void generate();

private:
    llvm::Value *generate_expr(Expr *expr);
    llvm::Value *generate_bool(BoolAST &ast);
    llvm::Value *generate_int(IntAST &ast);
    llvm::Value *generate_bin_op(BinOpAST &ast);
    llvm::Value *generate_if(IfAST &ast);
    llvm::Value *generate_return(ReturnAST &ast);
    llvm::Value *generate_let(LetAST &ast);
    llvm::Value *generate_var(VarAST &ast);
};

void Generator::generate()
{
    generate_expr(expr_);
}

// generate a constant boolean value
llvm::Value *Generator::generate_bool(BoolAST &ast)
{
    uint64_t value = ast.value ? 1 : 0;
    auto bool_type = llvm::IntegerType::getInt1Ty(*llvmCtx); // bool
    return llvm::ConstantInt::get(bool_type, value, true);   // value
}

// generate a constant integer value
llvm::Value *Generator::generate_int(IntAST &ast)
{
    auto int_type = llvm::IntegerType::getInt32Ty(*llvmCtx);  // int32_t
    return llvm::ConstantInt::get(int_type, ast.value, true); // value
}

// generate a binary operation
llvm::Value *Generator::generate_bin_op(BinOpAST &ast)
{
    auto left = generate_expr(ast.left);
    auto right = generate_expr(ast.right);

    switch (ast.operation)
    {
    case BinOps::Add:
    {
        // addition: a + b
        return llvmBuilder.CreateAdd(left, right, "add");
        break;
    }
    case BinOps::Mul:
    {
        // multiplication: a * b
        return llvmBuilder.CreateMul(left, right, "mul");
        break;
    }
    case BinOps::Lt:
    {
        // equal: a == b
        return llvmBuilder.CreateICmpSLT(left, right, "lt");
    }
    case BinOps::Eq:
    {
        // equal: a == b
        return llvmBuilder.CreateICmpEQ(left, right, "eq");
    }
    default:
        std::cerr << "invalid operation" << std::endl;
        exit(1);
    }
}

// generate a expression
llvm::Value *Generator::generate_expr(Expr *expr)
{
    decltype(auto) ti = typeid(*expr);

    if (ti == typeid(BoolAST))
    {
        return generate_bool(*dynamic_cast<BoolAST *>(expr));
    }
    else if (ti == typeid(IntAST))
    {
        return generate_int(*dynamic_cast<IntAST *>(expr));
    }
    else if (ti == typeid(BinOpAST))
    {
        return generate_bin_op(*dynamic_cast<BinOpAST *>(expr));
    }
    else if (ti == typeid(IfAST))
    {
        return generate_if(*dynamic_cast<IfAST *>(expr));
    }
    else if (ti == typeid(ReturnAST))
    {
        return generate_return(*dynamic_cast<ReturnAST *>(expr));
    }
    else if (ti == typeid(LetAST))
    {
        return generate_let(*dynamic_cast<LetAST *>(expr));
    }
    else if (ti == typeid(VarAST))
    {
        return generate_var(*dynamic_cast<VarAST *>(expr));
    }
    std::cerr << "invalid expression" << std::endl;
    exit(1);
}

// generate if a expression
llvm::Value *Generator::generate_if(IfAST &ast)
{
    auto if_block = llvm::BasicBlock::Create(*llvmCtx, "if", func_);
    auto then_block = llvm::BasicBlock::Create(*llvmCtx, "then", func_);
    auto else_block = llvm::BasicBlock::Create(*llvmCtx, "else", func_);
    auto end_if_block = llvm::BasicBlock::Create(*llvmCtx, "endif", func_);

    //-------------------------------------------------------------------------
    // condition

    // set the insertion point to the "if" block
    llvmBuilder.SetInsertPoint(if_block);

    auto cond = generate_expr(ast.condition); // condition

    // insert a conditional branch
    llvmBuilder.CreateCondBr(cond, then_block, else_block);

    //-------------------------------------------------------------------------
    // then expression

    // set the insertion point to the "then" block
    llvmBuilder.SetInsertPoint(then_block);

    // generate expressions
    auto result_then = generate_expr(ast.then_expr);

    // jump to the endif block
    llvmBuilder.CreateBr(end_if_block);

    //-------------------------------------------------------------------------
    // else expression

    // set the insertion point to the "else" block
    llvmBuilder.SetInsertPoint(else_block);

    // generate expressions
    auto result_else = generate_expr(ast.else_expr);

    // jump to the endif block
    llvmBuilder.CreateBr(end_if_block);

    //-------------------------------------------------------------------------

    // set the insertion point to the "endif" block
    llvmBuilder.SetInsertPoint(end_if_block);

    // insert a phi instruction
    auto result =
        llvmBuilder.CreatePHI(llvm::IntegerType::get(*llvmCtx, 64), 2, "result");

    result->addIncoming(result_then, then_block);
    result->addIncoming(result_else, else_block);

    return result;
}

// generate a return expression
llvm::Value *Generator::generate_return(ReturnAST &ast)
{
    auto expr = generate_expr(ast.expr);

    // insert a return instruction
    llvmBuilder.CreateRet(expr);

    // return a void value (dummy value)
    return llvm::UndefValue::get(llvm::Type::getVoidTy(*llvmCtx));
}

// generate a let expression
llvm::Value *Generator::generate_let(LetAST &ast)
{
    auto expr = generate_expr(ast.expr);
    variables_[ast.variable] = expr;

    if (ast.next)
    {
        generate_expr(ast.next);
    }

    // return a void value (dummy value)
    return llvm::UndefValue::get(llvm::Type::getVoidTy(*llvmCtx));
}

llvm::Value *Generator::generate_var(VarAST &ast)
{
    auto it = variables_.find(ast.variable);
    if (it == variables_.end())
    {
        std::cerr << "undefind variable: " << ast.variable << std::endl;
        exit(1);
    }

    return it->second;
}

// uint32_t __example(bool arg0, uint32_t arg1, uint32_t arg2)
// {
//    let a = if (arg0) { arg1 * arg2 } else { arg1 + arg2 };
//    return a;
// }
llvm::Function *make_example_function()
{
    //-------------------------------------------------------------------------
    // generate function definition as
    // uint32_t __example(bool arg0, uint32_t arg1, uint32_t arg2);

    // create the type of arguments
    std::vector<llvm::Type *> arg_type;                    // type of arguments
    auto arg_type0 = llvm::IntegerType::get(*llvmCtx, 1);  // boolean type
    auto arg_type1 = llvm::IntegerType::get(*llvmCtx, 64); // 64 bit integer type
    auto arg_type2 = llvm::IntegerType::get(*llvmCtx, 64); // 64 bit integer type

    arg_type.push_back(arg_type0);
    arg_type.push_back(arg_type1);
    arg_type.push_back(arg_type2);

    // create the type of the return value
    auto ret_type = llvm::IntegerType::get(*llvmCtx, 64); // 64 bit integer type

    // create the type of the function
    auto func_type = llvm::FunctionType::get(ret_type, arg_type, false);

    // create the prototype of function
    auto func_def = llvm::Function::Create(
        func_type, llvm::Function::ExternalLinkage, "__example", *llvmModule);

    // get arguments
    std::vector<llvm::Value *> args;
    for (auto &v : func_def->args())
    {
        args.push_back(&v);
    }

    //-------------------------------------------------------------------------
    // create AST
    //
    // let a = if (arg0) { arg1 * arg2 } else { arg1 + arg3 };
    // return a;

    // arg1
    auto arg0 = new VarAST;
    arg0->variable = "arg0";

    // arg1
    auto arg1 = new VarAST;
    arg1->variable = "arg1";

    // arg2
    auto arg2 = new VarAST;
    arg2->variable = "arg2";

    // arg1 * arg2
    auto mul = new BinOpAST;
    mul->left = arg1, mul->right = arg2, mul->operation = BinOps::Mul;

    // arg1 + arg2
    auto add = new BinOpAST;
    add->left = arg1, add->right = arg2, add->operation = BinOps::Add;

    // if (arg0) { mul } else { add }
    auto if_expr = new IfAST;
    if_expr->condition = arg0, if_expr->then_expr = mul, if_expr->else_expr = add;

    // a
    auto a = new VarAST;
    a->variable = "a";

    // return a;
    auto ret = new ReturnAST;
    ret->expr = a;

    // let a = if_expr;
    auto let = new LetAST;
    let->variable = "a", let->expr = if_expr, let->next = ret;

    //-------------------------------------------------------------------------
    // create a generator, and set the AST and the arguments

    auto generator = Generator();
    generator.func_ = func_def;
    generator.expr_ = let;
    generator.variables_["arg0"] = args[0];
    generator.variables_["arg1"] = args[1];
    generator.variables_["arg2"] = args[2];

    //-------------------------------------------------------------------------
    generator.generate();

    return generator.func_;
}

int main(int argc, char *argv[])
{
    // initialize
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // // Do simple "peephole" optimizations and bit-twiddling optzns.
    // fpm.add(llvm::createInstructionCombiningPass());
    // // Reassociate expressions.
    // fpm.add(llvm::createReassociatePass());
    // // Eliminate Common SubExpressions.
    // fpm.add(llvm::createGVNPass());
    // // Simplify the control flow graph (deleting unreachable blocks, etc).
    // fpm.add(llvm::createCFGSimplificationPass());
    // // Use registers instead of memory if possible
    // fpm.add(llvm::createPromoteMemoryToRegisterPass());
    // // Loop vectorization
    // fpm.add(llvm::createLoopVectorizePass());
    // // Loop unrolling
    // fpm.add(llvm::createLoopUnrollPass());
    // // Loop unswitching
    // fpm.add(llvm::createLoopUnswitchPass());

    // fpm.doInitialization();

    // generate LLVM IR
    auto func = make_example_function();

    // Run the optimizer on the function.
    // fpm.run(*func);

    // print LLVM IR
    std::string s;
    llvm::raw_string_ostream os(s);
    llvmModule->print(os, nullptr);
    std::cout << s << std::endl;

    // JIT compilation
    auto jit = ExitOnErr(llvm::orc::KaleidoscopeJIT::Create());

    ExitOnErr(jit->addModule(llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmCtx))));

    // find address of the function
    auto symbol = jit->lookup("__example");
    auto ex = (uint64_t(*)(bool, uint64_t, uint64_t))(symbol->getAddress());

    // call the function
    std::cerr << "ex(false, 10, 20) = " << ex(false, 10, 20) << std::endl;
    std::cerr << "ex(true, 10, 20) = " << ex(true, 10, 20) << std::endl;

    return 0;
}
