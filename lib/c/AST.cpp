#include "c/AST.h"

namespace c {

const char *AST::kindName(Kind kind) {
  switch (kind) {
  case MUL:
    return "Mul";
  case ADD:
    return "Add";
  case SUB:
    return "Sub";
  case VAR:
    return "Var";
  case CONST_INT:
    return "IntConst";
  case INC_PRE:
    return "IncPre";
  case DEREF:
    return "Deref";
  case ADDR:
    return "Addr";
  case EMPTY:
    return "Empty";
  case ASSIGN:
    return "Assign";
  case ASSIGN_ADD:
    return "AssignAdd";
  case ASSIGN_SUB:
    return "AssignSub";
  case ASSIGN_MUL:
    return "AssignMul";
  case ASSIGN_DIV:
    return "AssignDiv";
  case ASSIGN_MOD:
    return "AssignMod";
  case ASSIGN_AND:
    return "AssignAnd";
  case ASSIGN_OR:
    return "AssignOr";
  case ASSIGN_XOR:
    return "AssignXor";
  case ASSIGN_LSHIFT:
    return "AssignShiftLeft";
  case ASSIGN_RSHIFT:
    return "AssignShiftRight";
  case DIV:
    return "Division";
  case MOD:
    return "Modulo";
  case BIT_AND:
    return "BitAnd";
  case BIT_OR:
    return "BitOr";
  case BIT_NOT:
    return "BitNot";
  case BIT_XOR:
    return "BitXor";
  case LSHIFT:
    return "ShiftLeft";
  case RSHIFT:
    return "ShiftRight";
  case LOG_AND:
    return "LogicalAnd";
  case LOG_OR:
    return "LogicalOr";
  case LOG_NOT:
    return "LogicalNot";
  case INC_POST:
    return "PostIncrement";
  case DEC_PRE:
    return "PreDecrement";
  case DEC_POST:
    return "PostDecrement";
  case EQ:
    return "==";
  case NEQ:
    return "!=";
  case LT:
    return "<";
  case GT:
    return ">";
  case LTE:
    return "<=";
  case GTE:
    return ">=";
  case ST_COMPOUND:
    return "CompoundStatement";
  case ST_IF:
    return "if";
  case ST_WHILE:
    return "while";
  case DECLARATOR:
    return "Declarator";
  case DECLARATION:
    return "Declaration";
  case FUNCTION_DEFINITION:
    return "FunctionDefinition";
  case TRANSLATION_UNIT:
    return "TranslationUnit";
  case ACCESS_MEMBER:
    return "MemberAccess";
  case ACCESS_ARRAY:
    return "ArrAccess";
  case FUNCTION_CALL:
    return "FunctionCall";
  case PLUS:
    return "Plus";
  case MINUS:
    return "Minus";
  case ST_CONTINUE:
    return "continue";
  case ST_BREAK:
    return "break";
  case ST_RETURN:
    return "return";
  case ST_FOR:
    return "for";
  case COMMA:
    return "Comma";
  case CAST:
    return "Cast";
  case ST_LABEL_NAMED:
    return "label";
  case ST_LABEL_DEFAULT:
    return "default";
  case ST_LABEL_CASE:
    return "case";
  case ST_SWITCH:
    return "switch";
  case ST_GOTO:
    return "goto";
  case ST_DO_WHILE:
    return "do_while";
  case TERNARY:
    return "Ternary";
  case INITIALIZER_LIST:
    return "InitializerList";
  case SIZEOF:
    return "Sizeof";
  case ALIGNOF:
    return "Alignof";
  case TYPE:
    return "Type";
  }
  return "unnamed";
}

} // namespace c
