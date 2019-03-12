#include "Parser.h"

#include <iostream>
#include <string>
#include <queue>

#include "../codegen/CodeGen.h"
#include "../scope/ScopeManager.h"
#include "../lexer/Lexer.h"
#include "../lib/Token.h"

/**
 * This is the parser. It is an LL(1) recursive decent parser, though it
 * technically does not need to be recursive, given the LL(1) nature of the
 * grammar. Currently, production methods return true if a production existed
 * without errors, and false if it started to exist but failed parsing, or did
 * not exist at all. This is alright for the current error-handling mechanism,
 * which basically dies upon seeing the first error, and stops parsing.
 * Eventually we should have more advanced error-handling and resynchronization,
 * and keep parsing until we absolutely cannot. In this case, we'll want the
 * production methods to return true if they exist with or with out errors, and
 * false only if they don't exist. This is because we want to continue parsing
 * even in the midst of errors, and only want to stop if we cannot get
 * resynchronized. The resync logic will be separate, and is necessary to parse
 * even in the midst of errors, to stop errors from becoming incredibly verbose
 * if the parser cannot find a good starting spot after an error state. Since
 * the resync logic is not implemented yet, we don't parse in the midst of
 * errors. If our production methods returned true even when errors were found
 * and we wanted to stop parsing in the midst of errors, we'd have to do an
 * error check after ever single production method call. That's a lot of checks.
 * Therefore, it seemed easier to just make erroneous parses return false, since
 * in the case a production was required, erroneous and nonexistant would be
 * handled the same. In the case where a production was not required, we just
 * have to check to see if the method returned false because of an error, or
 * because it did not exist. This seems like less checks, but I could be wrong.
 */

// TODO(domfarolino): Factor these out as a part of
// https://github.com/domfarolino/compiler/issues/26.
// Implements
// https://docs.google.com/document/d/1QrD3HN5rHX-3zrShlq4g6sQgU0hXnOqtfcCx6OqqNNc/edit#heading=h.ilfzxw1fez7p.
bool IsArrayIndex(const SymbolRecord& symbolRecord) {
  return symbolRecord.type == SymbolType::Integer;
}

bool IsBooleanEquivalent(const SymbolRecord& symbolRecord) {
  if (symbolRecord.isArray) return false;

  return symbolRecord.type == SymbolType::Bool ||
         symbolRecord.type == SymbolType::Integer;
}

// TODO(domfarolino): Maybe we want to move this elsewhere?
AbstractType SymbolTypeToAbstractType(const SymbolRecord& symbol_record) {
  bool is_array = symbol_record.isArray;

  if (symbol_record.type == SymbolType::Integer)
    return (is_array) ? AbstractType::IntegerArray : AbstractType::Integer;
  else if (symbol_record.type == SymbolType::Float)
    return (is_array) ? AbstractType::FloatArray : AbstractType::Float;
  else if (symbol_record.type == SymbolType::Bool)
    return (is_array) ? AbstractType::BoolArray : AbstractType::Bool;
  else if (symbol_record.type == SymbolType::Char)
    return (is_array) ? AbstractType::CharArray : AbstractType::Char;
  else if (symbol_record.type == SymbolType::String)
    return (is_array) ? AbstractType::StringArray : AbstractType::String;

  // Assert: Not reached.
  return AbstractType::Void;
}

Parser::Parser(const std::string& program_name, Lexer& inLexer,
                                                ScopeManager &inScopes,
                                                bool inSymbolInsight,
                                                bool code_gen):
                                              program_name_(program_name),
                                              lexer_(inLexer),
                                              scopeManager_(inScopes),
                                              symbolInsight_(inSymbolInsight),
                                              code_gen_(code_gen) {
  token_ = lexer_.nextToken();

  Setup();

  // Start parsing the program from the <program> production!
  Program();
  FlushErrors();
}

// This method is responsible for performing initial CodeGen setup, adding the
// built-ins to the global scopeadding the built-ins into the ScopeManager, and
// getting the built-in implementation Value*s from CodeGen.
void Parser::Setup() {
  if (code_gen_ == false)
    CodeGen::EnterErrorState();

  std::vector<Value*> built_in_impls = CodeGen::Setup();

  if (code_gen_) {
    // TODO(domfarolino): [CODEGEN] Make a CodeGen::CreateMain().
    std::vector<std::tuple<std::string, AbstractType, int>> mainArguments;
    CodeGen::CreateFunction("main", AbstractType::Integer, mainArguments);
  }

  // This will need refactored if future built-ins have non-trivial signatures.
  std::vector<std::string> built_in_names =
    {"putInteger", "putFloat", "putBool", "putChar", "putString", "getInteger",
     "getFloat", "getBool", "getChar", "getString"};
  std::vector<SymbolType> param_symbol_types =
    {SymbolType::Integer, SymbolType::Float, SymbolType::Bool, SymbolType::Char,
    SymbolType::String, SymbolType::Integer, SymbolType::Float,
    SymbolType::Bool, SymbolType::Char, SymbolType::String};
  std::vector<ParameterType> param_types =
    {ParameterType::In, ParameterType::In, ParameterType::In, ParameterType::In,
    ParameterType::In, ParameterType::Out, ParameterType::Out, ParameterType::Out,
    ParameterType::Out, ParameterType::Out};

  for (int i = 0; i < built_in_names.size(); ++i) {
    SymbolRecord built_in_symbol;
    SymbolRecord param;

    // Build symbols.
    param.type = param_symbol_types[i];
    param.paramType = param_types[i];

    built_in_symbol.isGlobal = true; // Not considered global from an LLVM pov.
    built_in_symbol.built_in = true; // For cleaner symbol table insight.
    built_in_symbol.type = SymbolType::Procedure;
    if (i < built_in_impls.size())
      built_in_symbol.value = built_in_impls[i];
    built_in_symbol.params = { std::make_pair("arg", param) };

    scopeManager_.insert(built_in_names[i], built_in_symbol);
  }
}

// This function compares the most recently received token (stored in token_)
// with an expected token terminal from some production. Unexpected, but valid
// tokens are not to be specifically handled by this function, as that is the
// responsibility of the caller. Comments should also be entirely handled by
// the lexer, resulting in non-comment tokens appearing here. See
// https://github.com/domfarolino/compiler/issues/3.
bool Parser::CheckTokenType(TokenType expectedTokenType) {
  if (lexer_.isDone()) return false;

  // TODO(domfarolino): Handle invalid tokens, EOF, etc.

  if (token_.type == expectedTokenType) {
    token_ = lexer_.nextToken();
    return true;
  }

  return false;
}

// Queues a plain error message to the error queue, for later displaying.
void Parser::QueueError(std::string inErrorString) {
  errorQueue_.push("Line " + std::to_string(lexer_.lineNumber) + ": " +
                   inErrorString);
}

void Parser::QueueExpectedTokenError(std::string inErrorString) {
  errorQueue_.push("Line " + std::to_string(lexer_.lineNumber) + ": " +
                   inErrorString + ", but got: '" + token_.lexeme + "'");
}

// Queues a regular error, but stops the code generator from generating code.
void Parser::QueueSymbolError(std::string inErrorString) {
  CodeGen::EnterErrorState();
  QueueError(inErrorString);
}

// Queues a regular error, but stops the code generator from generating code.
void Parser::QueueTypeError(std::string inErrorString) {
  CodeGen::EnterErrorState();
  QueueError(inErrorString);
}

void Parser::FlushErrors() {
  std::cerr << "\033[1;31m";

  while (!errorQueue_.empty()) {
    std::cerr << errorQueue_.front() << std::endl;
    errorQueue_.pop();
  }

  std::cerr << "\033[0m";
}

// <program> ::= <program_header> <program_body> .
void Parser::Program() {
  // The ScopeManager is initialized with a global scope, so as long as there
  // is only one "program" in any given parse, we don't have to explicitly enter
  // a new scope.
  bool parsedHeader = ProgramHeader();
  if (!parsedHeader)
    QueueError("Error parsing program header");

  bool parsedBody = ProgramBody();
  if (!parsedBody) {
    QueueError("Error parsing program body");
    return;
  }

  if (symbolInsight_) scopeManager_.printTopScope();

  if (!CheckTokenType(TokenType::TPeriod))
    QueueError("Missing period ('.') at the end of program");
  else if (parsedHeader && parsedBody)
    std::cout << "Program compiled successfully" << std::endl;

  if (code_gen_) {
    CodeGen::Return(CodeGen::ProduceInteger(0));
    CodeGen::EndFunction();
  }

  CodeGen::PrintBitCode(program_name_);
}

// <program_header> ::= program <identifier> is
bool Parser::ProgramHeader() {
  if (!CheckTokenType(TokenType::TProgram)) {
    QueueExpectedTokenError("Expected 'program' in program header");
    return false;
  }

  // program
  if (!Identifier()) {
    QueueExpectedTokenError("Expected program identifier in program header");
    return false;
  }

  // program <identifier>
  if (!CheckTokenType(TokenType::TIs)) {
    QueueExpectedTokenError("Expected 'is' in program header");
    return false;
  }

  // program <identifier> is
  return true;
}

// <program_body> ::= (<declaration>;)* begin (<statement>;)* end program
bool Parser::ProgramBody() {
  int errorQueueSizeSnapshot = errorQueue_.size();
  while (Declaration()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after declaration");
      return false;
    }
  }

  // Declaration() isn't required, so there are two reasons it may return false:
  //  1.) The first token didn't match what we were looking for, so we have to
  //      assume the user provided no declaration. That's ok, we should only
  //      return false here if a given declaration _failed_ to parse part way
  //      through.
  //  2.) We were part-way through parsing a declaration and failed for some
  //      very bad reason, in which case the program body also failed to parse.
  //      We can distinguish between missing and invalid optional productions
  //      by taking a snapshot of the error queue.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (!CheckTokenType(TokenType::TBegin)) {
    QueueExpectedTokenError("Expected 'begin' in program body");
    return false;
  }

  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement");
      return false;
    }
  }

  // Same error checking as above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (!CheckTokenType(TokenType::TEnd) ||
      !CheckTokenType(TokenType::TProgram)) {
    QueueExpectedTokenError("Expected 'end program' after program body");
    return false;
  }

  return true;
}

// <identifier> ::= [a-zA-Z][a-zA-Z0-9_]*
bool Parser::Identifier() {
  // TODO(domfarolino): Can we modify this so that even if there is a valid
  // identifier that does not exist in an accessible symbol table, we return
  // false? Should experiment with this for code health.
  return CheckTokenType(TokenType::TIdentifier);
}

// <identifier> ::= [a-zA-Z][a-zA-Z0-9_]*
bool Parser::Identifier(std::string& id) {
  // TODO(domfarolino): Maybe if the token was not an identifier, we can check
  // to see if it is a reserved word and throw a more specific error, like "can
  // not use reserved word as identifier" or something.
  id = token_.lexeme; // This must come before CheckTokenType, as CheckTokenType
                      // advances the token stream by one token.
  return CheckTokenType(TokenType::TIdentifier);
}

// <string> :: = "[a-zA-Z0-9 _,;:.']*"
bool Parser::String(std::string& string_value, SymbolRecord& symbolRecord) {
  string_value = token_.lexeme;
  if (CheckTokenType(TokenType::TString)) {
    symbolRecord.type = SymbolType::String;
    return true;
  }

  return false;
}

// <char> ::= '[a-zA-Z0-9 _;:."]'
bool Parser::Char(std::string& char_value, SymbolRecord& symbolRecord) {
  char_value = token_.lexeme;
  if (CheckTokenType(TokenType::TChar)) {
    symbolRecord.type = SymbolType::String;
    return true;
  }

  return false;
}

// <name> ::= <identifier> [ [ <expression> ] ]
// We can get away without consuming |identifier|, however for error messages in
// Factor(), it is convenient to have the name of the symbol that may have
// caused the error.
bool Parser::Name(std::string& identifier, SymbolRecord& nameSymbol) {
  // Assert: nameSymbol == nullptr.

  // <name> is not required (see <factor>).
  if (!Identifier(identifier))
    return false;

  if (!scopeManager_.lookup(identifier)) {
    QueueSymbolError("Symbol '" + identifier + "' not found");
    return false;
  }

  // Assert: the nameSymbol exists; get it.
  nameSymbol = SymbolRecord(*scopeManager_.getSymbol(identifier));

  // <identifier>
  // TODO(domfarolino): [CODEGEN] We need to set |nameSymbol|'s Value* to a
  // reference of the variable we're interested in. This is either:
  //   - An element of an regular array. See the below todo interested in this.
  //   - An element of a reference array. See |    |    |        |     |    |.
  //   - A regular symbol reference (original AllocaInst*) (see second below todo).
  //   - A reference to a regular symbol reference (original AllocaInst* that
  //     represents a reference).
  if (CheckTokenType(TokenType::TLeftBracket)) {
    if (!nameSymbol.isArray) {
      QueueSymbolError("Cannot index into non-array name '" + identifier + "'");
      return false;
    }

    int errorQueueSizeSnapshot = errorQueue_.size();

    // The |nameSymbol| represents the RHS that we're interested in. If what
    // we're interested in is an array element, we must ensure |nameSymbol| does
    // not reflect array-ness copied from the original SymbolRecord.
    nameSymbol.isArray = false;

    // <identifier> [
    SymbolRecord expressionSymbol;
    if (!Expression(expressionSymbol)) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected expression after '[' in name");
      return false;
    }

    if (!IsArrayIndex(expressionSymbol)) {
      QueueTypeError("Array index must be an integer type, but got a " +
                     SymbolRecord::SymbolTypeToDebugString(expressionSymbol.type));
      return false;
    }

    // Assert: Since IsArrayIndex(expressionSymbol), |expressionSymbol|'s Value*
    // must be an integer type. See
    // http://llvm.org/doxygen/Type_8h_source.html#l00196.

    // TODO(domfarolino): [CODEGEN] The unmodified |nameSymbol| represents an
    // array, which we'll want to index into with |nameSymbol|'s Value* as the
    // variable, and |expressionSymbol|'s Value* as the index. The resulting
    // Value* will be a reference to the original array element. We'll want to
    // set |nameSymbol|'s Value* to the result of this index. There are two
    // cases here though:
    //   - |nameSymbol| is an array, and NOT an Out/InOut reference.
    //     Handle this indexing with CodeGen::IndexArray(<originalValue>, ...) ❌
    //   - |nameSymbol| is an array, AND either an Out/InOut reference. Handle
    //     this indexing with CodeGen::IndexArray(Load(<originalValue>, ...)
    //     because the Value* is itself a reference reference, so we want to do
    //     a single load first.                                                ❌
    // In both cases, we must ensure that |nameSymbol.paramType| = None to
    // prevent |nameSymbol| from ever being interpreted as a reference variable.

    // <identifier> [ <expression>
    if (!CheckTokenType(TokenType::TRightBracket)) {
      // TODO(domfarolino): Verify that we do not need this check:
      //if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected ']' after expression in name");
      return false;
    }

    // <identifier> "[" <expression> "]".
    // We're returning true here because the code below this block is for when
    // we _don't_ index.
    return true;
  }

  // TODO(domfarolino): [CODEGEN] As per the comment above the previous
  // condition block, if we're here, we did not index into an array name, so we
  // want to get |nameSymbol|'s Value* as a reference, as our final
  // destination. There are two cases:
  //   - |nameSymbol| is a regular symbol, NOT Out/InOut.
  //     Handled by just retrieving |nameSymbol|'s Value* (original AllocaInst*)
  //     member. This even works with an array name.        ✅
  //   - |nameSymbol| is a reference symbol, EITHER Out/InOut.
  //     Handled by performing a single load with CodeGen.  ❌
  // In both cases, we must ensure that |nameSymbol.paramType| = None to
  // prevent |nameSymbol| from ever being interpreted as a reference variable.

  // <identifier> (no <expression>!).
  return true;
}

// <declaration> ::= [ global ] <procedure_declaration> |
//                   [ global ] <variable_declaration>
bool Parser::Declaration() {
  std::string identifier;
  SymbolRecord symbolRecord;

  bool global = false;
  if (CheckTokenType(TokenType::TGlobal))
    global = true;

  symbolRecord.isGlobal = global;

  int errorQueueSizeSnapshot = errorQueue_.size();
  if (ProcedureDeclaration(identifier, symbolRecord))
    return true;
  else if (errorQueue_.size() == errorQueueSizeSnapshot &&
           VariableDeclaration(identifier, symbolRecord)) {

    // [CODEGEN].
    AbstractType variable_type = SymbolTypeToAbstractType(symbolRecord);
    symbolRecord.value = CodeGen::CreateVariable(variable_type, identifier,
                                                 global, symbolRecord.isArray,
                                                 symbolRecord.isArray ?
                                                 symbolRecord.array_length(): 0);

    // The caller of VariableDeclaration is responsible for inserting its
    // symbol into the current scope, hence why we have this conditional.
    if (!scopeManager_.insert(identifier, symbolRecord)) {
      QueueSymbolError("Symbol '" + identifier +
                       "' already exists, cannot redeclare");
      return false;
    }

    return true;
  }

  // At this point, there was either an error parsing one of them, or they both
  // did not exist. Either way, we return false so the caller knows it shouldn't
  // look for a semicolon. The caller will determine whether or not it should
  // continue parsing given the error queue size snapshot.
  if (global) {
    QueueExpectedTokenError("Expected procedure or variable declaration " +
                            std::string("after 'global'"));
  }

  return false;
}

// <statement> ::= <assignment_statement> |
//                 <if_statement>         |
//                 <loop_statement>       |
//                 <return_statement>     |
//                 <procedure_call>
bool Parser::Statement() {
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (IfStatement()) return true;
  else if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  if (LoopStatement()) return true;
  else if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  std::string identifier;
  bool validIdentifier = true;
  if (AssignmentStatement(identifier, validIdentifier))
    return true;
  else if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // Assert: There were no parsing errors specific to AssignmentStatement. In
  // other words, AssignmentStatement failed without queueing an error, and this
  // only happens when:
  //  1.) The next token was not a valid identifier, therefore
  //      AssignmentStatment and ProcedureCall will both return false with no
  //      queued error (because these productions are not mandatory).
  //  2.) The next token was a valid identifier, but the symbol associated with
  //      it was type procedure, and therefore ProcedureCall must handle it.
  if (validIdentifier && ProcedureCall(identifier))
    return true;

  return ReturnStatement();
}

// <assignment_statement> ::= <destination> := <expression>
bool Parser::AssignmentStatement(std::string& identifier,
                                 bool& validIdentifier) {
  SymbolRecord destinationSymbol;
  // <assignment_statement> is not required, as up the chain it is part of a
  // (<statement>;)*, so if we fail to find the first token we cannot queue
  // an error quite yet.
  if (!Destination(identifier, destinationSymbol, validIdentifier))
    return false;

  // At this point, |destinationSymbol|'s Value* has been set by |Destination|.
  // Can we assert a non-nullptr here?

  // <destination>
  if (!CheckTokenType(TokenType::TColonEq)) {
    QueueExpectedTokenError("Expected ':=' after destination in assignment " +
                            std::string("statement"));
    return false;
  }

  int errorQueueSizeSnapshot = errorQueue_.size();
  // <destination> :=
  SymbolRecord expressionSymbol;
  if (!Expression(expressionSymbol)) {
    // Only queue an error if Expression didn't already (sometimes it does!).
    if (errorQueueSizeSnapshot == errorQueue_.size()) {
      QueueExpectedTokenError("Expected expression for destination in " +
                              std::string("assignment statement"));

    }

    return false;
  }

  // Both-are-or-are-not-arrays check.
  if (destinationSymbol.isArray != expressionSymbol.isArray) {
    QueueTypeError("Assignment statement target and expression array-ness " +
                   std::string("must match"));
    return false;
  }

  // If both are arrays, do a static length check.
  // TODO(domfarolino): Factor this out
  // https://github.com/domfarolino/compiler/issues/26.
  if (destinationSymbol.isArray) {
    int lhs_size = destinationSymbol.array_length(),
        rhs_size = expressionSymbol.array_length();
    if (lhs_size != rhs_size) {
      QueueTypeError("Assignment statement target and expression arrays must " +
                     std::string("be of the same length"));
      return false;
    }
  }

  // TODO(domfarolino): Factor this out
  // https://github.com/domfarolino/compiler/issues/26.
  if (destinationSymbol.type != expressionSymbol.type && !(
       (destinationSymbol.type == SymbolType::Integer &&
        expressionSymbol.type == SymbolType::Float) ||
       (destinationSymbol.type == SymbolType::Float &&
        expressionSymbol.type == SymbolType::Integer) ||
       (destinationSymbol.type == SymbolType::Integer &&
        expressionSymbol.type == SymbolType::Bool) ||
       (destinationSymbol.type == SymbolType::Bool &&
        expressionSymbol.type == SymbolType::Integer)
     )) {
    QueueTypeError("Assignment statement target (" +
                   SymbolRecord::SymbolTypeToDebugString(destinationSymbol.type) +
                   ") is not compatible with expression (" +
                   SymbolRecord::SymbolTypeToDebugString(expressionSymbol.type) +
                   ")");

    return false;
  }

  // [CODEGEN].
  // The type-checking has passed, and we have to CodeGen an assignment now. The
  // LHS's Value* is the AllocaInst* reference to the LHS symbol, and the RHS is
  // either the Value* representing the literal RHS value is
  // |expressionSymbol.is_literal| is true, or the AllocaInst* reference to the
  // RHS symbol otherwise. Therefore all possible combinations of assignments
  // are dependent on the RHS possibilities, which are mostly evened out by
  // ::Name(). In this case, the RHS |expressionSymbol| variants we'll see here
  // are one of the following:
  //  - Symbol record whose |is_literal| flag is set, and Value* represents a
  //    literally-created value. In this case, we have to
  //    CodeGen::Assign(..., RHSValue).                             ✅
  //  - Symbol record whose Value* is an AllocaInst* representing the immediate
  //    address of some variable value. This is the case even for reference
  //    variables, since ::Name() will perform a single load on reference
  //    variable AllocaInst*s (hence the "evening out the combinations"). In
  //    this case, we have to CodeGen::Assign(..., Load(RHSValue)). ✅

  // As mentioned above, this complements whatever Expression() does, since we
  // load |is_literal| values here when necessary.
  if (expressionSymbol.is_literal == false) {
    expressionSymbol.value = CodeGen::Load(expressionSymbol.value);
    // I guess we don't really need to set |is_literal| here?
  }

  CodeGen::Assign(destinationSymbol.value, expressionSymbol.value);

  // <destination> := <expression>
  return true;
}

// <destination> ::= <identifier> [ [ <expression> ] ]
bool Parser::Destination(std::string& identifier,
                         SymbolRecord& destinationSymbol,
                         bool& validIdentifier) {
  // <destination> is not required (see <assignment_statement>).
  if (!Identifier(identifier)) {
    validIdentifier = false;
    return false;
  }

  if (!scopeManager_.lookup(identifier)) {
    QueueSymbolError("Symbol '" + identifier + "' not found");
    return false;
  }

  // Create a copy of the symbol we're referencing. The symbol we're "returning"
  // from this function should be this _copy_, because we may be modifying the
  // the SymbolRecord, and its Value* (as a result of indexing, for example).
  destinationSymbol = SymbolRecord(*scopeManager_.getSymbol(identifier));

  // Procedures cannot be destinations. If the author was trying to assign or
  // index into a procedure, then ProcedureCall will catch that and queue an
  // appropriate error. We don't queue the error here, because we give
  // ProcedureCall a chance to check things out.
  if (destinationSymbol.type == SymbolType::Procedure)
    return false;

  // <identifier>
  // TODO(domfarolino): [CODEGEN] We need to set |destinationSymbol|'s Value*
  // to a reference of the variable we're assigning, so we can actually complete
  // the assignment in AssignmentStatement. The destination can be one of four
  // things:
  //   - ArrayVariable[i] member
  //   - ReferenceArrayVariable[i] member
  //   - Variable (aka AllocaInst*)
  //   - Reference variable (aka AllocaInst* to another AllocaInst*)
  // The first two are handled in the below block, and the last two are handled
  // _beneath_ the below block.
  if (CheckTokenType(TokenType::TLeftBracket)) {
    if (!destinationSymbol.isArray) {
      QueueSymbolError("Cannot index into non-array destination '" +
                       identifier + "'");
      return false;
    }

    // The |destinationSymbol| represents the LHS of what we're assigning. If
    // what we're assigning is an array element that we're indexing, we must
    // ensure |destinationSymbol| does not reflect array-ness copied from the
    // original SymbolRecord.
    destinationSymbol.isArray = false;

    int errorQueueSizeSnapshot = errorQueue_.size();

    // <identifier> [
    SymbolRecord expressionSymbol;
    if (!Expression(expressionSymbol)) {
      if (errorQueueSizeSnapshot == errorQueue_.size()) {
        QueueExpectedTokenError("Expected expression after '[' in assignment " +
                                std::string("statement"));
      }

      return false;
    }

    if (!IsArrayIndex(expressionSymbol)) {
      QueueTypeError("Array index must be an integer type, but got a " +
                      SymbolRecord::SymbolTypeToDebugString(expressionSymbol.type));
      return false;
    }

    // Assert: Since IsArrayIndex(expressionSymbol), |expressionSymbol|'s Value*
    // must be an integer type. See
    // http://llvm.org/doxygen/Type_8h_source.html#l00196.

    // TODO(domfarolino): [CODEGEN] The unmodified |destinationSymbol|
    // represents an array, which we'll want to index into with
    // |destinationSymbol|'s Value* as the variable, and |expressionSymbol|'s
    // Value* as the index. The resulting Value* will be a reference to the
    // original array element. We'll want to set |destinationSymbol|'s Value* to
    // the result of this index.
    // There are two cases here though:
    //   - |destinationSymbol| is an array, and NOT an Out/InOut reference.
    //     Handle this indexing with CodeGen::IndexArray(<originalValue>, ...) ❌
    //   - |destinationSymbol| is an array, AND either an Out/InOut reference.
    //     Handle this indexing with
    //     CodeGen::IndexArray(Load(<originalValue>, ...) because the Value* is
    //     itself a reference reference, so we want to do a single load first. ❌
    // In both cases, we must ensure that |destinationSymbol.paramType| = None
    // to prevent |destinationSymbol| from ever being interpreted as a reference
    // variable.

    // <identifier> [ <expression>
    if (!CheckTokenType(TokenType::TRightBracket)) {
      QueueExpectedTokenError("Expected ']' after expression in assignment " +
                              std::string("destination"));
      return false;
    }

    // <identifier> "[" <expression> "]".
    // We're returning true here because the code below this block is for when
    // we _don't_ index.
    return true;
  }

  // [CODEGEN] As per the comment above the previous condition block, if we're
  // here, we did not index into an array symbol, so we want to get
  // |destinationSymbol|'s Value* as a reference, as our final destination.
  // There are two cases:
  //   - |destinationSymbol| is a regular primitive, NOT Out/InOut.
  //     Handled by just retrieving |destinationSymbol|'s Value* (original
  //     AllocaInst*) member. This even works with an array destination.   ✅
  //   - |destinationSymbol| is a reference primitive, either In/OutOut.
  //     Handled by performing a load with CodeGen. (Not implemented yet). ❌
  // In both cases, we must ensure that |destinationSymbol.paramType| = None
  // to prevent |destinationSymbol| from ever being interpreted as a reference
  // variable.

  // <identifier> (no <expression>!).
  return true;
}

/////////////////////////////// Expression ////////////////////////////////////

// <expression> ::= [ not ] <arithOp> <expressionPrime>
// Expression is not always required, so this function will only queue errors
// when something bad happens, not just for non-existence.
bool Parser::Expression(SymbolRecord& expression) {
  bool notIsPresent = false;
  if (CheckTokenType(TokenType::TNot))
    notIsPresent = true;

  // [ not ]
  if (!ArithOp(expression))
    return false;

  if (notIsPresent && (expression.type != SymbolType::Bool &&
                       expression.type != SymbolType::Integer)) {
    QueueTypeError("[Expression]: operator 'not' cannot be affixed to " +
                   SymbolRecord::SymbolTypeToDebugString(expression.type) +
                   "s; must only be applied to boolean-equivalent types");
    return false;
  }

  // TODO(domfarolino): Figure out how |not| should work.

  // [ not ] <arithOp>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ExpressionPrime(expression)) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression. Expected valid syntax");
    return false;
  }

  // [ not ] <arithOp> <expressionPrime>
  return true;
}

// <expressionPrime> ::= & <arithOp> <expressionPrime> |
//                       | <arithOp> <expressionPrime> |
//                       ε
bool Parser::ExpressionPrime(SymbolRecord& leftArithOp) {
  // Capture the potential TAmp or TOr.
  TokenType token_type = token_.type;
  if (CheckTokenType(TokenType::TAmp) || CheckTokenType(TokenType::TOr)) {
    // & or |
    SymbolRecord rightArithOp;
    SymbolType &left_arithop_type = leftArithOp.type,
               &right_arithop_type = rightArithOp.type;
    if (!ArithOp(rightArithOp))
      return false;

    if (!(left_arithop_type == SymbolType::Integer &&
          right_arithop_type == SymbolType::Integer) &&
        !(left_arithop_type == SymbolType::Bool &&
          right_arithop_type == SymbolType::Bool)) {
      QueueTypeError("[Expression]: " +
                     SymbolRecord::SymbolTypeToDebugString(left_arithop_type) +
                     " and " +
                     SymbolRecord::SymbolTypeToDebugString(right_arithop_type) +
                     " cannot be bitwise or logical operands");
      return false;
    }

    if (leftArithOp.isArray && rightArithOp.isArray &&
        leftArithOp.array_length() != rightArithOp.array_length()) {
      QueueTypeError("[Expression]: Arrays must be the same length to be " +
                     std::string("bitwise/logical operands"));
      return false;
    }

    // Regardless of what we're dealing with, any non-literal scalars must be
    // loaded before being used.
    if (leftArithOp.isArray == false && leftArithOp.is_literal == false) {}
      //leftArithOp.value = CodeGen::Load(leftArithOp.value);
    if (rightArithOp.isArray == false && rightArithOp.is_literal == false) {}
      //rightArithOp.value = CodeGen::Load(rightArithOp.value);

    // If we're at all dealing with arrays, the CodeGen module's returned value
    // will be the non-LOADed address so that we may index into the array later.
    // This value is not a literal value, so we must be sure to communicate that
    // here. Otherwise, if we're dealing with non-arrays, the CodeGen module's
    // returned value will be a literal value representing the result of the
    // operation.
    if (leftArithOp.isArray == false && rightArithOp.isArray == false) {
      // No arrays.
      /*
      leftArithOp.value = (token_type == TokenType::TAmp) ?
                          CodeGen::AndIntegers(leftArithOp.value,
                                               rightArithOp.value):
                          CodeGen::OrIntegers(leftArithOp.value,
                                              rightArithop.value);
      */
      leftArithOp.is_literal = true;
    } else {
      // At least one array in the mix.
      // Non-literal scalars have already been loaded.
      /*
      leftArithOp.value = CodeGen::BitwiseArrayComboImpl(
                                                 leftArithOp.value,
                                                 rightArithOp.value,
                                                 token_type == TokenType::TAmp);
      */

      // Whenever we make a "new" array, the bounds must always be set so that
      // the length can be computed properly in the future.
      if (leftArithOp.isArray == false) {
        leftArithOp.lowerBound = rightArithOp.lowerBound;
        leftArithOp.upperBound = rightArithOp.upperBound;
      }

      leftArithOp.isArray = true;
      leftArithOp.is_literal = false;
    }

    // & or | <arithOp>
    return ExpressionPrime(leftArithOp);
  }

  // ε
  return true;
}

//////////////////////////// End Expression ////////////////////////////////

/////////////////////////////// ArithOp ////////////////////////////////////

// <arithOp> ::= <relation> <arithOpPrime>
bool Parser::ArithOp(SymbolRecord& arithOp) {
  if (!Relation(arithOp))
    return false;

  // <relation>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ArithOpPrime(arithOp)) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression operator. Expected " +
                              std::string("valid syntax"));
    return false;
  }

  // <relation> <arithOpPrime>
  return true;
}

// <arithOpPrime> ::= + <relation> <arithOpPrime> |
//                    - <relation> <arithOpPrime> | ε
bool Parser::ArithOpPrime(SymbolRecord& leftRelation) {
  // Capture the potential TPlus or TMinus.
  TokenType token_type = token_.type;
  if (CheckTokenType(TokenType::TPlus) || CheckTokenType(TokenType::TMinus)) {
    // + or -
    SymbolRecord rightRelation;
    SymbolType &leftRelationType = leftRelation.type,
               &rightRelationType = rightRelation.type;
    if (!Relation(rightRelation))
      return false;

    // At this point, both |leftRelationType| and |rightRelationType| must be
    // some combination of Integers and Floats for a valid arith op.
    if ((leftRelationType != SymbolType::Integer &&
         leftRelationType != SymbolType::Float) ||
        (rightRelationType != SymbolType::Integer &&
         rightRelationType != SymbolType::Float)) {
      QueueTypeError("[ArithOp]: " +
                     SymbolRecord::SymbolTypeToDebugString(leftRelationType) +
                     " and " +
                     SymbolRecord::SymbolTypeToDebugString(rightRelationType) +
                     " cannot be added or subtracted");
      return false;
    }

    if (leftRelation.isArray && rightRelation.isArray &&
        leftRelation.array_length() != rightRelation.array_length()) {
      QueueTypeError("[ArithOp]: Arrays must be the same length to be added " +
                     std::string("or subtracted"));
      return false;
    }

    // Assert: |leftRelationType|  == (Integer || Float) &&
    //         |rightRelationType| == (Integer || Float).

    // Regardless of what we're dealing with, any non-literal scalars must be
    // loaded before being used.
    if (leftRelation.isArray == false && leftRelation.is_literal == false) {}
      //leftRelation.value = CodeGen::Load(leftRelation.value);
    if (rightRelation.isArray == false && rightRelation.is_literal == false) {}
      //rightRelation.value = CodeGen::Load(rightRelation.value);

    if (leftRelation.isArray == false && rightRelation.isArray == false) {
      // Not dealing with arrays.

      // Simplest case:
      if (leftRelation.type == rightRelation.type) {
        if (token_type == TokenType::TPlus) {
          /*
          leftRelation.value = (leftRelation.type == SymbolType::Integer) ?
                               CodeGen::AddIntegers(leftRelation.value,
                                                    rightRelation.value):
                               CodeGen::AddFloats(leftRelation.value,
                                                  rightRelation.value);
          */
        } else {
          /*
          leftRelation.value = (leftRelation.type == SymbolType::Integer) ?
                               CodeGen::SubtractIntegers(leftRelation.value,
                                                    rightRelation.value):
                               CodeGen::SubtractFloats(leftRelation.value,
                                                  rightRelation.value);
          */
        }
      } else {
        // We are definitely promoting to a float here.

        if (leftRelation.type != SymbolType::Float) {}
          // leftRelation.value = CodeGen::CastIntegerToFloat(leftRelation.value);
        else {}
          // rightRelation.value = CodeGen::CastIntegerToFloat(rightRelation.value);

        if (token_type == TokenType::TPlus) {
          //leftRelation.value = CodeGen::AddFloats(leftRelation.value,
          //                                        rightRelation.value);
        } else {
          //leftRelation.value = CodeGen::SubtractFloats(leftRelation.value,
          //                                             rightRelation.value);
        }
        leftRelation.type = SymbolType::Float;
      }
      // CodeGen module produces literal values when not dealing with arrays.
      leftRelation.is_literal = true;
    } else {
      // At least one array in the mix.
      // Non-literal scalars have already been loaded.
      /*
      leftRelation.value = CodeGen::AddSubtractArrayComboImpl(
                                                leftRelation.value,
                                                rightRelation.value,
                                                token_type == TokenType::TPlus);
      */

      // Make |leftRelation| aware of potential float promotion.
      if (leftRelation.type == SymbolType::Float ||
          rightRelation.type == SymbolType::Float) {
        leftRelation.type = SymbolType::Float;
      }

      // Whenever we make a "new" array, the bounds must always be set so that
      // the length can be computed properly in the future.
      if (leftRelation.isArray == false) {
        leftRelation.lowerBound = rightRelation.lowerBound;
        leftRelation.upperBound = rightRelation.upperBound;
      }

      leftRelation.isArray = true;
      leftRelation.is_literal = false;
    }

    // + or - <relation>
    return ArithOpPrime(leftRelation);
  }

  // ε
  return true;
}

///////////////////////////// End ArithOp //////////////////////////////////

////////////////////////////// Relation ////////////////////////////////////

// <relation> ::= <term> <relationPrime>
bool Parser::Relation(SymbolRecord& relation) {
  if (!Term(relation))
    return false;

  // <term>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!RelationPrime(relation)) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression relation. Expected valid syntax");
    return false;
  }

  // <term> <relationPrime>
  return true;
}

// <relationPrime> ::= < <term> <relationPrime> |
//                    >= <term> <relationPrime> |
//                    <= <term> <relationPrime> |
//                     > <term> <relationPrime> |
//                    == <term> <relationPrime> |
//                    != <term> <relationPrime> | ε
bool Parser::RelationPrime(SymbolRecord& leftTerm) {
  // Capture the potential relational operator.
  TokenType token_type = token_.type;
  // TODO(domfarolino): Maybe factor this out. See
  // https://github.com/domfarolino/compiler/issues/26.
  if (CheckTokenType(TokenType::TLessThan) ||
      CheckTokenType(TokenType::TGreaterThanEq) ||
      CheckTokenType(TokenType::TLessThanEq) ||
      CheckTokenType(TokenType::TGreaterThan) ||
      CheckTokenType(TokenType::TCompareEq) ||
      CheckTokenType(TokenType::TNotEq)) {
    // < or >= or ... or !=
    SymbolRecord rightTerm;
    SymbolType &leftTermType = leftTerm.type,
               &rightTermType = rightTerm.type;
    if (!Term(rightTerm))
      return false;

    if (leftTerm.isArray || rightTerm.isArray) {
      QueueTypeError("[Relation]: The relational operators are not " +
                     std::string("implemented for arrays at this time"));
      return false;
    }

    // At this point, both |leftTermType| and |rightTermType| must be some
    // combination of the following types for a valid relation:
    // Comparison types allowed:
    //  - Integer and {Integer, Float, Bool} ✅
    //  - Bool and {Bool, Integer}           ✅
    //  - Float and {Float, Integer}         ✅
    //  - Char and {Char}                    ✅
    //  - Float and AboveSet ∪ {Bool}        ❌ (see issue#25).
    if ((leftTermType == SymbolType::Integer &&
          (rightTermType != SymbolType::Integer &&
           rightTermType != SymbolType::Float &&
           rightTermType != SymbolType::Bool)) ||
        (leftTermType == SymbolType::Bool &&
          (rightTermType != SymbolType::Bool &&
           rightTermType != SymbolType::Integer)) ||
        (leftTermType == SymbolType::Float &&
          (rightTermType != SymbolType::Float &&
           rightTermType != SymbolType::Integer)) ||
        (leftTermType == SymbolType::Char &&
          rightTermType != SymbolType::Char) ||
        leftTermType == SymbolType::String ||
        leftTermType == SymbolType::Procedure) {
      QueueTypeError("[Relation]: " +
                     SymbolRecord::SymbolTypeToDebugString(leftTermType) +
                     " and " +
                     SymbolRecord::SymbolTypeToDebugString(rightTermType) +
                     " cannot be compared");
      return false;
    }

    // Regardless of what we're dealing with, any non-literal scalars must be
    // loaded before being used.
    if (leftTerm.isArray == false && leftTerm.is_literal == false) {}
      //leftTerm.value = CodeGen::Load(leftTerm.value);
    if (rightTerm.isArray == false && rightTerm.is_literal == false) {}
      //rightTerm.value = CodeGen::Load(right.value);

    if (leftTerm.type == SymbolType::Float ||
        rightTerm.type == SymbolType::Float) {
      // Attempt to promote |leftTerm.value| or |rightTerm.value|.

      if (leftTerm.type == SymbolType::Float) {}
        //leftTerm.value = CodeGen::CastIntegerToFloat(leftTerm.value);
      if (rightTerm.type == SymbolType::Float) {}
        //rightTerm.value = CodeGen::CastIntegerToFloat(rightTerm.value);

      if (token_type == TokenType::TLessThan) {}
        //leftTerm.value = CodeGen::LessThanFloats(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TGreaterThanEq) {}
        //leftTerm.value = CodeGen::GreaterThanOrEqualFloats(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TLessThanEq) {}
        //leftTerm.value = CodeGen::LessThanOrEqualFloats(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TGreaterThan) {}
        //leftTerm.value = CodeGen::GreaterThanFloats(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TCompareEq) {}
        //leftTerm.value = CodeGen::EqualFloats(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TNotEq) {}
        //leftTerm.value = CodeGen::NotEqualFloats(leftTerm.value, rightTerm.value);
    } else {
      // We're only dealing with integers! This is nice :).

      if (token_type == TokenType::TLessThan) {}
        //leftTerm.value = CodeGen::LessThanIntegers(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TGreaterThanEq) {}
        //leftTerm.value = CodeGen::GreaterThanOrEqualIntegers(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TLessThanEq) {}
        //leftTerm.value = CodeGen::LessThanOrEqualIntegers(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TGreaterThan) {}
        //leftTerm.value = CodeGen::GreaterThanIntegers(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TCompareEq) {}
        //leftTerm.value = CodeGen::EqualIntegers(leftTerm.value, rightTerm.value);
      else if (token_type == TokenType::TNotEq) {}
        //leftTerm.value = CodeGen::NotEqualIntegers(leftTerm.value, rightTerm.value);
    }

    leftTermType = SymbolType::Bool;
    leftTerm.is_literal = true;

    // < or >= or ... or != <term>
    return RelationPrime(leftTerm);
  }

  // ε
  return true;
}

//////////////////////////// End Relation //////////////////////////////////

//////////////////////////////// Term //////////////////////////////////////

// <term> ::= <factor> <termPrime>
bool Parser::Term(SymbolRecord& term) {
  if (!Factor(term))
    return false;

  // <factor>
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!TermPrime(term)) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Error parsing expression term. Expected valid syntax");
    return false;
  }

  // <factor> <termPrime>
  return true;
}

// <termPrime> ::= * <factor> <termPrime> |
//                 / <factor> <termPrime> | ε
bool Parser::TermPrime(SymbolRecord& leftFactor) {
  // Capture the potential TMultiply or TDivide.
  TokenType token_type = token_.type;
  if (CheckTokenType(TokenType::TMultiply) ||
      CheckTokenType(TokenType::TDivide)) {
    // * or /
    SymbolRecord rightFactor;
    SymbolType &leftFactorType = leftFactor.type,
               &rightFactorType = rightFactor.type;
    if (!Factor(rightFactor))
      return false;

    // At this point, both |leftFactorType| and |rightFactorType| must be some
    // combination of Integers and Floats for a valid term.
    // TODO(domfarolino): Factor this out.
    // https://github.com/domfarolino/compiler/issues/26.
    if ((leftFactorType != SymbolType::Integer &&
         leftFactorType != SymbolType::Float) ||
        (rightFactorType != SymbolType::Integer &&
         rightFactorType != SymbolType::Float)) {
      QueueTypeError("[Term]: " +
                     SymbolRecord::SymbolTypeToDebugString(leftFactorType) +
                     " and " +
                     SymbolRecord::SymbolTypeToDebugString(rightFactorType) +
                     " cannot be multiplied or divided");
      return false;
    }

    if (leftFactor.isArray && rightFactor.isArray &&
        leftFactor.array_length() != rightFactor.array_length()) {
      QueueTypeError("[Term]: Arrays must be the same length to be " +
                     std::string("multiplied or divided"));
      return false;
    }

    // Assert: |leftFactorType|  == (Integer || Float) &&
    //         |rightFactorType| == (Integer || Float).

    // Regardless of what we're dealing with, any non-literal scalars must be
    // loaded before being used.
    if (leftFactor.isArray == false && leftFactor.is_literal == false) {}
      //leftFactor.value = CodeGen::Load(leftFactor.value);
    if (rightFactor.isArray == false && rightFactor.is_literal == false) {}
      //rightFactor.value = CodeGen::Load(rightFactor.value);

    if (leftFactor.isArray == false && rightFactor.isArray == false) {
      // Not dealing with arrays.

      // Simplest case:
      if (leftFactor.type == rightFactor.type) {
        if (token_type == TokenType::TMultiply) {
          /*
          leftFactor.value = (leftFactor.type == SymbolType::Integer) ?
                               CodeGen::MultiplyIntegers(leftFactor.value,
                                                         rightFactor.value):
                               CodeGen::MultiplyFloats(leftFactor.value,
                                                       rightFactor.value);
          */
        } else {
          /*
          leftFactor.value = (leftFactor.type == SymbolType::Integer) ?
                               CodeGen::DivideIntegers(leftFactor.value,
                                                       rightFactor.value):
                               CodeGen::DivideFloats(leftFactor.value,
                                                     rightFactor.value);
          */
        }
      } else {
        // We are definitely promoting to a float here.

        if (leftFactor.type != SymbolType::Float) {}
          // leftFactor.value = CodeGen::CastIntegerToFloat(leftFactor.value);
        else {}
          // rightFactor.value = CodeGen::CastIntegerToFloat(rightFactor.value);

        if (token_type == TokenType::TMultiply) {
          //leftFactor.value = CodeGen::Multiplyloats(leftFactor.value,
          //                                          rightFactor.value);
        } else {
          //leftFactor.value = CodeGen::DivideFloats(leftFactor.value,
          //                                         rightFactor.value);
        }
        leftFactor.type = SymbolType::Float;
      }
      // CodeGen module produces literal values when not dealing with arrays.
      leftFactor.is_literal = true;
    } else {
      // At least one array in the mix.
      // Non-literal scalars have already been loaded.
      /*
      leftFactor.value = CodeGen::MultiplyDivideArrayComboImpl(
                                            leftFactor.value,
                                            rightFactor.value,
                                            token_type == TokenType::TMultiply);
      */

      // Make |leftFactor| aware of potential float promotion.
      if (leftFactor.type == SymbolType::Float ||
          rightFactor.type == SymbolType::Float) {
        leftFactor.type = SymbolType::Float;
      }

      // Whenever we make a "new" array, the bounds must always be set so that
      // the length can be computed properly in the future.
      if (leftFactor.isArray == false) {
        leftFactor.lowerBound = rightFactor.lowerBound;
        leftFactor.upperBound = rightFactor.upperBound;
      }

      leftFactor.isArray = true;
      leftFactor.is_literal = false;
    }

    // * or / <factor>
    return TermPrime(leftFactor);
  }

  // ε
  return true;
}

////////////////////////////// End Term ////////////////////////////////////

// <factor> ::= ( <expression> ) |
//              [ - ] <name> |
//              [ - ] <number> |
//              <string> |
//              <char> |
//              true |
//              false
bool Parser::Factor(SymbolRecord& symbolRecord) {
  bool minus = false;
  if (CheckTokenType(TokenType::TMinus))
    minus = true;

  int errorQueueSizeSnapshot = errorQueue_.size();

  // Name is not required, but if it queued an error, we shouldn't continue.

  // We can get away without passing |identifier| in, however for error
  // messages, it is convenient to have the name of the symbol that may have
  // caused the error.
  std::string identifier;
  if (Name(identifier, symbolRecord)) {
    // Assert: symbolRecord is a copy of a valid Symbol.

    // TODO(domfarolino): Factor this (and below copy of this) out into the Type
    // checking module https://github.com/domfarolino/compiler/issues/26.
    // Minus (-) cannot be applied to non-{integer, float}s.
    if (minus && (symbolRecord.type != SymbolType::Integer &&
                  symbolRecord.type != SymbolType::Float)) {
      QueueTypeError("Minus (-) cannot be applied to symbol '" + identifier +
                     "' (" +
                     SymbolRecord::SymbolTypeToDebugString(symbolRecord.type) +
                     ") which is not of type integer or float");
      return false;
    }

    // Minus should not apply to array-types currently. See
    // https://github.com/domfarolino/compiler/issues/35.
    if (minus && symbolRecord.isArray) {
      QueueTypeError("Minus (-) cannot be applied to array symbols. This may " +
                     std::string("change however, see ") +
                     std::string("https://github.com/domfarolino/compiler/issues/35"));
      return false;
    }

    // [CODEGEN].
    if (minus) {
      symbolRecord.value = (symbolRecord.type == SymbolType::Integer) ?
                      CodeGen::NegateInteger(CodeGen::Load(symbolRecord.value)):
                      CodeGen::NegateFloat(CodeGen::Load(symbolRecord.value));
      symbolRecord.is_literal = true;
    }
    return true;
  } else if (errorQueue_.size() > errorQueueSizeSnapshot) {
    return false;
  }

  std::string number;
  if (Number(number, symbolRecord)) {
    double number_val = atof(number.c_str());
    // [CODEGEN].
    symbolRecord.value = (symbolRecord.type == SymbolType::Integer) ?
                         CodeGen::ProduceInteger(number_val):
                         CodeGen::ProduceFloat(number_val);
    if (minus) {
      symbolRecord.value = (symbolRecord.type == SymbolType::Integer) ?
                           CodeGen::NegateInteger(symbolRecord.value):
                           CodeGen::NegateFloat(symbolRecord.value);
    }

    symbolRecord.is_literal = true;
    return true;
  }

  if (CheckTokenType(TokenType::TLeftParen)) {
    // (
    if (!Expression(symbolRecord)) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected expression after '(' in factor");
      return false;
    }

    // ( <expression>
    if (!CheckTokenType(TokenType::TRightParen)) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected ')' after expression in factor");
      return false;
    }

    // Minus (-) cannot be applied to non-{integer, float}s.
    if (minus && (symbolRecord.type != SymbolType::Integer &&
                  symbolRecord.type != SymbolType::Float)) {
      QueueTypeError("Minus (-) cannot be applied to an '( expression )' of type " +
                     SymbolRecord::SymbolTypeToDebugString(symbolRecord.type) +
                     " which is not an integer or float");
      return false;
    }

    // Minus should not apply to array-types currently. See
    // https://github.com/domfarolino/compiler/issues/35.
    if (minus && symbolRecord.isArray) {
      QueueTypeError("Minus (-) cannot be applied to an '( expression )' " +
                     std::string("that is an array. This may change ") + 
                     std::string("however, see https://github.com/domfarolino/compiler/issues/35"));
      return false;
    }

    // TODO(domfarolino): [CODEGEN] If |minus|, we know we're dealing with an
    // integer or float, and we should CodeGen::NegateX |symbolRecord|'s Value*
    // appropriately.

    // ( <expression> )
    return true;
  }

  if (minus) {
    QueueTypeError("Minus cannot be applied to strings, bools, or chars.");
    return false;
  }

  // Any successful branch beyond this point will result in |symbolRecord|'s
  // Value* member being a literal, so we should set its |is_literal| flag
  // appropriately.
  symbolRecord.is_literal = true;

  // These help us grab the corresponding bool, string, or char vals:
  TokenType token_type = token_.type;
  std::string string_value;
  if (CheckTokenType(TokenType::TTrue) ||
      CheckTokenType(TokenType::TFalse)) { // Boolean types.
    symbolRecord.type = SymbolType::Bool;

    symbolRecord.value = CodeGen::ProduceBool(token_type == TokenType::TTrue ?
                                              true: false);
    return true;
  } else if (String(string_value, symbolRecord)) { // String type.
    symbolRecord.type = SymbolType::String;

    // Assert: |string_value.length() >= 2|.
    // TODO(domfarolino): We probably just want the lexer to not save the quotes
    // so we don't have to do substring acrobatics here.
    symbolRecord.value = CodeGen::ProduceString(string_value.substr(
                                                  1, string_value.length() - 2)
                                               );
    return true;
  } else if (Char(string_value, symbolRecord)) { // Char type.
    symbolRecord.type = SymbolType::Char;
    symbolRecord.value = CodeGen::ProduceChar(string_value[1]);
    return true;
  }

  return false;
}

// <loop_statement> ::= for ( <assignment_statement> ; <expression> )
//                        ( <statement> ; )*
//                      end for
bool Parser::LoopStatement() {
  // <loop_statement> is not required; cannot queue error if first token is not
  // found.
  if (!CheckTokenType(TokenType::TFor))
    return false;

  // for
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' after 'for' in for loop");
    return false;
  }

  // for (
  int errorQueueSizeSnapshot = errorQueue_.size();
  std::string identifier;
  bool validIdentifier = true;
  // Only queue an error if AssignmentStatement did not (sometimes it does!).
  if (!AssignmentStatement(identifier, validIdentifier) &&
      errorQueueSizeSnapshot == errorQueue_.size()) {
    QueueExpectedTokenError("Expected assignment statement after '(' in for loop");
    return false;
  }

  // for ( <assignment_statement>
  if (!CheckTokenType(TokenType::TSemicolon)) {
    QueueExpectedTokenError("Expected ';' after assignment statement in for loop");
    return false;
  }

  // for ( <assignment_statement> ;
  SymbolRecord expressionSymbol;
  if (!Expression(expressionSymbol)) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected expression after ';' in for loop");
    return false;
  }

  if (!IsBooleanEquivalent(expressionSymbol)) {
    QueueTypeError("Loop condition must be a boolean or boolean equivalent, " +
                   std::string("not ") +
                   SymbolRecord::SymbolTypeToDebugString(expressionSymbol.type) +
                   (expressionSymbol.isArray ? " array": ""));
    return false;
  }

  // [CODEGEN].
  CodeGen::For();
  if (expressionSymbol.is_literal == false)
    expressionSymbol.value = CodeGen::Load(expressionSymbol.value);
  CodeGen::ForCondition(expressionSymbol.value);

  // for ( <assignment_statement> ; <expression>
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after expression in for loop statement");
    return false;
  }

  // for ( <assignment_statement> ; <expression> )
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement in for loop");
      return false;
    }
  }

  // Same error checking as above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // for ( <assignment_statement> ; <expression> ) ( <statement> ; )*
  if (!CheckTokenType(TokenType::TEnd) || !CheckTokenType(TokenType::TFor)) {
    QueueExpectedTokenError("Expected 'end for' after for loop statement");
    return false;
  }

  CodeGen::EndFor();

  // for ( <assignment_statement> ; <expression> ) ( <statement> ; )* end for
  return true;
}

// <if_statement> ::= if ( <expression> ) then
//                      ( <statement> ; )+
//                    [ else
//                      ( <statement> ; )+
//                    ]
//                    end if
bool Parser::IfStatement() {
  // <if_statement> is not required; cannot queue error if first token is not
  // found.
  if (!CheckTokenType(TokenType::TIf))
    return false;

  // if
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' after 'if' in if statement");
    return false;
  }

  // if (
  int errorQueueSizeSnapshot = errorQueue_.size();
  SymbolRecord expressionSymbol;
  if (!Expression(expressionSymbol)) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected expression in if statement after '('");
    return false;
  }

  if (!IsBooleanEquivalent(expressionSymbol)) {
    QueueTypeError("If statement must be a boolean or boolean equivalent, not " +
                   SymbolRecord::SymbolTypeToDebugString(expressionSymbol.type) + 
                   (expressionSymbol.isArray ? " array": ""));
    return false;
  }

  // [CODEGEN].
  if (expressionSymbol.is_literal == false)
    expressionSymbol.value = CodeGen::Load(expressionSymbol.value);
  CodeGen::IfThen(expressionSymbol.value);

  // if ( <expression>
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after 'if' in if statement");
    return false;
  }

  // if ( <expression> )
  if (!CheckTokenType(TokenType::TThen)) {
    QueueExpectedTokenError("Expected 'then' after '( <expression> )' in if statement");
    return false;
  }

  // if ( <expression> ) then
  if (!Statement()) {
    if (errorQueueSizeSnapshot == errorQueue_.size())
      QueueExpectedTokenError("Expected at least one statement after 'then' in if statement");
    return false;
  }

  // if ( <expression> ) then <statement>
  if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement under 'then' in if statement");
    return false;
  }

  // if ( <expression> ) then <statement> ;
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement under 'then' in if statement");
      return false;
    }
  }

  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  CodeGen::Else();

  // if ( <expression> ) then ( <statement> ; )+
  if (CheckTokenType(TokenType::TElse)) {

    if (!Statement()) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected at least one statement after 'else' in if statement");
      return false;
    }

    // if ( <expression> ) then ( <statement> ; )+ else <statement>
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement under 'else' in if statement");
      return false;
    }

    // if ( <expression> ) then ( <statement> ; )+ else <statement> ;
    while (Statement()) {
      if (!CheckTokenType(TokenType::TSemicolon)) {
        QueueExpectedTokenError("Expected ';' after statement under 'else' in if statement");
        return false;
      }
    }

    if (errorQueue_.size() > errorQueueSizeSnapshot)
      return false;
  }

  // if ( <expression> ) then ( <statement> ; )+ [ else ( <statement> ; )+ ]
  if (!CheckTokenType(TokenType::TEnd) || !CheckTokenType(TokenType::TIf)) {
    QueueExpectedTokenError("Expected 'end if' after if statement");
    return false;
  }

  CodeGen::EndIf();

  // if ( <expression> ) then ( <statement> ; )+ [ else ( <statement> ; )+ ] end if
  return true;
}

// <procedure_call> ::= <identifier> ( [<argument_list>] )
bool Parser::ProcedureCall(std::string& identifier) {
  // We are given a valid identifier. If a valid identifier could not be found
  // then Statement will know this, and report it without calling us.
  //if (!Identifier())
    //return false;

  // Assert: scopeManager_.getSymbol(identifier)->type == SymbolType::Procedure.
  // Assert: THIS IS NOT A DRILL. This is a real procedure call since we know
  // the identifier is valid. This means in order to properly invoke a
  // procedure, we have check that:
  //   1.) The identifier matches an accessible procedure identifier         ✅
  //     - This is already tested by AssignmentStatement's Destination call.
  //   2.) Any names used as arguments are valid symbols                     ✅
  //   3.) The argument list length matches the stored parameter list length ✅
  //   4.) Each argument type matches the expected type                      ✅
  // TODO(domfarolino): If anything is wrong with the above conditions, errors
  // must be displayed in the above order. This needs tested; See
  // https://github.com/domfarolino/compiler/issues/24.

  // <identifier>
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' after procedure identifier to invoke procedure");
    return false;
  }

  // <identifier> (
  // An ArgumentList is optional for procedure invocation, so if
  // ArgumentList() returned false only because it didn't exist, we can't let
  // that make us return false here, since the rest of ProcedureCall existed
  // just fine. We only want to propagate errors.
  std::vector<SymbolRecord> argumentVec;
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ArgumentList(argumentVec) && errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // Conditions (1) and (2) from above are met; checking (3), length, now.
  SymbolRecord* procedureSymbol = scopeManager_.getSymbol(identifier);

  // Assert: |procedureSymbol| is non-null.
  int argsLength = argumentVec.size();
  int expectedArgsLength = procedureSymbol->params.size();

  if (argsLength != expectedArgsLength) {
    QueueSymbolError("Argument length mismatch: Procedure '" + identifier +
                     "' expected " + std::to_string(expectedArgsLength) +
                     " args, but was given " + std::to_string(argsLength) +
                     " args");
    return false;
  }

  // Assert: |argumentVec.size()| == |procedureSymbol->params.size()|.
  // Perform the type checking now (4).
  // "The type signatures of a procedures arguments must match exactly their
  // parameter declaration."
  for (int i = 0; i < argumentVec.size(); ++i) {
    SymbolType argType = argumentVec[i].type,
               paramType = procedureSymbol->params[i].second.type;
    bool arg_arrayness = argumentVec[i].isArray,
         param_arrayness = procedureSymbol->params[i].second.isArray;

    // The below three checks implement
    // https://github.com/domfarolino/compiler/issues/37.
    // General type check.
    if (argType != paramType) {
      QueueTypeError("Procedure '" + identifier + "' argument at position " +
                     std::to_string(i + 1) + " of type " +
                     SymbolRecord::SymbolTypeToDebugString(argType) +
                     " does not match corresponding parameter of type " +
                     SymbolRecord::SymbolTypeToDebugString(paramType));
      return false;
    }

    // Array-ness check.
    if (arg_arrayness != param_arrayness) {
      QueueTypeError("Procedure '" + identifier + "' argument at position " +
                     std::to_string(i + 1) +
                     "'s array-ness does match corresponding parameter '" +
                     procedureSymbol->params[i].first + "'");
      return false;
    }

    // Literal-ness and reference param check.
    ParameterType arg_param_type = argumentVec[i].paramType,
                  param_param_type = procedureSymbol->params[i].second.paramType;
    bool arg_is_literal = argumentVec[i].is_literal,
         param_is_literal = procedureSymbol->params[i].second.is_literal;
    // ParameterType check.
    if (arg_is_literal && (param_param_type == ParameterType::Out ||
                           param_param_type == ParameterType::InOut)) {
      QueueTypeError("Cannot pass a literal value as '" +
                     procedureSymbol->params[i].first + "' reference param");
      return false;
    }
  }

  // <identifier> (...
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after argument list");
    return false;
  }

  // TODO(domfarolino): [CODEGEN] Call CodeGen::CallFunction() with
  // |procedureSymbol|'s value as the Function*. We also
  // need to loop through |procedureSymbol->params| and determine what CodeGen
  // loads we might need to perform on corresponding |argumentVec| args, and use
  // the results in our CodeGen::CallFunction() call. For example, all of the
  // arguments in |argumentVec| will either be:
  //   - AllocaInst*s representing the address of a local variable (goes for
  //     primitive, array, and single array member). If we're passing this as
  //     a(n):
  //     - In primitive whose |isArray| and |is_literal| are not set, we need to
  //     - CodeGen::Load the Value* to produce a copy to pass in.              ✅
  //     - Out/InOut primitive or array, we can just pass along the
  //       AllocaInst*. We already know |is_literal| will not be set.          ❌
  //   - Value* if the argument was literally-produced (aka the SymbolRecord is
  //     a literal anonymous one produced by ::Name()). In this case, we need
  //     to make sure that the corresponding parameter is In, as we cannot pass
  //     a literal value by reference.                                         ✅
  std::vector<Value*> arguments;
  for (int i = 0; i < argumentVec.size(); ++i) {
    if (procedureSymbol->params[i].second.paramType == ParameterType::In &&
        argumentVec[i].isArray == false &&
        argumentVec[i].is_literal == false) {
      argumentVec[i].value = CodeGen::Load(argumentVec[i].value);
    }

    arguments.push_back(argumentVec[i].value);
  }

  CodeGen::CallFunction(procedureSymbol->value, arguments, "call-" + identifier);

  // <identifier> (...)
  return true;
}

// <return_statement> ::= return
bool Parser::ReturnStatement() {
  // TODO(domfarolino): CodeGen::Return here. Verify that CodeGen can handle
  // multiple Return() calls without breaking. Tests should be made for this.
  return CheckTokenType(TokenType::TReturn);
}

// <argument_list> ::= <expression> , <argument_list> | <expression>
bool Parser::ArgumentList(std::vector<SymbolRecord>& argumentVec) {
  // The language allows empty argument lists, so Expression() could have
  // returned false because it didn't exist, or because it failed to parse.
  // If it failed to parse, Expression will take care of queueing an error.
  SymbolRecord expressionSymbol;
  if (!Expression(expressionSymbol))
    return false;

  // TODO(domfarolino): Make this and the below instance actually push_back the
  // expression information instead of just placeholder info for length.
  argumentVec.push_back(expressionSymbol);

  int errorQueueSizeSnapshot = errorQueue_.size();
  while (CheckTokenType(TokenType::TComma)) {
    SymbolRecord expressionSymbol;
    if (!Expression(expressionSymbol)) {
      if (errorQueueSizeSnapshot == errorQueue_.size())
        QueueError("Expected argument after ',' in argument list");
      return false;
    }

    argumentVec.push_back(expressionSymbol);
  }

  return true;
}

// <procedure_declaration> ::= <procedure_header> <procedure_body>
bool Parser::ProcedureDeclaration(std::string& identifier,
                                  SymbolRecord& symbolRecord) {
  // TODO(domfarolino): Consider making this function take in a global flag
  // instead of a symbol record, so we can create and add the symbol record
  // here. See https://github.com/domfarolino/compiler/issues/19.

  symbolRecord.type = SymbolType::Procedure;
  SymbolTable& outerScope = scopeManager_.getCurrentScopeRef();
  std::vector<std::pair<std::string, SymbolRecord>> parameters;
  // This is not a valid ProcedureDeclaration, if the ProcedureHeader is either
  // missing or invalid.
  if (!ProcedureHeader(identifier, parameters))
    return false;

  // Create the final procedure symbol record.
  symbolRecord.params = parameters;

  // TODO(domfarolino): [CODEGEN] CodeGen::CreateFunction() somewhere around
  // here. CreateFunction() returns a FunctionDeclaration, which contains a
  // bunch of Value*s that we need to store in the symbol table.

  // Enter procedure scope.
  scopeManager_.enterScope();

  // Add the procedure's symbol to its own symbol table.
  if (!scopeManager_.insertAllowShadow(identifier, symbolRecord))
    throw std::logic_error("Unreachable: Cannot insert procedure into its own local scope");

  // Add each parameter's symbol to the current procedure's symbol table.
  for (auto parameterInfo: parameters) {
    // parameterInfo.first = identifier.
    // parameterInfo.second = symbol record.

    if (!scopeManager_.insertAllowShadow(parameterInfo.first, parameterInfo.second)) {
      QueueSymbolError("Cannot add parameter '" + parameterInfo.first +
                       "' to the current scope, symbol name already exists");
      return false;
    }
  }

  // Insert the symbol into the "outer" scope that it appears within.
  if (!scopeManager_.insertAtScope(identifier, symbolRecord, outerScope)) {
    QueueSymbolError("Symbol '" + identifier + "' already exists, cannot redeclare");
    return false;
  }

  // <procedure_header>
  // Same goes for ProcedureBody...
  bool parsedBody = ProcedureBody();
  
  if (symbolInsight_) scopeManager_.printTopScope();

  // TODO(domfarolino): [CODEGEN] CodeGen::EndFunction() around here.
  scopeManager_.leaveScope();
  return parsedBody;
}

// <procedure_header> :: = procedure <identifier> ( [<parameter_list>] )
bool Parser::ProcedureHeader(std::string& identifier,
                             std::vector<std::pair<std::string,
                                                   SymbolRecord>>& parameters) {
  // A <procedure_header> doesn't always have to exist (as up the chain it is
  // eventually a (<declaration>;)*, which can appear no times), so if the first
  // terminal of a production like this, we have to let our caller decide
  // whether it is appropriate to throw an error. This is very similar to what
  // we do in VariableDeclaration, since it seeks for the first terminal in a
  // production that doesn't always have to produce anything.
  if (!CheckTokenType(TokenType::TProcedure))
    return false;

  // procedure
  if (!Identifier(identifier)) {
    QueueExpectedTokenError("Expected identifier in procedure header");
    return false;
  }

  // procedure <identifier>
  if (!CheckTokenType(TokenType::TLeftParen)) {
    QueueExpectedTokenError("Expected '(' before parameter list in procedure header");
    return false;
  }

  // procedure <identifier> (
  // A ParameterList is optional, so if ParameterList() returned false only
  // because it didn't exist, we can't let that make us return false here, since
  // the rest of ProcedureHeader existed just fine. We only want to propagate
  // errors.
  int errorQueueSizeSnapshot = errorQueue_.size();
  if (!ParameterList(parameters) && errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // procedure <identifier> (..."
  if (!CheckTokenType(TokenType::TRightParen)) {
    QueueExpectedTokenError("Expected ')' after parameter list in procedure header");
    return false;
  }

  // procedure <identifier> (...)
  return true;
}

// <parameter_list> ::= <parameter> , <parameter_list> | <parameter>
bool Parser::ParameterList(std::vector<std::pair<std::string, SymbolRecord>>& parameters) {
  // The language allows empty parameter lists, so Parameter() could have
  // returned false because it didn't exist, or because it failed to parse.
  // Either way, we return false, and our caller will detect whether there
  // was an error or not. If Parameter() was true, we only return true if
  // there are no trailing commas in the list.
  if (!Parameter(parameters))
    return false;

  while (CheckTokenType(TokenType::TComma)) {
    if (!Parameter(parameters)) {
      QueueError("Expected parameter after ',' in parameter list");
      return false;
    }
  }

  return true;
}

// <parameter> ::= <variable_declaration> (in | out | inout)
bool Parser::Parameter(std::vector<std::pair<std::string, SymbolRecord>>& parameters) {
  // If VD returns false, that could be because it didn't find a VD, or because
  // a VD failed to parse. Parameters aren't mandatory, so we only care about
  // the latter case. Our caller will determine whether the VD had an error
  // parsing or not. In this frame, we only care about completion.
  // [CODEGEN]: |symbolRecord|'s Value* member will be initialized to the
  // passed in argument's Value* in ProcedureDeclaration.
  std::string identifier;
  SymbolRecord symbolRecord; // Exception to issue #19.
  if (!VariableDeclaration(identifier, symbolRecord))
    return false;

  // Must pull the lexeme of the parameter type out now, since |CheckTokenType|
  // advances the token. We use |paramTypeString| below.
  std::string paramTypeString = token_.lexeme;
  if (!CheckTokenType(TokenType::TIn) &&
      !CheckTokenType(TokenType::TOut) &&
      !CheckTokenType(TokenType::TInOut)) {
    QueueExpectedTokenError("Expected 'in', 'out', or 'inout' after parameter identifier");
    return false;
  }

  symbolRecord.paramType = SymbolRecord::ParameterTypeStringToParameterType(paramTypeString);
  parameters.push_back(std::make_pair(identifier, symbolRecord));
  return true;
}

// <procedure_body> ::= (<declaration>;)* begin (<statement>;)* end procedure
bool Parser::ProcedureBody() {
  int errorQueueSizeSnapshot = errorQueue_.size();
  while (Declaration()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after declaration");
      return false;
    }
  }

  // Declaration() isn't required, so there are two reasons it may return false.
  // For these reasons, see ProgramBody() above.
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // Assert: We ran out of Declarations, so really if we see anything that is
  // not `begin`, we were expecting `begin` or more declarations.
  // (<declaration>;)*
  if (!CheckTokenType(TokenType::TBegin)) {
    QueueExpectedTokenError("Expected 'begin' or declaration in procedure body");
    return false;
  }

  // (<declaration>;)* begin
  while (Statement()) {
    if (!CheckTokenType(TokenType::TSemicolon)) {
      QueueExpectedTokenError("Expected ';' after statement");
      return false;
    }
  }

  // Same error checking as above (like w/ Declaration).
  if (errorQueue_.size() > errorQueueSizeSnapshot)
    return false;

  // (<declaration>;)* begin (<statement>;)
  if (!CheckTokenType(TokenType::TEnd) ||
      !CheckTokenType(TokenType::TProcedure)) {
    QueueExpectedTokenError("Expected 'end program' after procedure body");
    return false;
  }

  // (<declaration>;)* begin (<statement>;)* end procedure
  return true;
}

// <variable_declaration> ::= <type_mark> <identifier>
//                              [ [ <lower_bound> “:” <upper_bound> ] ]
bool Parser::VariableDeclaration(std::string& identifier,
                                 SymbolRecord& symbolRecord) {
  // Can't report error if TypeMark was not found, because VariableDeclaration
  // isn't always required. Caller will decide whether a VD that doesn't exist
  // is an error or not, given the context.
  std::string typeMark;
  if (!TypeMark(typeMark))
    return false;

  symbolRecord.type = SymbolRecord::TypeMarkToSymbolType(typeMark);

  // <type_mark>
  if (!Identifier(identifier)) {
    QueueExpectedTokenError("Expected identifier in variable declaration");
    return false;
  }

  // <type_mark> <identifier>
  int error_queue_size_snapshot = errorQueue_.size();
  if (CheckTokenType(TokenType::TLeftBracket)) {
    symbolRecord.isArray = true;

    // Declaring an array, lower and upper bound are required.
    // <type_mark> <identifier> [
    if (!LowerOrUpperBound(symbolRecord.lowerBound)) {
      if (error_queue_size_snapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected integer for array lower bound");
      return false;
    }

    // <type_mark> <identifier> [ <number>
    if (!CheckTokenType(TokenType::TColon)) {
      QueueExpectedTokenError("Expected ':' separating lower and upper array bounds");
      return false;
    }

    // <type_mark> <identifier> [ <number> :
    if (!LowerOrUpperBound(symbolRecord.upperBound)) {
      if (error_queue_size_snapshot == errorQueue_.size())
        QueueExpectedTokenError("Expected integer for array upper bound");
      return false;
    }

    // <type_mark> <identifier> [ <number> : <number>
    if (!CheckTokenType(TokenType::TRightBracket)) {
      QueueExpectedTokenError("Expected ']' after array bounds");
      return false;
    }
  }

  return true;
}

// <type_mark> ::= integer | float | string | bool | char
// Not necessary to find one in all usages, so we leave error reporting to the
// caller.
bool Parser::TypeMark(std::string& typeMark) {
  typeMark = token_.lexeme;
  return CheckTokenType(TokenType::TIntegerType) ||
         CheckTokenType(TokenType::TFloatType)   ||
         CheckTokenType(TokenType::TStringType)  ||
         CheckTokenType(TokenType::TBoolType)    ||
         CheckTokenType(TokenType::TCharType);
}

// <lower_bound> ::= [-] <number>
// <upper_bound> ::= [-] <number>
bool Parser::LowerOrUpperBound(std::string& number) {
  if (CheckTokenType(TokenType::TMinus))
    number += '-';

  SymbolRecord dummyRecord;
  if (!Number(number, dummyRecord))
    return false;

  if (dummyRecord.type == SymbolType::Float) {
    QueueTypeError("Array lower/upper bounds must be of type 'integer', not " +
                   SymbolRecord::SymbolTypeToDebugString(dummyRecord.type));
    return false;
  }

  return true;
}

// <number> ::= [0-9][0-9_]*[.[0-9_]*]
// Not necessary to find one in all usages, so we leave error reporting to the
// caller.
bool Parser::Number(std::string& number, SymbolRecord& symbolRecord) {
  number += token_.lexeme;
  if (CheckTokenType(TokenType::TInteger)) {
    symbolRecord.type = SymbolType::Integer;
    return true;
  } else if (CheckTokenType(TokenType::TFloat)) {
    symbolRecord.type = SymbolType::Float;
    return true;
  }

  return false;
}
