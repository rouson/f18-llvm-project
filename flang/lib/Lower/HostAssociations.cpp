//===-- HostAssociations.cpp ----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/Lower/HostAssociations.h"
#include "SymbolMap.h"
#include "flang/Lower/AbstractConverter.h"
#include "flang/Lower/CharacterExpr.h"
#include "flang/Lower/ConvertType.h"
#include "flang/Lower/FIRBuilder.h"
#include "flang/Lower/PFTBuilder.h"
#include "flang/Lower/Todo.h"
#include "flang/Optimizer/Support/FatalError.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "flang-host-assoc"

// `t` should be the result of getArgumentType, which has a type of
// `!fir.ref<tuple<...>>`.
static mlir::TupleType unwrapTupleTy(mlir::Type t) {
  return fir::dyn_cast_ptrEleTy(t).cast<mlir::TupleType>();
}

void Fortran::lower::HostAssociations::hostProcedureBindings(
    Fortran::lower::AbstractConverter &converter,
    Fortran::lower::SymMap &symMap) {
  if (symbols.empty())
    return;

  // Create the tuple variable.
  auto tupTy = unwrapTupleTy(getArgumentType(converter));
  auto &builder = converter.getFirOpBuilder();
  auto loc = converter.genLocation();
  auto hostTuple = builder.create<fir::AllocaOp>(loc, tupTy);
  auto offTy = builder.getIntegerType(32);

  // Walk the list of symbols and update the pointers in the tuple.
  auto offsetAndType =
      [&](unsigned sidx) -> std::pair<mlir::Value, mlir::Type> {
    auto off = builder.createIntegerConstant(loc, offTy, sidx);
    auto varTy = tupTy.getType(sidx);
    auto refTy = builder.getRefType(varTy);
    auto eleOff = builder.create<fir::CoordinateOp>(loc, refTy, hostTuple, off);
    return {eleOff, varTy};
  };
  for (auto s : llvm::enumerate(symbols)) {
    auto box = symMap.lookupSymbol(s.value());
    box.match(
        [&](const Fortran::lower::SymbolBox::Intrinsic &v) {
          auto [eleOff, varTy] = offsetAndType(s.index());
          auto castBox = builder.createConvert(loc, varTy, v.getAddr());
          builder.create<fir::StoreOp>(loc, castBox, eleOff);
        },
        [&](const Fortran::lower::SymbolBox::Char &v) {
          // Create a boxchar.
          auto boxchar = Fortran::lower::CharacterExprHelper(builder, loc)
                             .createEmboxChar(v.getAddr(), v.getLen());
          auto eleOff = offsetAndType(s.index()).first;
          builder.create<fir::StoreOp>(loc, boxchar, eleOff);
        },
        [&](const Fortran::lower::SymbolBox::FullDim &v) {
          // Create a box.
          auto [eleOff, boxTy] = offsetAndType(s.index());
          auto mbox = builder.create<fir::EmboxOp>(loc, boxTy, v.getAddr(),
                                                   builder.consShape(loc, v));
          builder.create<fir::StoreOp>(loc, mbox, eleOff);
        },
        [&](const Fortran::lower::SymbolBox::CharFullDim &v) {
          // Create a box.
          auto [eleOff, boxTy] = offsetAndType(s.index());
          auto len = v.getLen();
          auto mbox = builder.create<fir::EmboxOp>(
              loc, boxTy, v.getAddr(), builder.consShape(loc, v),
              /*slice=*/mlir::Value{}, mlir::ValueRange{len});
          builder.create<fir::StoreOp>(loc, mbox, eleOff);
        },
        [&](const Fortran::lower::SymbolBox::PointerOrAllocatable &v) {
          // Already boxed. so just use the box on hand.
          auto eleOff = offsetAndType(s.index()).first;
          builder.create<fir::StoreOp>(loc, v.getAddr(), eleOff);
        },
        [&](const Fortran::lower::SymbolBox::Box &v) {
          // Already boxed, so just use the box on hand.
          auto eleOff = offsetAndType(s.index()).first;
          builder.create<fir::StoreOp>(loc, v.getAddr(), eleOff);
        },
        [&](const Fortran::lower::SymbolBox::None &) {
          fir::emitFatalError(loc, "symbol not mapped");
        });
  }

  converter.bindHostAssocTuple(hostTuple);
}

void Fortran::lower::HostAssociations::internalProcedureBindings(
    Fortran::lower::AbstractConverter &converter,
    Fortran::lower::SymMap &symMap) {
  if (symbols.empty())
    return;

  // Find the argument with the tuple type. The argument ought to be appended.
  auto &builder = converter.getFirOpBuilder();
  auto argTy = getArgumentType(converter);
  auto tupTy = unwrapTupleTy(argTy);
  auto loc = converter.genLocation();
  auto func = builder.getFunction();
  mlir::Value tupleArg;
  for (auto [ty, arg] : llvm::reverse(
           llvm::zip(func.getType().getInputs(), func.front().getArguments())))
    if (ty == argTy) {
      tupleArg = arg;
      break;
    }
  if (!tupleArg)
    fir::emitFatalError(loc, "no host association argument found");

  converter.bindHostAssocTuple(tupleArg);

  auto offTy = builder.getIntegerType(32);

  // Walk the list and add the bindings to the symbol table.
  for (auto s : llvm::enumerate(symbols)) {
    auto off = builder.createIntegerConstant(loc, offTy, s.index());
    auto varTy = builder.getRefType(tupTy.getType(s.index()));
    auto eleOff = builder.create<fir::CoordinateOp>(loc, varTy, tupleArg, off);
    auto box = builder.create<fir::LoadOp>(loc, eleOff);
    Fortran::semantics::SymbolRef symRef = *s.value();
    symMap.addSymbol(symRef, box.getResult());
  }
}

mlir::Type Fortran::lower::HostAssociations::getArgumentType(
    Fortran::lower::AbstractConverter &converter) {
  if (symbols.empty())
    return {};
  if (argType)
    return argType;

  // Walk the list of Symbols and create their types. Wrap them in a reference
  // to a tuple.
  auto *ctxt = &converter.getMLIRContext();
  llvm::SmallVector<mlir::Type> tupleTys;
  for (const auto *sym : symbols) {
    auto varTy = Fortran::lower::translateSymbolToFIRType(converter, *sym);
    if (!fir::isa_trivial(varTy))
      TODO(converter.getCurrentLocation(),
           "non trivial associated variable in internal procedure");
    tupleTys.emplace_back(fir::PointerType::get(varTy));
  }
  argType = fir::ReferenceType::get(mlir::TupleType::get(ctxt, tupleTys));
  return argType;
}
