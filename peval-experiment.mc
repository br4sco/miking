include "peval/pval.mc"
include "peval/peval.mc"
include "mexpr/pprint.mc"
include "mexpr/boot-parser.mc"
include "mexpr/side-effect.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/lamlift.mc"

lang ExperimentalLang =
  PE + PEvalLetInline +
  MExprSym + MExprTypeCheck + MExprLambdaLiftAllowSpineCapture +
  MExprPrettyPrint +
  BootParser
end

mexpr

use ExperimentalLang in

let pevalInlineLets = pevalInlineLets (sideEffectEnvEmpty ()) in

--- Read Program ---

let tm = parseMCoreFile
           { _defaultBootParserParseMCoreFileArg () with
             eliminateDeadCode = false }
           "input-program.mc"
in
let tm = symbolize tm in
let tm = typeCheck tm in

--- Transform

let tm = pESpecialize tm in

--- Write Resulting expression as a program

writeFile "output-program.mc" (join ["mexpr\n\n", (mexprToString tm), "\n"]);

print "OK\n";

()
