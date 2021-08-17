##
# Gavin Gray ~ August, 2021
#
# Curly is a simple scheme-like language
# the entire program is a single expression
# that must evaluate to a Int
#
# <expr> ::= int64
#            | symbol
#            | { + <expr> <expr> }
#            | { - <expr> <expr> }
#            | { if0 <expr> <expr> <expr>}
#            | { let  { <symbol> <expr> } <expr> }
#            | { lambda { <symbol> } <expr> }
#            | { <symbol> <expr> }

module curly

using ParserCombinator

export @curly_str, @test, run_tests

###########
# STRUCTS #
###########

abstract type Node end

struct CInt <: Node
    val
end

struct CSymb <: Node
    sym
end

struct Plus <: Node
    lhs
    rhs
end

struct Sub <: Node
    lhs
    rhs
end

struct IfZero <: Node
    cnd
    true_e
    false_e
end

struct Let <: Node
    arg
    val
    bdy
end

struct Lambda <: Node
    arg
    bdy
end

struct App <: Node
    f
    arg
end

##########
# PARSER #
##########

ws = p"[\s\n]*"

opt_ws = ws | e""

expr = Delayed()

curly_int = p"\d+" > (x -> CInt(parse(Int64, x)))

sym = p"[a-zA-Z]+[a-zA-Z_]*" > CSymb

plus = E"{+ " + ~opt_ws + expr + ~opt_ws + expr + ~opt_ws + E"}" > Plus

sub = E"{- " + ~opt_ws +  expr + ~opt_ws + expr + ~opt_ws + E"}" > Sub

ifzero = E"{if0 " + ~opt_ws + expr + ~opt_ws + expr + ~opt_ws +  expr + ~opt_ws + E"}" > IfZero

lete = E"{let {" + ~opt_ws + expr + ~opt_ws + expr + ~opt_ws + E"}" + ~opt_ws + expr + ~opt_ws + E"}" > Let

lambda = E"{lambda {" + ~opt_ws + expr + ~opt_ws + E"}" + ~opt_ws + expr + ~opt_ws + E"}" > Lambda

app = E"{" + ~opt_ws + expr + ~opt_ws + expr + ~opt_ws + E"}" > App

expr.matcher = curly_int | sym | plus | sub | ifzero | lete | lambda | app

prog = expr + Eos()

##########
# INTERP #
##########

interp(n::Node) = println("Interp is undefined for this construct")

interp(n::CInt) = :($n.val)

interp(s::CSymb) = Symbol(s.sym)

function interp(p::Plus)
    l = interp(p.lhs);
    r = interp(p.rhs);
    return :($l + $r)
end

function interp(s::Sub)
    l = interp(s.lhs);
    r = interp(s.rhs);
    return :($l - $r)
end

function interp(iz::IfZero)
    cnd = interp(iz.cnd);
    te = interp(iz.true_e);
    fe = interp(iz.false_e);
    return :(($cnd == 0) ? $te : $fe)
end

function interp(l::Let)
    sy = interp(l.arg);
    v = interp(l.val);
    bdy = interp(l.bdy);
    # Would be better to exchange for a typechecker
    @assert typeof(sy) == Symbol;
    return :( ($sy -> $bdy)($v) )
end

function interp(lam::Lambda)
    sy = interp(lam.arg);
    arg_ex = interp(lam.bdy);
    # Would be better to exchange for a typechecker
    @assert typeof(sy) == Symbol
    return :(($sy -> $arg_ex))
end

function interp(a::App)
    g = interp(a.f);
    p = interp(a.arg);
    return :($g($p))
end

########
# EXEC #
########

"Curly program string"
macro curly_str(line)
    # parse one program (i.e. expr)
    # and then take the first result
    # as there should only be one :)
    curly_expr = parse_one(strip(line), prog)[1];
    julia_expr = interp(curly_expr);
    return julia_expr
end

"Macro for pretty-printing test cases"
macro test(prog, expected)
    quote
        if $prog == $expected
            printstyled("[Pass] \U2713 \n", color=:green);
        else
            printstyled("[Failed] ==> "
                        , $(Meta.quot(prog))
                        , " == "
                        , $(Meta.quot(expected)), "\n"
                        , color=:red);
        end
    end
end

"Run the Curly testing suite"
function run_tests()
    @test curly"{+ 1 2}" 3;
    @test curly"{+ 2 1}" 3;
    @test curly"{+ 1 0}" 1;
    @test curly"{+ 0 1}" 1;
end

end # module curly
