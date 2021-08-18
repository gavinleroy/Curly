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

ws = Drop(Star(Space()))

@with_pre ws begin

    curly_int = p"\d+" > (x -> CInt(parse(Int64, x)))

    sym = p"[a-zA-Z]+[a-zA-Z_]*" > CSymb

    # Expressions

    expr = Delayed()

    cexpr = Delayed()

    group = E"{" + ws + expr + ws + E"}"

    plus = E"+" + cexpr + cexpr > Plus

    sub = E"-" +  cexpr + cexpr > Sub

    ifzero = E"if0" + cexpr + cexpr +  cexpr > IfZero

    lete = E"let" + ws + E"{" + sym + cexpr + E"}" + cexpr > Let

    lambda = E"lambda" + ws + E"{" + sym + ws + E"}" + cexpr > Lambda

    app = cexpr + cexpr > App

    expr.matcher = plus | sub | ifzero | lete | lambda | app

    cexpr.matcher = (group | curly_int | sym) + ws

    prog = cexpr + ws + Eos() # Add `Eos` to ensure that all input is matched.

end

#############
# TRANSFORM #
#############

# Any other input to transform besides the below should be an error.
transform(n) = @assert false

transform(n::CInt) = n.val

transform(s::CSymb) = Symbol(s.sym)

function transform(p::Plus)
    l = transform(p.lhs);
    r = transform(p.rhs);
    return :($l + $r)
end

function transform(s::Sub)
    l = transform(s.lhs);
    r = transform(s.rhs);
    return :($l - $r)
end

function transform(iz::IfZero)
    cnd = transform(iz.cnd);
    te = transform(iz.true_e);
    fe = transform(iz.false_e);
    return :(($cnd == 0) ? $te : $fe)
end

function transform(l::Let)
    sy = transform(l.arg);
    v = transform(l.val);
    bdy = transform(l.bdy);
    return :( ($sy -> $bdy)($v) )
end

function transform(lam::Lambda)
    sy = transform(lam.arg);
    arg_ex = transform(lam.bdy);
    return :(($sy -> $arg_ex))
end

function transform(a::App)
    g = transform(a.f);
    p = transform(a.arg);
    return :($g($p))
end

########
# EXEC #
########

"Curly program string"
macro curly_str(source)
    # parse one program (i.e. cexpr)
    # and then take the first result
    # as there should only be one :)
    curly_expr = parse_one(source, prog)[1];
    return transform(curly_expr)
end

"Macro for pretty-printing test results"
macro test(prog, expected)
    quote
        if $prog == $expected
            printstyled("[Pass] \U2713 \n", color=:green)
        else
            printstyled("[Fail] \U0021 \n"
                        , $(Meta.quot(prog))
                        , " == "
                        , $(Meta.quot(expected)), "\n"
                        , " Received : ", $prog
                        , color=:red)
        end
    end
end

"Run the Curly test suite"
function run_tests()
    # Basic additive properties
    @test curly"{+ 1 2}" 3;
    @test curly"{+ 2 1}" 3;
    @test curly"{+ 1 {+ 2 3}}" 6;
    @test curly"{+ {+ 1 2} 3}" 6;
    @test curly"{+ 1 0}" 1;
    @test curly"{+ 0 1}" 1;

    # subtractive properties
    @test curly"{- 42 0}" 42;
    @test curly"{- 0 42}" -42;

    # Handle arbitrary whitespace
    @test curly"""
          {+ 1
            {- 0
              {+ 0
                1}}}
          """ 0

    # simple lambdas and application
    @test curly"{{lambda {x} x} 6}" 6;
    @test curly"{{lambda {x} {+ x 1}} 6}" 7;
    @test curly"""
          {{{lambda {x}
            {lambda {y}
              {+ x y}}}
              7} 4}
          """ 11

    @test curly"""
          {let
{f {
lambda {
y} {lambda {x}
                        {
+ x
y}}}} {-
{ {     f 10
} 20} {{f 30} 40}}}
            """ -40
    @test curly"""
    {
    let
    {
    x
    1
    }
    {
    let
    {
    f
    {
    lambda
    {
    x
    }
    {
    -
    x
    1
    }
    }
    }
    {
    +
    x
    {
    f
    x
    }
    }
    }
    }
    """ 1

    # Testing Recursive programs
    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {sum {lambda {f}
                        {lambda {x}
                          {if0 x
                            0
                            {+ x {f {- x 1}}}}}}}
              {{Y sum} 10}}}
          """ 55

    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {sum {lambda {f}
                        {lambda {x}
                          {if0 x
                            0
                            {+ x {f {- x 1}}}}}}}
              {{Y sum} 200000}}}
          """ 20000100000

    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {mult {lambda {f}
                        {lambda {x}
                          {lambda {y}
                            {if0 y
                              0
                              {+ x {{f x } {- y 1}}}}}}}}
              {{{Y mult} 7} 6}}}
          """ 42

    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {mult {Y {lambda {f}
                        {lambda {x}
                          {lambda {y}
                            {if0 y
                              0
                              {+ x {{f x } {- y 1}}}}}}}}}
              {-
                {{mult 7} {+ 4 3}}
                {+ {{mult 7} 4} {{mult 3} 7}}}}}
          """ 0

    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {mult {lambda {f}
                        {lambda {x}
                          {lambda {y}
                            {if0 y
                              0
                              {+ x {{f x } {- y 1}}}}}}}}
              {{{Y mult} 73} 648}}}
          """ 47304

    # the recursive factorial can easily cause a stack overflow
    # due to the also recursive multiplication.
    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {mult {Y {lambda {f}
                        {lambda {x}
                          {lambda {y}
                            {if0 y
                              0
                              {+ x {{f x } {- y 1}}}}}}}}}
              {let {fac {Y {lambda {f}
                            {lambda {x}
                              {if0 x
                                1
                                {{mult x} {f {- x 1}}}}}}}}
                {fac 5}}}}
          """ 120

    @test curly"""
          {let {Y {lambda {f}
                    {{lambda {x}
                      {x x}}
                      {lambda {x}
                        {f {lambda {y}
                          {{x x} y}}}}}}}
            {let {fib {Y {lambda {f}
                          {lambda {x}
                            {if0 {- x 1}
                              1
                              {if0 x
                                1
                                {+ {f {- x 1}} {f {- x 2}}}}}}}}}
              {fib 42}}}
          """ 433494437
end

end # module curly
