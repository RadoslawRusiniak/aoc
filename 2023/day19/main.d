import core.bitop, std.bitmanip;
import core.checkedint;
import core.time;
import std.algorithm, std.functional, std.meta;
import std.array, std.container;
import std.bigint;
import std.conv;
import std.format;
import std.math, std.numeric;
import std.range, std.range.interfaces;
import std.stdio, std.string;
import std.ascii, std.typecons;
import std.datetime.date, std.datetime.systime;

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

alias Category = string;
alias Rule = Tuple!(Category, "category", string, "op", int, "x", string, "res");
alias Flow = Tuple!(string, "name", Rule[], "rules");
alias System = Rule[][string];
alias Part = int[Category];
alias State = Tuple!(System, "system", Part[], "parts");

Rule parseRule(string input) {
    if (input.canFind("<") || input.canFind(">")) {
        auto symbol = input.canFind("<") ? "<" : ">";
        auto s = input.split(symbol);
        auto s2 = s[1].split(":");

        return Rule(s[0], symbol, s2[0].to!int, s2[1]);
    }

    return Rule("", "", 0, input);
}

Flow parseFlow(string input) {
    auto s = input.split("{");
    auto name = s[0];
    auto rest = s[1][0 .. $-1];
    auto rulesInput = rest.split(",");
    auto rulesOp = rulesInput.map!parseRule.array;

    return Flow(name, rulesOp);
}

System parseSystem(string[] input) => input.map!parseFlow.assocArray;

Part parsePart(string input) {
    Tuple!(Category, int) parseCategoryWithValue(string input) {
        auto s = input.split("=");
        return tuple(s[0], s[1].to!int);
    }

    input = input[1 .. $-1];

    return input.split(",").map!parseCategoryWithValue.assocArray;
}

Part[] parseParts(string[] input) => input.map!parsePart.array;

State parseInput(string[] input) {
    auto s = input.split("");
    auto system = parseSystem(s[0]);
    auto parts = parseParts(s[1]);
    return State(system, parts);
}

int solveEasy(string[] input) {
    auto state = input.parseInput;

    debug { 
        state.system.byKeyValue.each!(kv => writeln(kv.key, ' ', kv.value));
        state.parts.each!writeln;
    }

    bool isAccepted(Part p) {
        string runThroughFlow(Rule[] rules) {
            foreach (rule; rules) {
                if (rule.category.empty) { return rule.res; }

                auto val = p[rule.category];
                auto ok = rule.op == "<" ? val < rule.x : val > rule.x;
                if (ok) { return rule.res; }
            }

            assert(false);
        }
        
        string flow = "in";
        while (true) {
            flow = runThroughFlow(state.system[flow]);
            if (flow == "A") { return true; }
            if (flow == "R") { return false; }
        }
    }

    int toValue(Part p) => p.values.sum;

    return state.parts.filter!isAccepted.map!toValue.sum;
}

int solveHard(string[] input) {
    auto state = input.parseInput;
    auto flows = state.system;
    
    return 0;
}

unittest {
    auto s = "a<2006:qkq";
    auto r = parseRule(s);
    assert(r.elem == "a");
    assert(r.op == "<");
    assert(r.x == 2006);
    assert(r.res == "qkq");
}

unittest {
    string s = "px{a<2006:qkq,m>2090:A,rfg}";
    auto f = parseFlow(s);
    assert(f.name == "px");
    assert(f.rules[0] == Rule("a", "<", 2006, "qkq"));
}