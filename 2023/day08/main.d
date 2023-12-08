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

alias Route = string;

alias Node = Tuple!(string, string);
alias Graph = Node[string];
alias State = Tuple!(Route, "route", Graph, "g");

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

Tuple!(string, Node) parseLine(string input) {
    string f;
    Node n;
    input.formattedRead("%s = (%s, %s)", f, n[0], n[1]);
    return tuple(f, n);
}

Graph parseGraph(string[] input) => input.map!parseLine.assocArray;

State parse(string[] input) {
    auto route = input[0].strip;
    auto graph = input[2..$].parseGraph;
    return State(route, graph);
}

int solveEasy(string[] input) {
    auto state = input.parse;

    debug { state.writeln; }

    auto current = "AAA";
    auto i = 0;
    auto steps = 0;
    while (current != "ZZZ") {
        current = state.route[i] == 'L' ? state.g[current][0] : state.g[current][1];

        steps += 1;
        i = (i + 1) % state.route.length;
    }

    return steps;
}

long solveHard(string[] input) {
    auto state = input.parse;

    debug { state.route.length.writeln; }

    auto nodes = state.g.keys.filter!(s => s.endsWith('A')).array;

    debug { nodes.writeln; }

    string getNodeAfterStep(string node, char dir) { return dir == 'L' ? state.g[node][0] : state.g[node][1]; }

    int getSteps(string node) {
        auto i = 0;
        auto steps = 0;
        while (!node.endsWith('Z')) {
            node = getNodeAfterStep(node, state.route[i]);
            steps += 1;

            i = (i + 1) % state.route.length;
        }

        return steps;
    }

    return nodes.map!getSteps.fold!lcm(1L);
}