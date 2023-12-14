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

    auto res = solveEasy(input);
    // auto res = solveHard(input);

    res.writeln;
}

string[] expandRows(string[] input) {
    auto rows = input.enumerate(0).filter!(t => t[1].all!(c => c == '.')).map!(t => t[0]).array;
    auto toInsert = '.'.repeat(input[0].length).to!string;

    foreach_reverse (idx; rows) {
        input.insertInPlace(idx, toInsert);
    }

    return input;
}

string[] transpose(string[] input) => (cast(char[][])input).transposed.map!(to!string).array;

string[] expandColumns(string[] input) => input.transpose.expandRows.transpose;

string[] expand(string[] input) => input.expandRows.expandColumns;

long solve(string[] input) {
    Tuple!(int, int)[] pts;
    foreach (i, rw; input) {
        foreach (j, e; rw) {
            if (e == '#') { pts ~= tuple(i.to!int, j.to!int); }
        }
    }
    
    long tot = 0;
    foreach (i, p1; pts) {
        foreach (p2; pts[i+1 .. $]) {
            auto cur = abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]);
            tot += cur;
        }
    }

    return tot;
}

long solveEasy(string[] input) => input.expand.solve;

int solveHard(string[] input) => 0;