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

long getTotalDistances(Tuple!(int, int)[] pts) {
    long tot = 0;
    foreach (i, p1; pts) {
        foreach (p2; pts[i+1 .. $]) {
            auto cur = abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]);
            tot += cur;
        }
    }
    return tot;
}

long solve(string[] input) {
    Tuple!(int, int)[] pts;
    foreach (i, rw; input) {
        foreach (j, e; rw) {
            if (e == '#') { pts ~= tuple(i.to!int, j.to!int); }
        }
    }
    
    return pts.getTotalDistances;
}

long solveEasy(string[] input) => input.expand.solve;

int[] getEmptyIndices(string[] input) {
    int[] res;
    foreach (i, rw; input) { if (rw.all!(c => c == '.')) { res ~= i; } }
    return res;
}

long solveHard(string[] input) {
    auto emptyRows = input.getEmptyIndices;
    input = input.transpose;
    auto emptyCols = input.getEmptyIndices;
    input = input.transpose;

    Tuple!(int, int)[] pts;
    auto emptyRwIdx = 0;
    auto emptyColIdx = 0;
    immutable expand_const = 999_999;
    foreach (i, rw; input) {
        if (emptyRwIdx < emptyRows.length && i == emptyRows[emptyRwIdx]) { ++emptyRwIdx; }
        emptyColIdx = 0;
        foreach (j, e; rw) {
            if (emptyColIdx < emptyCols.length && j == emptyCols[emptyColIdx]) { ++emptyColIdx; }

            if (e == '#') {
                pts ~= tuple(i.to!int + emptyRwIdx * expand_const, j.to!int + emptyColIdx * expand_const);
            }
        }
    }

    return pts.getTotalDistances;
}