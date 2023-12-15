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

long solve(string[] input) {
    int[] colMin = new int[] (input.length);
    auto allLen = input.length;
    long ans = 0;
    foreach (i, rw; input) {
        foreach (j, e; rw) {
            if (e == '#') { colMin[j] = i+1; }
            if (e == 'O') {
                auto cur = allLen - colMin[j];
                ++colMin[j];

                ans += cur;
            }
        }
    }

    return ans;
}

long solveEasy(string[] input) => input.solve;

long solveHard(string[] input) {
    return 0;
}