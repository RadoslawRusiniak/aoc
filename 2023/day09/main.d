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

int[] parseLine(string input) => input.split.map!(to!int).array;

int[][] parse(string[] input) => input.map!parseLine.array;

int solveLine(int[] line) {
    int[][] diffs;
    diffs ~= line;
    do {
        diffs ~= 
            zip(diffs.back.dropOne, diffs.back)
            .map!(t => t[0] - t[1])
            .array;
    } while (!diffs.back.all!(x => x == 0));

    diffs[$ - 1] ~= 0;

    foreach_reverse (i; 0 .. diffs.length - 1) {
        auto above = diffs[i+1];
        diffs[i] ~= diffs[i].back + above.back;
    }

    // debug { diffs.each!writeln; }

    return diffs[0].back;
}

long solveEasy(string[] input) => input.parse.map!solveLine.sum(0L);

long solveHard(string[] input) => input.parse.map!(a => a.retro.array).map!solveLine.sum(0L);