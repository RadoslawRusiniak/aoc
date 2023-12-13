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

bool isHorizontalReflectionEasy(string[] input, int x) {
    auto res = input[x..$].zip(input[0..x].retro).all!(t => t[0] == t[1]);

    debug { writeln(input, ' ', x, ' ', res); }
    return res;
}

int getHorizontalReflectionsScore(string[] input, bool function(string[], int) isHorizontalReflection)
    => 
    iota(1, input.length)
    .filter!(x => isHorizontalReflection(input, x))
    .sum;

int getScore(string[] input, bool function(string[], int) isHorizontalReflection) => 
    input.getHorizontalReflectionsScore(isHorizontalReflection) * 100 +
    input.map!(to!(char[])).array.transposed.map!(to!string).array.getHorizontalReflectionsScore(isHorizontalReflection);

int solve(string[] input, bool function(string[], int) isHorizontalReflection)
    =>
    input.split("").map!(mp => getScore(mp, isHorizontalReflection)).sum;

int solveEasy(string[] input) => solve(input, (input, x) => isHorizontalReflectionEasy(input, x));

bool isHorizontalReflectionHard(string[] input, int x) {
    auto pairedRows = input[x..$].zip(input[0..x].retro).array;

    int getDifferenceCount(string rw1, string rw2) => zip(rw1, rw2).count!(t => t[0] != t[1]);

    auto differences = pairedRows.map!(t => getDifferenceCount(t[0], t[1])).sum;

    return differences == 1;
}

int solveHard(string[] input) => input.solve((input, x) => isHorizontalReflectionHard(input, x));